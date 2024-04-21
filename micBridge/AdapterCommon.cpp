#include "AdapterCommon.h"

#include "MinipairDescriptorFactory.h"
#include "MiniportWaveRT.h"
#include "SubdeviceHelper.h"

LONG AdapterCommon::m_Instances = 0;
AdapterCommon* AdapterCommon::first_instance = nullptr;
AdapterCommon& AdapterCommon::getInstance()
{
	return *first_instance;
}


AdapterCommon::AdapterCommon(PUNKNOWN unknown) 
	: CUnknown(unknown)
{
}

AdapterCommon::~AdapterCommon()
{
	PAGED_CODE();
	DPF_ENTER(("[AdapterCommon::~AdapterCommon]"));

	if (m_pDeviceHelper)
	{
		ExFreePoolWithTag(m_pDeviceHelper, MINIADAPTER_POOLTAG);
	}

	InterlockedDecrement(&AdapterCommon::m_Instances);
	ASSERT(AdapterCommon::m_Instances == 0);
}

NTSTATUS AdapterCommon::Create
(
	_Out_       PUNKNOWN *              Unknown,
	_In_        REFCLSID,
	_In_opt_    PUNKNOWN                UnknownOuter,
	_When_((PoolType & NonPagedPoolMustSucceed) != 0,
		__drv_reportError("Must succeed pool allocations are forbidden. "
			"Allocation failures cause a system crash"))
	_In_        POOL_TYPE               PoolType,
	_In_		PDEVICE_OBJECT DeviceObject,
	_In_		PIRP StartupIrp
)
{
	PAGED_CODE();
	DPF_ENTER(("[AdapterCommon::Create]"));
	ASSERT(Unknown);
	ASSERT(StartupIrp);

	NTSTATUS ntStatus;

	// This is a singleton, check before creating instance.
	if (InterlockedCompareExchange(&AdapterCommon::m_Instances, 1, 0) != 0)
	{
		ntStatus = STATUS_DEVICE_BUSY;
		DPF(D_TERSE, ("NewAdapterCommon failed, adapter already created."));
		goto Done;
	}

	AdapterCommon* p = new(PoolType, MINIADAPTER_POOLTAG) AdapterCommon(UnknownOuter);
	if (p == NULL)
	{
		ntStatus = STATUS_INSUFFICIENT_RESOURCES;
		DPF(D_TERSE, ("NewAdapterCommon failed, 0x%x", ntStatus));
		goto Done;
	}

	first_instance = p;

	ntStatus = p->Init(StartupIrp, DeviceObject);
	IF_FAILED_ACTION_RETURN(ntStatus, DPF(D_TERSE, ("Error initializing Adapter, 0x%x", ntStatus)));

	*Unknown = PUNKNOWN((IAdapterCommon*)(p));
	(*Unknown)->AddRef();
	ntStatus = STATUS_SUCCESS;

Done:
	return ntStatus;
}

NTSTATUS __stdcall AdapterCommon::Init(IRP* StartupIrp, PDEVICE_OBJECT DeviceObject)
{
	PAGED_CODE();
	DPF_ENTER(("[AdapterCommon::Init]"));
	ASSERT(DeviceObject);

	NTSTATUS        ntStatus = STATUS_SUCCESS;

	m_pDeviceObject = DeviceObject;
	m_pDeviceHelper = new(NonPagedPoolNx, MINIADAPTER_POOLTAG) SubdeviceHelper(this);
	if (!m_pDeviceHelper)
	{
		m_pDeviceHelper = NULL;
		ntStatus = STATUS_INSUFFICIENT_RESOURCES;
	}

	if (NT_SUCCESS(ntStatus))
	{
		ntStatus = PcGetPhysicalDeviceObject(DeviceObject, &m_pPhysicalDeviceObject);
		if (!NT_SUCCESS(ntStatus))
		{
			DPF(D_TERSE, ("PcGetPhysicalDeviceObject failed, 0x%x", ntStatus));
		}
	}

	ntStatus = installVirtualMic(StartupIrp);
	IF_FAILED_ACTION_RETURN(ntStatus, DPF(D_TERSE, ("InstallVirtualCable failed, 0x%x", ntStatus)));

	if (!NT_SUCCESS(ntStatus))
	{
		m_pDeviceObject = NULL;
		if (m_pDeviceHelper) ExFreePoolWithTag(m_pDeviceHelper, MINIADAPTER_POOLTAG);
		m_pPhysicalDeviceObject = NULL;
	}

	return ntStatus;
}

NTSTATUS AdapterCommon::installVirtualMic(IRP* Irp)
{
	PAGED_CODE();
	NTSTATUS ntStatus = STATUS_SUCCESS;

	ENDPOINT_MINIPAIR* pCaptureMiniport = NULL;
	ntStatus = MinipairDescriptorFactory::CreateMicrophone(&pCaptureMiniport);
	if (!NT_SUCCESS(ntStatus))
	{
		DPF(D_TERSE, ("InstallVirtualMic failed, 0x%x", ntStatus));
		return ntStatus;
	}

	IUnknown* unknownMic;
	m_pDeviceHelper->InstallMinipair(Irp, pCaptureMiniport, NULL, NULL, NULL, NULL, &unknownMic);

	MiniportWaveRT* microphone;
	ntStatus = unknownMic->QueryInterface(IID_MiniportWaveRT, (PVOID*)&microphone);
	if (NT_SUCCESS(ntStatus))
	{
		m_pMicrophone = microphone;
	}

	return ntStatus;
}

void __stdcall AdapterCommon::Cleanup()
{
	PAGED_CODE();
	DPF_ENTER(("[AdapterCommon::Cleanup]"));

	if (m_pDeviceHelper)
	{
		m_pDeviceHelper->Clean();
	}
}

STDMETHODIMP_(void) AdapterCommon::SetWaveServiceGroup(IN PSERVICEGROUP ServiceGroup )
{
	PAGED_CODE();

	DPF_ENTER(("[AdapterCommon::SetWaveServiceGroup]"));

	if (m_pServiceGroupWave)
	{
		m_pServiceGroupWave->Release();
	}

	m_pServiceGroupWave = ServiceGroup;

	if (m_pServiceGroupWave)
	{
		m_pServiceGroupWave->AddRef();
	}
}

STDMETHODIMP AdapterCommon::NonDelegatingQueryInterface
(
	_In_ REFIID                      Interface,
	_COM_Outptr_ PVOID *        Object
)
{
	PAGED_CODE();

	ASSERT(Object);

	if (IsEqualGUIDAligned(Interface, IID_IUnknown))
	{
		*Object = PVOID(PUNKNOWN((IAdapterCommon*)(this)));
	}
	else if (IsEqualGUIDAligned(Interface, IID_IAdapterCommon))
	{
		*Object = PVOID((IAdapterCommon*)(this));
	}
	else if (IsEqualGUIDAligned(Interface, IID_IAdapterPowerManagement))
	{
		*Object = PVOID(PADAPTERPOWERMANAGEMENT(this));
	}
	else
	{
		*Object = NULL;
	}

	if (*Object)
	{
		PUNKNOWN(*Object)->AddRef();
		return STATUS_SUCCESS;
	}

	return STATUS_INVALID_PARAMETER;
}

PDEVICE_OBJECT __stdcall AdapterCommon::GetDeviceObject(void)
{
	return m_pDeviceObject;
}

PDEVICE_OBJECT __stdcall AdapterCommon::GetPhysicalDeviceObject(void)
{
	return m_pPhysicalDeviceObject;
}

#pragma code_seg()
NTSTATUS __stdcall AdapterCommon::WriteEtwEvent(EPcMiniportEngineEvent miniportEventType, ULONGLONG ullData1, ULONGLONG ullData2, ULONGLONG ullData3, ULONGLONG ullData4)
{
	NTSTATUS ntStatus = STATUS_SUCCESS;

	if (m_pPortClsEtwHelper)
	{
		ntStatus = m_pPortClsEtwHelper->MiniportWriteEtwEvent(miniportEventType, ullData1, ullData2, ullData3, ullData4);
	}
	return ntStatus;
}

#pragma code_seg("PAGE")
void __stdcall AdapterCommon::SetEtwHelper(PPORTCLSETWHELPER _pPortClsEtwHelper)
{
	PAGED_CODE();

	SAFE_RELEASE(m_pPortClsEtwHelper);

	m_pPortClsEtwHelper = _pPortClsEtwHelper;

	if (m_pPortClsEtwHelper)
	{
		m_pPortClsEtwHelper->AddRef();
	}
}

MiniportWaveRT* AdapterCommon::getMicrophone()
{
	return m_pMicrophone;
}
