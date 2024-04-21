#pragma once
#include "Globals.h"

#include "Macros.h"
#include "IAdapterCommon.h"
#include "SubdeviceHelper.h"

class MiniportWaveRT;

class AdapterCommon : public IAdapterCommon, public CUnknown
{
public:
	static AdapterCommon& getInstance();

public:
	DECLARE_STD_UNKNOWN()
	AdapterCommon(PUNKNOWN unknown);
	~AdapterCommon();

	static NTSTATUS Create(
		_Out_       PUNKNOWN* Unknown,
		_In_        REFCLSID,
		_In_opt_    PUNKNOWN                UnknownOuter,
		_When_((PoolType& NonPagedPoolMustSucceed) != 0,
			__drv_reportError("Must succeed pool allocations are forbidden. "
				"Allocation failures cause a system crash"))
		_In_        POOL_TYPE               PoolType,
		_In_		PDEVICE_OBJECT			DeviceObject,
		_In_		PIRP StartupIrp
	);

	NTSTATUS __stdcall Init
	(
		_In_ IRP* StartupIrp,
		_In_ PDEVICE_OBJECT DeviceObject
	);

	void __stdcall Cleanup();

	NTSTATUS __stdcall WriteEtwEvent
	(
		_In_ EPcMiniportEngineEvent    miniportEventType,
		_In_ ULONGLONG      ullData1,
		_In_ ULONGLONG      ullData2,
		_In_ ULONGLONG      ullData3,
		_In_ ULONGLONG      ullData4
	);

	void __stdcall SetEtwHelper
	(
		PPORTCLSETWHELPER _pPortClsEtwHelper
	);

	STDMETHODIMP_(void) SetWaveServiceGroup
	(
		IN  PSERVICEGROUP   ServiceGroup
	);

	PDEVICE_OBJECT __stdcall GetDeviceObject(void);

	PDEVICE_OBJECT __stdcall GetPhysicalDeviceObject(void);

	MiniportWaveRT* getMicrophone();

private:
	NTSTATUS installVirtualMic(IRP* Irp);

private:
	static AdapterCommon* first_instance;
	static LONG m_Instances;
	SubdeviceHelper* m_pDeviceHelper;
	PPORTCLSETWHELPER m_pPortClsEtwHelper;

	MiniportWaveRT* m_pMicrophone;
	PDEVICE_OBJECT m_pDeviceObject;
	PDEVICE_OBJECT m_pPhysicalDeviceObject;

	PSERVICEGROUP m_pServiceGroupWave;
};

