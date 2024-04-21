#include "MinipairDescriptorFactory.h"
#include "MicrophoneWaveProperties.h"
#include "MicrophoneTopologyPropertiesh.h"

#include "MiniportTopology.h"

MinipairDescriptorFactory::MinipairDescriptorFactory()
{
}
MinipairDescriptorFactory::~MinipairDescriptorFactory()
{
}

void MinipairDescriptorFactory::SetLastCharacterOfString(PWSTR string, int valueToSet)
{
	UNREFERENCED_PARAMETER(string);
	UNREFERENCED_PARAMETER(valueToSet);
	//TODO: implement
	//string[wcslen(string) - 1] = ('0' + (valueToSet % 36));
}

int MinipairDescriptorFactory::m_CurrentIndex = 0;
ENDPOINT_MINIPAIR MinipairDescriptorFactory::m_MicrophoneTemplate =
{
	DeviceType::CaptureDevice,
	L"micBridgeTopologyCapture",                       // make sure this or the template name matches with KSNAME_TopologyMicIn in the inf's [Strings] section 
	L"TopologyCaptureTemplate",                                   // optional template name
	MiniportTopology::Create,
	&MicInTopoMiniportFilterDescriptor,
	0, NULL,                                // Interface properties
	L"micBridgeWaveCapture",                           // make sure this or the template name matches with KSNAME_WaveMicIn in the inf's [Strings] section
	L"WaveCaptureTemplate",                                   // optional template name
	MiniportWaveRT::Create,
	&MicInWaveMiniportFilterDescriptor,
	0, NULL,                                // Interface properties
	MICIN_DEVICE_MAX_CHANNELS,
	MicInPinDeviceFormatsAndModes,
	SIZEOF_ARRAY(MicInPinDeviceFormatsAndModes),
	MicInTopologyPhysicalConnections,
	SIZEOF_ARRAY(MicInTopologyPhysicalConnections),
	ENDPOINT_FLAG_NONE,
	NULL, 0, NULL,                          // audio module settings.
};

NTSTATUS MinipairDescriptorFactory::CreateMicrophone(_Outptr_ ENDPOINT_MINIPAIR** pMinipair)
{
	ENDPOINT_MINIPAIR* pNewMinipair = new(NonPagedPoolNx, MINIADAPTER_POOLTAG) ENDPOINT_MINIPAIR;
	if (!pNewMinipair) 
	{
		*pMinipair = NULL;
		return STATUS_INSUFFICIENT_RESOURCES;
	}
	*pNewMinipair = m_MicrophoneTemplate;

	MinipairDescriptorFactory::SetLastCharacterOfString(pNewMinipair->TopoName, m_CurrentIndex);
	MinipairDescriptorFactory::SetLastCharacterOfString(pNewMinipair->WaveName, m_CurrentIndex);
	//TODO: check if correct

	*pMinipair = pNewMinipair;
	m_CurrentIndex = m_CurrentIndex + 1;

	return STATUS_SUCCESS;
	/*
	ENDPOINT_MINIPAIR* pNewMinipair = (ENDPOINT_MINIPAIR*)ExAllocatePoolWithTag(NonPagedPoolNx, sizeof(ENDPOINT_MINIPAIR), MINIADAPTER_POOLTAG);
	RtlCopyMemory(pNewMinipair, &m_MicrophoneTemplate, sizeof(ENDPOINT_MINIPAIR));
	*/
}


