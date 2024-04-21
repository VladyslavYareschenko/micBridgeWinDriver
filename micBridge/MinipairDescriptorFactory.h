#pragma once
#include "Globals.h"

#include "EndpointMinipair.h"


class MinipairDescriptorFactory
{
private:
	static int m_CurrentIndex;
	const static char m_StringCache[20];

	static ENDPOINT_MINIPAIR m_MicrophoneTemplate;
	static void SetLastCharacterOfString(PWSTR string, int valueToSet);

	MinipairDescriptorFactory();
	~MinipairDescriptorFactory();
public:
	static NTSTATUS CreateMicrophone(_Outptr_ ENDPOINT_MINIPAIR** pMinipair);
	
};

