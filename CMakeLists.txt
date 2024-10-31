cmake_minimum_required(VERSION 3.0)

project(micBridgeDriver)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/FindWDK/cmake")
find_package(WDK REQUIRED)

set(KDMF_VER 1.15)

wdk_add_driver(micBridgeDriver
               KMDF ${KDMF_VER}
               NTDDI_VERSION NTDDI_WIN10_RS2
               micBridge/TypeTraits.h
               micBridge/Adapter.cpp
               micBridge/Adapter.h
               micBridge/AdapterCommon.cpp
               micBridge/AdapterCommon.h
               micBridge/Atomic.h
               micBridge/Driver.cpp
               micBridge/Driver.h
               micBridge/EndpointMinipair.h
               micBridge/Globals.h
               micBridge/IAdapterCommon.h
               micBridge/InterlockedIntrinsic.h
               micBridge/KsAudioProcessingAttribute.h
               micBridge/KsHelper.cpp
               micBridge/KsHelper.h
               micBridge/LockFreeRingBuffer.h
               micBridge/Macros.h
               micBridge/MicrophoneTopologyPropertiesh.h
               micBridge/MicrophoneWaveProperties.h
               micBridge/MinipairDescriptorFactory.cpp
               micBridge/MinipairDescriptorFactory.h
               micBridge/MiniportTopology.cpp
               micBridge/MiniportTopology.h
               micBridge/MiniportWaveRT.cpp
               micBridge/MiniportWaveRT.h
               micBridge/MiniportWaveRTStream.cpp
               micBridge/MiniportWaveRTStream.h
               micBridge/MiniportWaveRTStreamAudioEngineNode.cpp
               micBridge/NewDelete.cpp
               micBridge/NewDelete.h
               micBridge/RegistryHelper.cpp
               micBridge/RegistryHelper.h
               micBridge/RingBuffer.cpp
               micBridge/RingBuffer.h
               micBridge/SubdeviceCache.cpp
               micBridge/SubdeviceCache.h
               micBridge/SubdeviceHelper.cpp
               micBridge/SubdeviceHelper.h)

set_property(TARGET micBridgeDriver PROPERTY OUTPUT_NAME "MicBridge")
set_property(TARGET micBridgeDriver PROPERTY CXX_STANDARD 17)
target_link_libraries(micBridgeDriver WDK::PORTCLS WDK::STDUNK)

include(Deployment.cmake)
