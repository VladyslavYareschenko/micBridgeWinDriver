#pragma once
#include "Globals.h"

class RingBuffer
{
public:
	RingBuffer();
	~RingBuffer();

	NTSTATUS Init(SIZE_T bufferSize);
	void Clear();

	NTSTATUS Put(_In_ BYTE* pBytes, _In_ SIZE_T count);
	NTSTATUS Take(_In_ BYTE* pTarget, _In_ SIZE_T count, _Outptr_opt_ SIZE_T* readCount);

	SIZE_T GetSize();
	SIZE_T GetAvailableBytes();

private:
	void clearImpl();

private:
	FAST_MUTEX m_BufferMutex;
	KSPIN_LOCK* m_BufferLock;
	KIRQL m_SpinLockIrql;
	BYTE* m_Buffer;
	SIZE_T m_BufferLength;
	BOOL m_IsFilling;
	BOOL m_isActive;

	SIZE_T m_autoFixCounter;

	ULONGLONG m_LinearBufferReadPosition;
	ULONGLONG m_LinearBufferWritePosition;

	ULONGLONG m_BufferNumWrite;

	KMUTEX bufferMutex;

private:  // Recover stream delay workaround entities.
	SIZE_T m_recoveryCounter = 0;
	bool isRecovering();
	void startRecoverProcess();
	void refreshRecoverIndicator();
};

