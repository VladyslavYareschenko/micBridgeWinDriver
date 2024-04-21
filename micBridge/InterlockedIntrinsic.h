#pragma once

extern "C" unsigned char _addcarry_u32(unsigned char c_in,
    unsigned int src1,
    unsigned int src2,
    unsigned int* sum_out);
#pragma intrinsic(_addcarry_u32)

extern "C" unsigned char _addcarry_u64(unsigned char c_in,
    unsigned __int64 src1,
    unsigned __int64 src2,
    unsigned __int64* sum_out);
#pragma intrinsic(_addcarry_u64)

extern "C" unsigned char _BitScanForward(unsigned long* index,
    unsigned long mask);
#pragma intrinsic(_BitScanForward)

extern "C" unsigned char _BitScanForward64(unsigned long* index,
    unsigned long long mask);
#pragma intrinsic(_BitScanForward64)

extern "C" unsigned char _BitScanReverse(unsigned long* index,
    unsigned long mask);
#pragma intrinsic(_BitScanReverse)

extern "C" unsigned char _BitScanReverse64(unsigned long* index,
    unsigned long long mask);
#pragma intrinsic(_BitScanReverse64)

extern "C" char _InterlockedExchange8(volatile char* place, char new_value);
#pragma intrinsic(_InterlockedExchange8)
#ifndef InterlockedExchange8
#define InterlockedExchange8 _InterlockedExchange8
#endif

extern "C" char _InterlockedCompareExchange8(volatile char* place,
    char new_value,
    char expected);
#pragma intrinsic(_InterlockedCompareExchange8)
#ifndef InterlockedCompareExchange8
#define InterlockedCompareExchange8 _InterlockedCompareExchange8
#endif

extern "C" char _InterlockedExchangeAdd8(volatile char* place, char append);
#pragma intrinsic(_InterlockedExchangeAdd8)
#ifndef InterlockedExchangeAdd8
#define InterlockedExchangeAdd8 _InterlockedExchangeAdd8
#endif

extern "C" inline char _InterlockedIncrement8(volatile char* target) {
    return InterlockedExchangeAdd8(target, 1);
}

#ifndef InterlockedIncrement8
#define InterlockedIncrement8 _InterlockedIncrement8
#endif

extern "C" inline char _InterlockedDecrement8(volatile char* target) {
    return InterlockedExchangeAdd8(target, -1);
}

#ifndef InterlockedDecrement8
#define InterlockedDecrement8 _InterlockedDecrement8
#endif

extern "C" inline char _InterlockedAdd8(volatile char* target, char value) {
    return InterlockedExchangeAdd8(target, value);
}

#ifndef InterlockedAdd8
#define InterlockedAdd8 _InterlockedAdd8
#endif

extern "C" inline char _InterlockedAdd16(volatile short* target, short value);

#ifndef InterlockedAdd16
#define InterlockedAdd16 _InterlockedAdd16
#endif

#if defined(_M_IX86)
#define CRTCALL __cdecl
#define STDCALL __stdcall
#define FASTCALL __fastcall
#else
#define CRTCALL
#define STDCALL
#define FASTCALL
#endif

extern "C" void* CRTCALL memcpy(void* dst, const void* src, size_t size);
#pragma intrinsic(memcpy)

extern "C" void* CRTCALL memset(void* lhs, int pattern, size_t size);
#pragma intrinsic(memset)

extern "C" int CRTCALL memcmp(const void* lhs, const void* rhs, size_t size);
#pragma intrinsic(memcmp)