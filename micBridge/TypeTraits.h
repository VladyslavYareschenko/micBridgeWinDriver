﻿#pragma once

#include <cstddef>

#if defined(_M_IX86)
#define CRTCALL __cdecl
#define STDCALL __stdcall
#define FASTCALL __fastcall
#else
#define CRTCALL
#define STDCALL
#define FASTCALL
#endif

#ifndef VECTORCALL
#define VECTORCALL __vectorcall
#endif

#ifndef THISCALL
#define THISCALL __thiscall
#endif

#define EXTERN_C extern "C"

#define ALIGN(align_val) __declspec(align(align_val))
#define CRTALLOC(x) __declspec(allocate(x))
#define NOINLINE __declspec(noinline)

#define CONCAT_IMPL(x, y) x##y
#define CONCAT(x, y) CONCAT_IMPL(x, y)

#define STRINGIFY_IMPL(expr) #expr
#define STRINGIFY(expr) STRINGIFY_IMPL(expr)

#define IN_FILE_ON_LINE \
  CONCAT(CONCAT("in file ", __FILE__), CONCAT(" on line ", STRINGIFY(__LINE__)))

#define container_of(ptr, type, member) \
  reinterpret_cast<type*>((uintptr_t)(ptr)-offsetof(type, member))

using int8_t = signed char;
using int16_t = short;
using int32_t = int;
using int64_t = long long;
using uint8_t = unsigned char;
using uint16_t = unsigned short;
using uint32_t = unsigned int;
using uint64_t = unsigned long long;

using int_least8_t = signed char;
using int_least16_t = short;
using int_least32_t = int;
using int_least64_t = long long;
using uint_least8_t = unsigned char;
using uint_least16_t = unsigned short;
using uint_least32_t = unsigned int;
using uint_least64_t = unsigned long long;

using int_fast8_t = signed char;
using int_fast16_t = int;
using int_fast32_t = int;
using int_fast64_t = long long;
using uint_fast8_t = unsigned char;
using uint_fast16_t = unsigned int;  // As if in MSVC headers
using uint_fast32_t = unsigned int;
using uint_fast64_t = unsigned long long;

using intmax_t = long long;
using uintmax_t = unsigned long long;
using size_t = decltype(sizeof(int));
using nullptr_t = decltype(nullptr);

#ifdef _M_AMD64
#define BITNESS 64  // NOLINT(cppcoreguidelines-macro-usage)
using ptrdiff_t = __int64;  // NOLINT(clang-diagnostic-language-extension-token)
using intptr_t = __int64;   // NOLINT(clang-diagnostic-language-extension-token)
using uintptr_t =
unsigned __int64;  // NOLINT(clang-diagnostic-language-extension-token)
#elif defined(_M_IX86)  // NOLINT(cppcoreguidelines-macro-usage)
#define BITNESS 32
using ptrdiff_t = int;
using intptr_t = int;
using uintptr_t = unsigned int;
#else
#error Unsupported platform
#endif

struct nothrow_t {};
inline constexpr nothrow_t nothrow;

using max_align_t = double;  // Most aligned type

#define INT8_C(x) (x)  // NOLINT(cppcoreguidelines-macro-usage)
#define INT16_C(x) (x)  // NOLINT(cppcoreguidelines-macro-usage)
#define INT32_C(x) (x)  // NOLINT(cppcoreguidelines-macro-usage)
#define INT64_C(x) (x##LL)  // NOLINT(cppcoreguidelines-macro-usage)
#define UINT8_C(x) (x)  // NOLINT(cppcoreguidelines-macro-usage)
#define UINT16_C(x) (x)  // NOLINT(cppcoreguidelines-macro-usage)
#define UINT32_C(x) (x##U)  // NOLINT(cppcoreguidelines-macro-usage)
#define UINT64_C(x) (x##ULL)  // NOLINT(cppcoreguidelines-macro-usage)

#define INTMAX_C(x) INT64_C(x)  // NOLINT(cppcoreguidelines-macro-usage)
#define UINTMAX_C(x) UINT64_C(x)  // NOLINT(cppcoreguidelines-macro-usage)


namespace kmdf_std
{

    template <class... Dummy>
    struct always_false {
        static constexpr bool value = false;
    };

    template <class... Dummy>
    inline constexpr bool always_false_v = always_false<Dummy...>::value;

    template <class...>
    using void_t = void;

    template <class Ty, Ty Value>
    struct integral_constant {
        static constexpr Ty value = Value;

        using value_type = Ty;
        using type = integral_constant;

        constexpr operator value_type() const noexcept { return value; }

        [[nodiscard]] constexpr value_type operator()() const noexcept {
            return value;
        }
    };

    template <bool Value>
    using bool_constant = integral_constant<bool, Value>;

    using false_type = bool_constant<false>;
    using true_type = bool_constant<true>;

    template <bool enable, class Ty = void>
    struct enable_if {};

    template <class Ty>
    struct enable_if<true, Ty> {
        using type = Ty;
    };

    template <bool enable, class Ty = void>
    using enable_if_t = typename enable_if<enable, Ty>::type;

    template <bool flag, class Ty1, class Ty2>
    struct conditional {
        using type = Ty1;
    };

    template <class Ty1, class Ty2>
    struct conditional<false, Ty1, Ty2> {
        using type = Ty2;
    };

    template <bool flag, class Ty1, class Ty2>
    using conditional_t = typename conditional<flag, Ty1, Ty2>::type;

    template <class Ty>
    struct remove_reference {
        using type = Ty;
    };

    template <class Ty>
    struct remove_reference<Ty&> {
        using type = Ty;
    };

    template <class Ty>
    struct remove_reference<Ty&&> {
        using type = Ty;
    };

    template <class Ty>
    using remove_reference_t = typename remove_reference<Ty>::type;

    template <class Ty>
    struct remove_pointer {
        using type = Ty;
    };

    template <class Ty>
    struct remove_pointer<Ty*> {
        using type = Ty;
    };

    template <class Ty>
    using remove_pointer_t = typename remove_pointer<Ty>::type;

    template <class Ty, class = void>
    struct add_reference {
        using lvalue = Ty;  //Для void
        using rvalue = Ty;
    };

    template <class Ty>
    struct add_reference<Ty,
        void_t<Ty&>> {  //Для типов, на которые допустимы ссылки
        using lvalue = Ty&;
        using rvalue = Ty&&;
    };

    template <class Ty>
    struct add_lvalue_reference {
        using type = typename add_reference<Ty>::lvalue;
    };

    template <class Ty>
    using add_lvalue_reference_t = typename add_lvalue_reference<Ty>::type;

    template <class Ty>
    struct add_rvalue_reference {
        using type = typename add_reference<Ty>::rvalue;
    };

    template <class Ty>
    using add_rvalue_reference_t = typename add_rvalue_reference<Ty>::type;

    template <class Ty>
    struct add_pointer {
        using type = Ty*;
    };

    template <class Ty>
    using add_pointer_t = typename add_pointer<Ty>::type;

    template <class Ty>
    struct remove_const {
        using type = Ty;
    };

    template <class Ty>
    struct remove_const<const Ty> {
        using type = Ty;
    };

    template <class Ty>
    using remove_const_t = typename remove_const<Ty>::type;

    template <class Ty>
    struct remove_volatile {
        using type = Ty;
    };

    template <class Ty>
    struct remove_volatile<volatile Ty> {
        using type = Ty;
    };

    template <class Ty>
    using remove_volatile_t = typename remove_volatile<Ty>::type;

    template <class Ty>
    struct remove_cv {
        using type = remove_volatile_t<remove_const_t<Ty>>;
    };

    template <class Ty>
    using remove_cv_t = typename remove_cv<Ty>::type;

    template <class Ty>
    struct remove_cvref {
        using type = typename remove_cv_t<remove_reference_t<Ty>>;
    };

    template <typename Ty>
    using remove_cvref_t = typename remove_cvref<Ty>::type;

    template <class Ty>
    struct remove_pointer_const {
        using type = Ty;
    };

    template <class Ty>
    struct remove_pointer_const<Ty*> {
        using type = Ty*;
    };

    template <class Ty>
    struct remove_pointer_const<const Ty*> {
        using type = Ty*;
    };

    template <class Ty>
    using remove_pointer_const_t = typename remove_pointer_const<Ty>::type;

    template <class Ty>
    struct add_const {
        using type = const Ty;
    };

    template <class Ty>
    struct add_const<const Ty> {
        using type = const Ty;
    };

    template <class Ty>
    using add_const_t = typename add_const<Ty>::type;

    template <class Ty>
    add_rvalue_reference_t<Ty> declval() noexcept;  //Только для SFINAE -
    //определение не требуется

    template <class Ty>
    struct is_empty {
        static constexpr bool value = __is_empty(Ty);
    };

    template <class Ty>
    inline constexpr bool is_empty_v = is_empty<Ty>::value;

    template <class Ty>
    struct is_final {
        static constexpr bool value = __is_final(Ty);
    };

    template <class Ty>
    inline constexpr bool is_final_v = is_final<Ty>::value;

    template <class From, class To, class = void>
    struct is_convertible : false_type {};

    template <class From, class To>
    struct is_convertible<From,
        To,
        void_t<decltype(static_cast<To>(declval<From>()))>>
        : true_type {};

    template <class From, class To>
    inline constexpr bool is_convertible_v = is_convertible<From, To>::value;

    template <class From, class To>
    struct is_nothrow_convertible {
        static constexpr bool value =
            is_convertible_v<From, To> && noexcept(static_cast<To>(declval<From>()));
    };

    template <class From, class To>
    inline constexpr bool is_nothrow_convertible_v =
        is_nothrow_convertible<From, To>::value;

    namespace tt::details {
        template <class, class Ty, class... Types>
        struct is_constructible_impl : false_type {};

        template <class Ty, class... Types>
        struct is_constructible_impl<void_t<decltype(Ty(declval<Types>()...))>,
            Ty,
            Types...> : true_type {};
    }  // namespace tt::details

    template <class Ty, class... Types>
    struct is_constructible
        : tt::details::is_constructible_impl<void_t<>, Ty, Types...> {};

    template <class Ty, class... Types>
    inline constexpr bool is_constructible_v =
        is_constructible<Ty, Types...>::value;

    template <class Ty, class... Types>
    struct is_trivially_constructible {
        static constexpr bool value = __is_trivially_constructible(Ty, Types...);
    };

    template <class Ty, class... Types>
    inline constexpr bool is_trivially_constructible_v =
        is_trivially_constructible<Ty, Types...>::value;

    template <class Ty>
    struct is_default_constructible {
        static constexpr bool value = is_constructible_v<Ty>;
    };

    template <class Ty>
    inline constexpr bool is_default_constructible_v =
        is_default_constructible<Ty>::value;

    template <class Ty>
    struct is_trivially_default_constructible {
        static constexpr bool value = __is_trivially_constructible(Ty);
    };

    template <class Ty>
    inline constexpr bool is_trivially_default_constructible_v =
        is_trivially_default_constructible<Ty>::value;

    template <class Ty>
    struct is_nothrow_default_constructible {
        static constexpr bool value = is_constructible_v<Ty> && noexcept(Ty());
    };

    template <class Ty>
    inline constexpr bool is_nothrow_default_constructible_v =
        is_default_constructible<Ty>::value;

    template <class Ty>
    struct is_copy_constructible {
        static constexpr bool value =
            is_constructible_v<Ty, add_const_t<add_lvalue_reference_t<Ty>>>;
    };

    template <class Ty>
    inline constexpr bool is_copy_constructible_v =
        is_copy_constructible<Ty>::value;

    template <class Ty>
    struct is_move_constructible {
        static constexpr bool value =
            is_constructible_v<Ty, add_rvalue_reference_t<Ty>>;
    };

    template <class Ty>
    inline constexpr bool is_move_constructible_v =
        is_move_constructible<Ty>::value;

    namespace tt::details {
        template <class, class Ty, class... Types>
        struct is_nothrow_constructible_impl : false_type {};

        template <class Ty, class... Types>
        struct is_nothrow_constructible_impl<
            void_t<enable_if_t<is_constructible_v<Ty, Types...>>>,
            Ty,
            Types...> : bool_constant<noexcept(Ty(declval<Types>()...))> {};
    }  // namespace tt::details

    template <class Ty, class... Types>
    inline constexpr bool is_nothrow_constructible_v =
        tt::details::is_nothrow_constructible_impl<void_t<>, Ty, Types...>::value;

    template <class Ty, class... Types>
    struct is_nothrow_constructible
        : bool_constant<is_nothrow_constructible_v<Ty, Types...>> {};

    template <class Ty>
    inline constexpr bool is_nothrow_copy_constructible_v =
        is_nothrow_constructible_v<Ty, add_const_t<add_lvalue_reference_t<Ty>>>;

    template <class Ty>
    struct is_nothrow_copy_constructible
        : bool_constant<is_nothrow_copy_constructible_v<Ty>> {};

    template <class Ty>
    inline constexpr bool is_nothrow_move_constructible_v =
        is_nothrow_constructible_v<Ty, add_rvalue_reference_t<Ty>>;

    template <class Ty>
    struct is_nothrow_move_constructible
        : bool_constant<is_nothrow_move_constructible_v<Ty>> {};

    template <class To, class From, class = void>
    struct is_assignable : false_type {};

    template <class To, class From>
    struct is_assignable<To,
        From,
        void_t<decltype(declval<To>() = declval<From>())>>
        : true_type {};

    template <class To, class From>
    inline constexpr bool is_assignable_v = is_assignable<To, From>::value;

    template <class Ty>
    inline constexpr bool is_copy_assignable_v =
        is_assignable_v<add_lvalue_reference_t<Ty>,
        add_const_t<add_lvalue_reference_t<Ty>>>;

    template <class Ty>
    struct is_copy_assignable : bool_constant<is_copy_assignable_v<Ty>> {};

    template <class Ty>
    inline constexpr bool is_move_assignable_v =
        is_assignable_v<add_lvalue_reference_t<Ty>, add_rvalue_reference_t<Ty>>;

    template <class Ty>
    struct is_move_assignable : bool_constant<is_move_assignable_v<Ty>> {};

    namespace tt::details {
        template <class, class To, class From>
        struct is_nothrow_assignable_impl : false_type {};

        template <class To, class From>
        struct is_nothrow_assignable_impl<
            void_t<enable_if_t<is_assignable_v<To, From>>>,
            To,
            From> : bool_constant<noexcept(declval<To>() = declval<From>())> {};
    }  // namespace tt::details

    template <class To, class From>
    inline constexpr bool is_nothrow_assignable_v =
        tt::details::is_nothrow_assignable_impl<void_t<>, To, From>::value;

    template <class To, class From>
    struct is_nothrow_assignable
        : bool_constant<is_nothrow_assignable_v<To, From>> {};

    template <class Ty>
    inline constexpr bool is_nothrow_copy_assignable_v =
        is_nothrow_assignable_v<add_lvalue_reference_t<Ty>,
        add_const_t<add_lvalue_reference_t<Ty>>>;

    template <class Ty>
    struct is_nothrow_copy_assignable
        : bool_constant<is_nothrow_copy_assignable_v<Ty>> {};

    template <class Ty>
    inline constexpr bool is_nothrow_move_assignable_v =
        is_nothrow_assignable_v<add_lvalue_reference_t<Ty>,
        add_rvalue_reference_t<Ty>>;

    template <class Ty>
    struct is_nothrow_move_assignable
        : bool_constant<is_nothrow_move_assignable_v<Ty>> {};

    template <class To, class From>
    struct is_trivially_assignable {
        static constexpr bool value = __is_trivially_assignable(To, From);
    };

    template <class To, class From>
    inline constexpr bool is_trivially_assignable_v =
        is_trivially_assignable<To, From>::value;

    template <class Ty>
    struct is_swappable {
        static constexpr bool value =
            is_move_constructible_v<Ty> && is_move_assignable_v<Ty>;
    };

    template <class Ty>
    inline constexpr bool is_swappable_v = is_swappable<Ty>::value;

    template <class Ty>
    struct is_nothrow_swappable {
        static constexpr bool value =
            is_nothrow_move_constructible_v<Ty> && is_nothrow_move_assignable_v<Ty>;
    };

    template <class Ty>
    inline constexpr bool is_nothrow_swappable_v = is_nothrow_swappable<Ty>::value;

    template <class Ty>
    struct is_pointer : false_type {};

    template <class Ty>
    struct is_pointer<Ty*> : true_type {};

    template <class Ty>
    inline constexpr bool is_pointer_v = is_pointer<Ty>::value;

    template <class Ty>
    struct is_null_pointer : false_type {};

    template <>
    struct is_null_pointer<nullptr_t> : true_type {};

    template <class Ty>
    inline constexpr bool is_null_pointer_v = is_null_pointer<Ty>::value;

    template <class Ty>
    struct is_lvalue_reference : false_type {};

    template <class Ty>
    struct is_lvalue_reference<Ty&> : true_type {};

    template <class Ty>
    struct is_lvalue_reference<Ty&&> : false_type {};

    template <class Ty>
    inline constexpr bool is_lvalue_reference_v = is_lvalue_reference<Ty>::value;

    template <class Ty>
    struct is_rvalue_reference : false_type {};

    template <class Ty>
    struct is_rvalue_reference<Ty&> : false_type {};

    template <class Ty>
    struct is_rvalue_reference<Ty&&> : true_type {};

    template <class Ty>
    inline constexpr bool is_rvalue_reference_v = is_rvalue_reference<Ty>::value;

    template <class Ty>
    struct is_reference {
        static constexpr bool value =
            is_lvalue_reference_v<Ty> || is_rvalue_reference_v<Ty>;
    };
    template <class Ty>
    inline constexpr bool is_reference_v = is_reference<Ty>::value;

    template <class Ty1, class Ty2>
    struct is_same : false_type {};

    template <class Ty>
    struct is_same<Ty, Ty> : true_type {};

    template <class Ty1, class Ty2>
    inline constexpr bool is_same_v = is_same<Ty1, Ty2>::value;

    template <class Ty>
    struct is_void {
        static constexpr bool value = is_same_v<remove_cv_t<Ty>, void>;
    };

    template <class Ty>
    inline constexpr bool is_void_v = is_void<Ty>::value;

    template <class Ty>
    struct is_const : false_type {};

    template <class Ty>
    struct is_const<const Ty> : true_type {};

    template <class Ty>
    inline constexpr bool is_const_v = is_const<Ty>::value;

    template <class Ty>
    struct is_volatile : false_type {};

    template <class Ty>
    struct is_volatile<volatile Ty> : true_type {};

    template <class Ty>
    inline constexpr bool is_volatile_v = is_volatile<Ty>::value;

    template <class>
    inline constexpr bool is_array_v = false;

    template <class Ty, size_t N>
    inline constexpr bool is_array_v<Ty[N]> = true;

    template <class Ty>
    inline constexpr bool is_array_v<Ty[]> = true;

    template <class Ty>
    struct is_array : bool_constant<is_array_v<Ty>> {};

    template <class Ty>
    inline constexpr bool is_class_v = __is_class(Ty);

    template <class Ty>
    struct is_class : bool_constant<is_class_v<Ty>> {};

    // TODO: use common macro for disabled warnings
#pragma warning(disable : 4180)  // qualifier applied to function type has no
                                 // meaning; ignored

    template <class Ty>
    inline constexpr bool is_function_v =
        !is_const_v<const Ty> &&
        !is_reference_v<Ty>;  // only function types and reference types can't be
                              // const qualified

    template <class Ty>
    struct is_function : bool_constant<is_function_v<Ty>> {};

#pragma warning(default : 4180)

    template <class Ty>
    struct is_object {
        static constexpr bool value =
            is_const_v<const Ty> &&
            !is_void_v<Ty>;  // only function types and reference
                             // types can't be const qualified
    };

    template <class Ty>
    inline constexpr bool is_object_v = is_object<Ty>::value;

    template <class Ty>
    struct is_trivially_copyable {
        static constexpr bool value = __is_trivially_copyable(Ty);
    };

    template <class Ty>
    inline constexpr bool is_trivially_copyable_v =
        is_trivially_copyable<Ty>::value;

    template <class Ty>
    struct is_trivially_destructible {
        static constexpr bool value = __is_trivially_destructible(Ty);
    };

    template <class Ty>
    inline constexpr bool is_trivially_destructible_v =
        is_trivially_copyable<Ty>::value;

    template <class Ty>
    struct is_trivial {
        static constexpr bool value =
            is_trivially_default_constructible_v<Ty> && is_trivially_copyable_v<Ty>;
    };

    template <class Ty>
    inline constexpr bool is_trivial_v = is_trivial<Ty>::value;

    template <class Ty>
    inline constexpr size_t alignment_of_v = alignof(Ty);

    namespace tt::details {
        template <class Ty, size_t Size>
        union align_type {  // union with size Size bytes and alignment of _Ty
            Ty value;
            char pad[Size];
        };

        template <size_t Size, size_t Align, class Ty, bool Ok>
        struct aligned_storage_impl;  // define type with size Size and alignment Ty

        template <size_t Size, size_t Align, class Ty>
        struct aligned_storage_impl<Size, Align, Ty, true> {
            using type = align_type<Ty, Size>;
        };

        template <size_t Size, size_t Align>
        struct aligned_storage_impl<Size, Align, double, false> {
#ifdef KTL_ENABLE_EXTENDED_ALIGNED_STORAGE
            struct type {
                alignas(Align) char space[Size];
            };
#else  // ^^^ _ENABLE_EXTENDED_ALIGNED_STORAGE /
            // !_ENABLE_EXTENDED_ALIGNED_STORAGE vvv
#ifndef KTL_DISABLE_EXTENDED_ALIGNED_STORAGE
            static_assert(
                always_false_v<aligned_storage_impl>,
                "Before VS 2017 15.8, alignment > alignof(max_align_t) doesn't allowed. "
                "2017 15.8 was fixed to handle this correctly, but the fix inherently "
                "changes layout and breaks "
                "binary compatibility."
                "Please define either"
                "(1) KTL_ENABLE_EXTENDED_ALIGNED_STORAGE to acknowledge that you "
                "understand "
                "this message and "
                "that you actually want a type with an extended alignment, or "
                "(2) KTL_DISABLE_EXTENDED_ALIGNED_STORAGE to silence this message and "
                "get "
                "the old non-conforming "
                "behavior.");
#endif  // !DISABLE_EXTENDED_ALIGNED_STORAGE
            using type = align_type<max_align_t, Size>;
#endif  // ENABLE_EXTENDED_ALIGNED_STORAGE
        };

        template <size_t Size, size_t Align>
        struct aligned_storage_impl<Size, Align, int, false> {
            using next_type = double;
            using type = typename aligned_storage_impl<Size,
                Align,
                next_type,
                Align <= alignof(next_type)>::type;
        };

        template <size_t Size, size_t Align>
        struct aligned_storage_impl<Size, Align, short, false> {
            using next_type = int;
            using type = typename aligned_storage_impl<Size,
                Align,
                next_type,
                Align <= alignof(next_type)>::type;
        };

        template <size_t Size, size_t Align>
        struct aligned_storage_impl<Size, Align, char, false> {
            using next_type = short;
            using type = typename aligned_storage_impl<Size,
                Align,
                next_type,
                Align <= alignof(next_type)>::type;
        };
    }  // namespace tt::details

    template <size_t Size, size_t Align = alignof(max_align_t)>
    struct aligned_storage {
        using next_type = char;
        using type =
            typename tt::details::aligned_storage_impl<Size,
            Align,
            next_type,
            Align <=
            alignof(next_type)>::type;
    };

    template <size_t Size, size_t Align = alignof(max_align_t)>
    using aligned_storage_t = typename aligned_storage<Size, Align>::type;

    template <class Ty>
    struct is_enum {
        static constexpr bool value = __is_enum(Ty);
    };

    template <class Ty>
    inline constexpr bool is_enum_v = is_enum<Ty>::value;

    namespace tt::details {
        template <class Ty, bool = is_enum_v<Ty>>
        struct underlying_type_impl {
            using type = __underlying_type(Ty);
        };

        template <class Ty>
        struct underlying_type_impl<Ty, false> {};
    }  // namespace tt::details

    template <class Ty>
    struct underlying_type : tt::details::underlying_type_impl<Ty> {
    };  // determine underlying type for enum

    template <class Ty>
    using underlying_type_t = typename underlying_type<Ty>::type;

    template <class Ty>
    struct remove_extent {
        using type = Ty;
    };

    template <class Ty, size_t N>
    struct remove_extent<Ty[N]> {
        using type = Ty;
    };

    template <class Ty>
    struct remove_extent<Ty[]> {
        using type = Ty;
    };

    template <class Ty>
    using remove_extent_t = typename remove_extent<Ty>::type;

    template <class Ty>
    struct remove_all_extents {
        using type = Ty;
    };

    template <class Ty, size_t N>
    struct remove_all_extents<Ty[N]> {
        using type = typename remove_all_extents<Ty>::type;
    };

    template <class Ty>
    struct remove_all_extents<Ty[]> {
        using type = typename remove_all_extents<Ty>::type;
    };

    template <class Ty>
    using remove_all_extents_t = typename remove_all_extents<Ty>::type;

    template <class Ty>
    struct decay {
    private:
        using unref_t = remove_reference_t<Ty>;

    public:
        using type = conditional_t<is_array_v<unref_t>,
            add_pointer_t<remove_extent_t<unref_t>>,
            conditional_t<is_function_v<unref_t>,
            add_pointer_t<unref_t>,
            remove_cv_t<unref_t>>>;
    };

    template <class Ty>
    using decay_t = typename decay<Ty>::type;

    namespace tt::details {
        template <class... Types>
        struct common_type_impl {};

        template <class Ty>
        struct common_type_impl<Ty> {
            using type = Ty;
        };

        template <class Ty1, class Ty2>
        struct common_type_impl<Ty1, Ty2> {
            using type = decay_t<decltype(true ? declval<Ty1>() : declval<Ty2>())>;
        };

        template <class Ty1, class Ty2, class... Rest>
        struct common_type_impl<Ty1, Ty2, Rest...> {
            using type =
                typename common_type_impl<typename common_type_impl<Ty1, Ty2>::type,
                Rest...>::type;
        };
    }  // namespace tt::details

    template <class... Types>
    struct common_type {
        using type = typename tt::details::common_type_impl<Types...>::type;
    };

    template <class... Types>
    using common_type_t = typename common_type<Types...>::type;

    template <class Ty, class ListHead, class... Typelist>
    struct is_in_typelist {
        static constexpr bool value =
            is_same_v<Ty, ListHead> || is_in_typelist<Ty, Typelist...>::value;
    };

    template <class Ty, class ListTail>
    struct is_in_typelist<Ty, ListTail> {
        static constexpr bool value = is_same_v<Ty, ListTail>;
    };

    template <class Ty, class... Types>
    inline constexpr bool is_in_typelist_v = is_in_typelist<Ty, Types...>::value;

    template <class Ty>
    struct is_integral {
        static constexpr bool value = is_in_typelist_v<remove_cv_t<Ty>,
            bool,
            char,
            signed char,
            unsigned char,
            wchar_t,
            char16_t,
            char32_t,
            short,
            unsigned short,
            int,
            unsigned int,
            long,
            unsigned long,
            long long,
            unsigned long long>;
    };

    template <class Ty>
    inline constexpr bool is_integral_v = is_integral<Ty>::value;

    template <class Ty>
    struct is_floating_point {
        static constexpr bool value =
            is_in_typelist_v<remove_cv_t<Ty>, float, double, long double>;
    };

    template <class Ty>
    inline constexpr bool is_floating_point_v = is_floating_point<Ty>::value;

    template <class Ty>
    struct is_arithmetic {
        static constexpr bool value = is_integral_v<Ty> || is_floating_point_v<Ty>;
    };

    template <class Ty>
    inline constexpr bool is_arithmetic_v = is_arithmetic<Ty>::value;

    namespace tt::details {
        template <class>
        struct is_member_object_pointer_impl {
            static constexpr bool value = false;
        };

        template <class Ty, class ClassTy>
        struct is_member_object_pointer_impl<Ty ClassTy::*> {
            static constexpr bool value = !is_function_v<Ty>;
        };

        template <class Ty, class ClassTy>
        struct is_member_object_pointer_impl<Ty ClassTy::* const> {
            static constexpr bool value = !is_function_v<Ty>;
        };

        template <class Ty, class ClassTy>
        struct is_member_object_pointer_impl<Ty ClassTy::* volatile> {
            static constexpr bool value = !is_function_v<Ty>;
        };

        template <class Ty, class ClassTy>
        struct is_member_object_pointer_impl<Ty ClassTy::* const volatile> {
            static constexpr bool value = !is_function_v<Ty>;
        };
    }  // namespace tt::details

    template <class Ty>
    struct is_member_object_pointer {
        static constexpr bool value =
            tt::details::is_member_object_pointer_impl<remove_cv_t<Ty>>::value;
    };

    template <class Ty>
    inline constexpr bool is_member_object_pointer_v =
        is_member_object_pointer<Ty>::value;

    template <class Ty>
    struct is_member_function_pointer : public false_type {};

    template <class Ty>
    struct is_member_function_pointer<const Ty>
        : public is_member_function_pointer<Ty> {};

    template <class Ty>
    struct is_member_function_pointer<volatile Ty>
        : public is_member_function_pointer<Ty> {};

    template <class Ty>
    struct is_member_function_pointer<const volatile Ty>
        : public is_member_function_pointer<Ty> {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(THISCALL ClassTy::*)(Types...)>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(ClassTy::*)(Types..., ...)>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(THISCALL ClassTy::*)(Types...) const>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(ClassTy::*)(Types..., ...) const>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(THISCALL ClassTy::*)(Types...) volatile>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(ClassTy::*)(Types..., ...) volatile>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(THISCALL ClassTy::*)(Types...)
        const volatile> : public true_type {};
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(ClassTy::*)(Types..., ...)
        const volatile> : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(THISCALL ClassTy::*)(Types...)&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(ClassTy::*)(Types..., ...)&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(THISCALL ClassTy::*)(Types...) const&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(ClassTy::*)(Types..., ...) const&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(THISCALL ClassTy::*)(Types...) volatile&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(ClassTy::*)(Types..., ...) volatile&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(THISCALL ClassTy::*)(Types...)
        const volatile&> : public true_type {};
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(ClassTy::*)(Types..., ...)
        const volatile&> : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(THISCALL ClassTy::*)(Types...)&&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(ClassTy::*)(Types..., ...)&&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(THISCALL ClassTy::*)(Types...) const&&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(ClassTy::*)(Types..., ...) const&&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(THISCALL ClassTy::*)(
        Types...) volatile&&> : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(ClassTy::*)(Types..., ...) volatile&&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(THISCALL ClassTy::*)(Types...)
        const volatile&&> : public true_type {};
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(ClassTy::*)(Types..., ...)
        const volatile&&> : public true_type {};

#ifndef _M_AMD64
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(STDCALL ClassTy::*)(Types...)>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(FASTCALL ClassTy::*)(Types...)>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(CRTCALL ClassTy::*)(Types...)>
        : public true_type {};
#endif

#if (defined(_M_IX86_FP) && (_M_IX86_FP >= 2) || defined(_M_X64))
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(VECTORCALL ClassTy::*)(Types...)>
        : public true_type {};

#ifndef _M_AMD64
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(STDCALL ClassTy::*)(Types...) const>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(FASTCALL ClassTy::*)(Types...) const>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(__cdecl ClassTy::*)(Types...) const>
        : public true_type {};
#endif

#if (defined(_M_IX86_FP) && (_M_IX86_FP >= 2) || defined(_M_X64))
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(VECTORCALL ClassTy::*)(Types...) const>
        : public true_type {};
#endif

    // volatile:
#ifndef _M_AMD64
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(STDCALL ClassTy::*)(Types...) volatile>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(FASTCALL ClassTy::*)(Types...) volatile>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(__cdecl ClassTy::*)(Types...) volatile>
        : public true_type {};
#endif

#if (defined(_M_IX86_FP) && (_M_IX86_FP >= 2) || defined(_M_X64))
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(VECTORCALL ClassTy::*)(
        Types...) volatile> : public true_type {};
#endif

#ifndef _M_AMD64
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(STDCALL ClassTy::*)(Types...)
        const volatile> : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(FASTCALL ClassTy::*)(Types...)
        const volatile> : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(__cdecl ClassTy::*)(Types...)
        const volatile> : public true_type {};
#endif

#if (defined(_M_IX86_FP) && (_M_IX86_FP >= 2) || defined(_M_X64))
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(VECTORCALL ClassTy::*)(Types...)
        const volatile> : public true_type {};
#endif

    // reference qualified:
#ifndef _M_AMD64
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(STDCALL ClassTy::*)(Types...)&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(FASTCALL ClassTy::*)(Types...)&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(__cdecl ClassTy::*)(Types...)&>
        : public true_type {};
#endif

#if (defined(_M_IX86_FP) && (_M_IX86_FP >= 2) || defined(_M_X64))
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(VECTORCALL ClassTy::*)(Types...)&>
        : public true_type {};
#endif

    // const:
#ifndef _M_AMD64
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(STDCALL ClassTy::*)(Types...) const&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(FASTCALL ClassTy::*)(Types...) const&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(__cdecl ClassTy::*)(Types...) const&>
        : public true_type {};
#endif

#if (defined(_M_IX86_FP) && (_M_IX86_FP >= 2) || defined(_M_X64))
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(VECTORCALL ClassTy::*)(Types...) const&>
        : public true_type {};
#endif

    // volatile:
#ifndef _M_AMD64
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(STDCALL ClassTy::*)(Types...) volatile&>
        : public true_type {};
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(FASTCALL ClassTy::*)(Types...) volatile&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(__cdecl ClassTy::*)(Types...) volatile&>
        : public true_type {};
#endif

#if (defined(_M_IX86_FP) && (_M_IX86_FP >= 2) || defined(_M_X64))
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(VECTORCALL ClassTy::*)(
        Types...) volatile&> : public true_type {};
#endif

    // const volatile:
#ifndef _M_AMD64
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(STDCALL ClassTy::*)(Types...)
        const volatile&> : public true_type {};
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(FASTCALL ClassTy::*)(Types...)
        const volatile&> : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(__cdecl ClassTy::*)(Types...)
        const volatile&> : public true_type {};
#endif

#if (defined(_M_IX86_FP) && (_M_IX86_FP >= 2) || defined(_M_X64))
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(VECTORCALL ClassTy::*)(Types...)
        const volatile&> : public true_type {};
#endif

    // rvalue reference qualified:
#ifndef _M_AMD64
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(STDCALL ClassTy::*)(Types...)&&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(FASTCALL ClassTy::*)(Types...)&&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(__cdecl ClassTy::*)(Types...)&&>
        : public true_type {};
#endif

#if (defined(_M_IX86_FP) && (_M_IX86_FP >= 2) || defined(_M_X64))
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(VECTORCALL ClassTy::*)(Types...)&&>
        : public true_type {};
#endif

    // const:
#ifndef _M_AMD64
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(STDCALL ClassTy::*)(Types...) const&&>
        : public true_type {};
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(FASTCALL ClassTy::*)(Types...) const&&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(__cdecl ClassTy::*)(Types...) const&&>
        : public true_type {};
#endif

#if (defined(_M_IX86_FP) && (_M_IX86_FP >= 2) || defined(_M_X64))
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(VECTORCALL ClassTy::*)(Types...) const&&>
        : public true_type {};
#endif

    // volatile:
#ifndef _M_AMD64
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(STDCALL ClassTy::*)(Types...) volatile&&>
        : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(FASTCALL ClassTy::*)(
        Types...) volatile&&> : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(__cdecl ClassTy::*)(Types...) volatile&&>
        : public true_type {};
#endif
#if (defined(_M_IX86_FP) && (_M_IX86_FP >= 2) || defined(_M_X64))
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(VECTORCALL ClassTy::*)(
        Types...) volatile&&> : public true_type {};
#endif

    // const volatile:
#ifndef _M_AMD64
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(STDCALL ClassTy::*)(Types...)
        const volatile&&> : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(FASTCALL ClassTy::*)(Types...)
        const volatile&&> : public true_type {};

    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(__cdecl ClassTy::*)(Types...)
        const volatile&&> : public true_type {};
#endif

#if (defined(_M_IX86_FP) && (_M_IX86_FP >= 2) || defined(_M_X64))
    template <class Ret, class ClassTy, class... Types>
    struct is_member_function_pointer<Ret(VECTORCALL ClassTy::*)(Types...)
        const volatile&&> : public true_type {};
#endif
#endif

    template <class Ty>
    inline constexpr bool is_member_function_pointer_v =
        is_member_function_pointer<Ty>::value;

    template <class Ty>
    struct is_member_pointer {
        static constexpr bool value =
            is_member_object_pointer_v<Ty> || is_member_function_pointer_v<Ty>;
    };

    template <class Ty>
    inline constexpr bool is_member_pointer_v = is_member_pointer<Ty>::value;

    template <class Base, class Derived>
    inline constexpr bool is_base_of_v = __is_base_of(Base, Derived);

    template <class Base, class Derived>
    struct is_base_of : bool_constant<is_base_of_v<Base, Derived>> {};

    template <class Ty>
    struct is_scalar {
        static constexpr bool value = is_arithmetic_v<Ty> || is_enum_v<Ty> ||
            is_pointer_v<Ty> || is_member_pointer_v<Ty> ||
            is_null_pointer_v<Ty>;
    };

    template <class Ty>
    inline constexpr bool is_scalar_v = is_scalar<Ty>::value;

    template <bool IsFirst, class Head, class... Tail>
    struct conjunction_impl {
        using type = Head;
    };

    template <class True, class Next, class... Tail>
    struct conjunction_impl<true, True, Next, Tail...> {
        using type = typename conjunction_impl<Next::value, Next, Tail...>::type;
    };

    template <class... Traits>
    struct conjunction : true_type {};

    template <class Head, class... Tail>
    struct conjunction<Head, Tail...>
        : conjunction_impl<Head::value, Head, Tail...>::type {};

    template <class... Traits>
    inline constexpr bool conjunction_v = conjunction<Traits...>::value;

    template <class Trait>
    inline constexpr bool negation_v = !Trait::value;

    template <class Trait>
    struct negation : bool_constant<negation_v<Trait>> {};

    template <class Ty, template <class...> class Template>
    inline constexpr bool is_specialization_v = false;

    template <template <class...> class Template, class... Types>
    inline constexpr bool is_specialization_v<Template<Types...>, Template> = true;

    template <class Ty, template <class...> class Template>
    struct is_specialization : bool_constant<is_specialization_v<Ty, Template>> {};

#define DEFINE_GET_NESTED_TYPE(NestedType)                       \
  template <class Ty, class = void>                              \
  struct get_##NestedType {};                                    \
                                                                 \
  template <class Ty>                                            \
  struct get_##NestedType<Ty, void_t<typename Ty::NestedType>> { \
    using type = typename Ty::NestedType;                        \
  };                                                             \
                                                                 \
  template <class Ty>                                            \
  using get_##NestedType##_t = typename get_##NestedType<Ty>::type;

#define DEFINE_HAS_NESTED_TYPE(NestedType)                                     \
  template <class Ty, class = void>                                            \
  struct has_##NestedType : false_type {};                                     \
                                                                               \
  template <class Ty>                                                          \
  struct has_##NestedType<Ty, void_t<typename Ty::NestedType>> : true_type {}; \
                                                                               \
  template <class Ty>                                                          \
  inline constexpr bool has_##NestedType##_v = has_##NestedType<Ty>::value;

    template <class Ty, class = void>
    struct is_dereferenceable : false_type {};

    template <class Ty>
    struct is_dereferenceable<Ty, void_t<decltype(*declval<Ty>())>> : true_type {};

    template <class Ty>
    inline constexpr bool is_dereferenceable_v = is_dereferenceable<Ty>::value;

    struct non_copyable {
        non_copyable() = default;
        non_copyable(const non_copyable&) = delete;
        non_copyable& operator=(const non_copyable&) = delete;
        non_copyable(non_copyable&&) = default;
        non_copyable& operator=(non_copyable&&) = default;
        ~non_copyable() = default;
    };

    struct non_relocatable {
        non_relocatable() = default;
        non_relocatable(const non_relocatable&) = delete;
        non_relocatable& operator=(const non_relocatable&) = delete;
        non_relocatable(non_relocatable&&) = delete;
        non_relocatable& operator=(non_relocatable&&) = delete;
        ~non_relocatable() = default;
    };

}  // namespace kmdf_std