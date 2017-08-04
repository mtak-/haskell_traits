#ifndef HASKELL_TRAITS_DETAIL_FUNC_HPP
#define HASKELL_TRAITS_DETAIL_FUNC_HPP

#include <haskell_traits/detail/meta.hpp>

HASKELL_TRAITS_DETAIL_BEGIN
    // modified from cppreference on aug 3, 2017:
    // http://en.cppreference.com/w/cpp/utility/functional/invoke
    template<typename T>
    constexpr bool is_reference_wrapper_v = instantiation_of<std::reference_wrapper, T>;

    template<typename T, typename Type, typename T1, typename... Args>
    constexpr result_t<Type T::*, T1&&, Args&&...>
    INVOKE(Type T::*f, T1 && t1, Args && ... args) noexcept(
        noexcept_callable<Type T::*, T1&&, Args&&...>)
    {
        if
            constexpr(std::is_member_function_pointer_v<decltype(f)>)
            {
                if
                    constexpr(std::is_base_of_v<T, std::decay_t<T1>>) return (
                        std::forward<T1>(t1).*f)(std::forward<Args>(args)...);
                else if
                    constexpr(is_reference_wrapper_v<std::decay_t<T1>>) return (t1.get().*f)(
                        std::forward<Args>(args)...);
                else
                    return ((*std::forward<T1>(t1)).*f)(std::forward<Args>(args)...);
            }
        else {
            static_assert(std::is_member_object_pointer_v<decltype(f)>);
            static_assert(sizeof...(args) == 0);
            if
                constexpr(std::is_base_of_v<T, std::decay_t<T1>>) return std::forward<T1>(t1).*f;
            else if
                constexpr(is_reference_wrapper_v<std::decay_t<T1>>) return t1.get().*f;
            else
                return (*std::forward<T1>(t1)).*f;
        }
    }

    template<typename F, typename... Args>
    constexpr result_t<F&&, Args&&...> INVOKE(F && f, Args && ... args) noexcept(
        noexcept_callable<F&&, Args&&...>)
    {
        return ((F &&) f)((Args &&) args...);
    }
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename F, typename... Args, REQUIRES(callable<F&&, Args&&...>)>
    constexpr result_t<F&&, Args&&...> invoke(F && f, Args && ... args) noexcept(
        noexcept_callable<F&&, Args&&...>)
    {
        return detail::INVOKE((F &&) f, (Args &&) args...);
    }
HASKELL_TRAITS_END

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename F, typename = void>
    struct func_impl : private F
    {
        template<typename... Args, REQUIRES(std::is_constructible_v<F, Args&&...>)>
        constexpr func_impl(Args&&... args) noexcept(std::is_nothrow_constructible_v<F, Args&&...>)
            : F((Args &&) args...)
        {
        }

        using F::operator();
    };

    template<typename F>
    struct func_impl<F, REQUIRES_(!std::is_class_v<F> || std::is_final_v<F>)>
    {
    private:
        F f;

    public:
        template<typename... Args, REQUIRES(std::is_constructible_v<F, Args&&...>)>
        constexpr func_impl(Args&&... args) noexcept(std::is_nothrow_constructible_v<F, Args&&...>)
            : f((Args &&) args...)
        {
        }

#define HASKELL_TRAITS_CALL_OP(cvref)                                                              \
    template<typename... Args, REQUIRES(callable<F cvref, Args...>)>                               \
    constexpr result_t<F cvref, Args&&...> operator()(Args&&... args)                              \
        cvref noexcept(noexcept_callable<F cvref, Args&&...>)                                      \
    {                                                                                              \
        return haskell_traits::invoke(static_cast<F cvref>(f), (Args &&) args...);                 \
    }                                                                                              \
        /**/

        HASKELL_TRAITS_CALL_OP(&)
        HASKELL_TRAITS_CALL_OP(&&)
        HASKELL_TRAITS_CALL_OP(const&)
        HASKELL_TRAITS_CALL_OP(const&&)
#undef HASKELL_TRAITS_CALL_OP
    };
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename F>
    struct func : private detail::func_impl<F>
    {
        static_assert(!std::is_reference_v<F>);
        static_assert(!instantiation_of<func, F>);

        using underlying_t = F;

        using detail::func_impl<F>::func_impl;
        using detail::func_impl<F>::operator();
    };

    template<typename F>
    inline constexpr auto is_func = instantiation_of<func, F>;

    template<typename F, REQUIRES(!is_func<F>)>
    func(F && f)->func<uncvref<F>>;
    template<typename F, REQUIRES(is_func<F>)>
    func(F && f)->func<typename uncvref<F>::underlying_t>;

    template<typename F>
    using func_t = decltype(func(std::declval<F>()));

    template<typename... Fs>
    struct merged : private Fs...
    {
        static_assert((is_func<Fs> && ...));

        template<typename... Fs_In, REQUIRES((std::is_constructible_v<Fs, Fs_In&&> && ...))>
        constexpr merged(Fs_In&&... fs) noexcept(
            (std::is_nothrow_constructible_v<Fs, Fs_In&&> && ...))
            : Fs((Fs_In)fs)...
        {
        }

        using Fs::operator()...;
    };

    template<typename... Fs>
    merged(Fs && ...)->merged<func_t<Fs>...>;

    template<typename... Fs>
    using merged_t = decltype(merged(std::declval<Fs>()...));

    template<typename T>
    struct expected_result
    {
    };
HASKELL_TRAITS_END

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename Expected, typename Func, typename... Args>
    inline constexpr auto callable_returns = std::bool_constant<
        callable<
            Func,
            Args...> && std::is_convertible_v<detected_or<nonesuch, result_t, Func, Args...>, Expected>>{};

    template<typename F>
    struct overload_return_member : private F
    {
        static_assert(is_func<F>);

        using F::F;

#define HASKELL_TRAITS_CALL_OP(cvref)                                                              \
    template<typename Expected,                                                                    \
             typename... Args,                                                                     \
             REQUIRES(callable_returns<Expected, F cvref, Args&&...>)>                             \
    constexpr result_t<F cvref, Args&&...> operator()(expected_result<Expected>, Args&&... args)   \
        cvref noexcept(noexcept(static_cast<F cvref>(*this)((Args &&) args...)))                   \
    {                                                                                              \
        return static_cast<F cvref>(*this)((Args &&) args...);                                     \
    }                                                                                              \
        /**/

        HASKELL_TRAITS_CALL_OP(&)
        HASKELL_TRAITS_CALL_OP(&&)
        HASKELL_TRAITS_CALL_OP(const&)
        HASKELL_TRAITS_CALL_OP(const&&)
#undef HASKELL_TRAITS_CALL_OP

        using F::operator();
    };
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename... Funcs>
    struct overload_return : private detail::overload_return_member<Funcs>...
    {
        template<typename... Funcs_In,
                 REQUIRES((std::is_constructible_v<Funcs, Funcs_In&&> && ...))>
        constexpr overload_return(Funcs_In&&... fs) noexcept(
            (std::is_nothrow_constructible_v<Funcs, Funcs_In&&> && ...))
            : detail::overload_return_member<Funcs>((Funcs_In &&) fs)...
        {
        }

        using detail::overload_return_member<Funcs>::operator()...;
    };

    template<typename... Fs>
    overload_return(Fs && ... fs)->overload_return<func_t<uncvref<Fs>>...>;

    template<typename... Fs>
    using overload_return_t = decltype(overload_return(std::declval<Fs>()...));
HASKELL_TRAITS_END

#endif /* HASKELL_TRAITS_DETAIL_FUNC_HPP */