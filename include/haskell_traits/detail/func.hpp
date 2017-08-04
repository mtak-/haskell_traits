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
    struct func_traits
    {
        using underlying_t = F;

        template<typename F0, typename... Args>
        using possible_results_t = type_list<result_t<F0, Args...>>;

        static constexpr inline bool satisfies_func = false;
    };

    template<typename F>
    struct func_traits<const F> : func_traits<F>
    {
    };

    template<typename F>
    struct func_traits<F&> : func_traits<F>
    {
    };

    template<typename F>
    struct func_traits<F&&> : func_traits<F>
    {
    };

    template<typename F>
    using underlying_t = typename func_traits<F>::underlying_t;

    template<typename F, typename... Args>
    using possible_results_t = typename func_traits<F>::template possible_results_t<F, Args...>;

    template<typename F>
    inline constexpr auto satisfies_func = func_traits<F>::satisfies_func;

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
    using func_t = std::conditional_t<satisfies_func<F>, F, decltype(func(std::declval<F>()))>;

    template<typename F>
    struct func_traits<func<F>>
    {
        using underlying_t = F;

        template<typename F0, typename... Args>
        using possible_results_t = possible_results_t<as_same_cvref<F, F0>, Args...>;

        static constexpr inline bool satisfies_func = true;
    };

    template<typename... Fs>
    struct merged : private Fs...
    {
        static_assert((satisfies_func<Fs> && ...));

        template<typename... Fs_In, REQUIRES((std::is_constructible_v<Fs, Fs_In&&> && ...))>
        constexpr merged(Fs_In&&... fs) noexcept(
            (std::is_nothrow_constructible_v<Fs, Fs_In&&> && ...))
            : Fs((Fs_In)fs)...
        {
        }

        using Fs::operator()...;
    };

    template<typename F>
    inline constexpr auto is_merged = instantiation_of<merged, F>;

    template<typename... Fs, REQUIRES(sizeof...(Fs) != 1 || !(is_merged<Fs> && ...))>
    merged(Fs && ...)->merged<func_t<Fs>...>;

    template<typename... Fs>
    merged(merged<Fs...> &&)->merged<Fs...>;

    template<typename... Fs>
    merged(const merged<Fs...>&)->merged<Fs...>;

    template<typename... Fs>
    using merged_t = decltype(merged(std::declval<Fs>()...));

    template<typename... Fs>
    struct func_traits<merged<Fs...>>
    {
        using underlying_t = merged<Fs...>;

        template<typename F0, typename... Args>
        using possible_results_t = concat<possible_results_t<as_same_cvref<Fs, F0>, Args...>...>;

        static constexpr inline bool satisfies_func = true;
    };

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
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename F>
    struct func_return : private F
    {
        static_assert(satisfies_func<F>);

        template<typename... Args, REQUIRES(std::is_constructible_v<F, Args&&...>)>
        constexpr func_return(Args&&... args) noexcept(
            std::is_nothrow_constructible_v<F, Args&&...>)
            : F((Args &&) args...)
        {
        }

#define HASKELL_TRAITS_CALL_OP(cvref)                                                              \
    template<typename Expected,                                                                    \
             typename... Args,                                                                     \
             REQUIRES(detail::callable_returns<Expected, F cvref, Args&&...>)>                     \
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

    template<typename F>
    inline constexpr auto is_func_return = instantiation_of<func_return, F>;

    template<typename F,
             REQUIRES(std::is_constructible_v<func_return<func_t<F>>, F&&> && !is_func_return<F>)>
    func_return(F &&)->func_return<func_t<F>>;

    template<typename F>
    func_return(func_return<F> &&)->func_return<F>;

    template<typename F>
    func_return(const func_return<F>&)->func_return<F>;

    template<typename F>
    using func_return_t = decltype(func_return(std::declval<F>()));

    template<typename F>
    struct func_traits<func_return<F>>
    {
        using underlying_t = underlying_t<F>;

        template<typename F0, typename... Args>
        using possible_results_t = possible_results_t<as_same_cvref<F, F0>, Args...>;

        static constexpr inline bool satisfies_func = true;
    };

    struct merged_return_fn
    {
        template<typename... Fs,
                 REQUIRES(std::is_constructible_v<merged_t<func_return_t<Fs>...>,
                                                  func_return_t<Fs>&&...>)>
        constexpr merged_t<func_return_t<Fs>...> operator()(Fs&&... fs) const
            NOEXCEPT_RETURNS(merged_t<func_return_t<Fs>...>(func_return_t<Fs>((Fs &&) fs)...));
    } inline constexpr merged_return{};

    template<typename... Fs>
    using merged_return_t = decltype(merged_return(std::declval<Fs>()...));
HASKELL_TRAITS_END

#endif /* HASKELL_TRAITS_DETAIL_FUNC_HPP */