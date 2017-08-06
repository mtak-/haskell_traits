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
    template<typename F>
    using underlying_t_ = typename uncvref<F>::underlying_t;
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename F>
    using underlying_t = detected_or<F, detail::underlying_t_, F>;
HASKELL_TRAITS_END

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename F, typename... Args>
    using mem_possible_results_t = typename uncvref<F>::template possible_results_t<F, Args...>;

    template<typename F, typename... Args>
    using result_list_t = type_list<result_t<F, Args...>>;

    template<typename F, typename... Args>
    using possible_results_t_ = detected_or<detected_or<type_list<>, result_list_t, F, Args...>,
                                            mem_possible_results_t,
                                            F,
                                            Args...>;
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename F, typename... Args>
    using possible_results_t = detail::possible_results_t_<F, Args...>;

    template<typename F>
    inline constexpr auto satisfies_func = std::is_class_v<F>;

    struct unknown_result
    {
        template<typename T>
        operator T() noexcept;
        template<typename T>
        operator T&() noexcept;
        template<typename T>
        operator T &&() noexcept;
    };
HASKELL_TRAITS_END

HASKELL_TRAITS_DETAIL_BEGIN
    template<
        typename F,
        bool = std::is_class_v<F> && !std::is_final_v<F>,
        bool
        = std::is_class_v<
              F> || std::is_member_object_pointer_v<F> || std::is_member_function_pointer_v<F> || std::is_function_v<std::remove_pointer_t<uncvref<F>>>>
    struct func_impl;

    template<typename F>
    struct func_impl<F, true, true> : private F
    {
        using underlying_t = underlying_t<F>;

        template<typename... Args, REQUIRES(std::is_constructible_v<F, Args&&...>)>
        constexpr func_impl(Args&&... args) noexcept(std::is_nothrow_constructible_v<F, Args&&...>)
            : F((Args &&) args...)
        {
        }

        using F::operator();
    };

    template<typename F>
    struct func_impl<F, false, true>
    {
        using underlying_t = underlying_t<std::decay_t<F>>;

    private:
        underlying_t f;

    public:
        template<typename... Args, REQUIRES(std::is_constructible_v<underlying_t, Args&&...>)>
        constexpr func_impl(Args&&... args) noexcept(
            std::is_nothrow_constructible_v<underlying_t, Args&&...>)
            : f((Args &&) args...)
        {
        }

#define HASKELL_TRAITS_CALL_OP(cvref)                                                              \
    template<typename... Args, REQUIRES(callable<underlying_t cvref, Args...>)>                    \
    constexpr result_t<underlying_t cvref, Args&&...> operator()(Args&&... args)                   \
        cvref noexcept(noexcept_callable<underlying_t cvref, Args&&...>)                           \
    {                                                                                              \
        return haskell_traits::invoke(static_cast<underlying_t cvref>(f), (Args &&) args...);      \
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

        using underlying_t = typename detail::func_impl<F>::underlying_t;

        template<typename F0, typename... Args>
        using possible_results_t = possible_results_t<as_same_cvref<underlying_t, F0>, Args...>;

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
    using func_t = std::
        conditional_t<satisfies_func<uncvref<F>>, uncvref<F>, decltype(func(std::declval<F>()))>;

    template<typename... Fs>
    struct merged : private Fs...
    {
        static_assert((satisfies_func<Fs> && ...));

        template<typename F0, typename... Args>
        using possible_results_t = concat<possible_results_t<as_same_cvref<Fs, F0>, Args...>...>;

        template<typename... Fs_In,
                 REQUIRES(sizeof...(Fs_In) == sizeof...(Fs)),
                 REQUIRES((std::is_constructible_v<Fs, Fs_In&&> && ...))>
        constexpr merged(Fs_In&&... fs) noexcept(
            (std::is_nothrow_constructible_v<Fs, Fs_In&&> && ...))
            : Fs((Fs_In &&) fs)...
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
    using merged_t = std::conditional_t<sizeof...(Fs) != 1,
                                        decltype(merged(std::declval<Fs>()...)),
                                        func_t<detected_or<void (*)(), front, type_list<Fs...>>>>;

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

        using underlying_t = underlying_t<F>;

        template<typename F0, typename... Args>
        using possible_results_t = possible_results_t<as_same_cvref<underlying_t, F0>, Args...>;

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

    template<typename... Fs>
    struct merged_return : private merged_t<func_return_t<Fs>...>
    {
    private:
        using base = merged_t<func_return_t<Fs>...>;
        static_assert((satisfies_func<Fs> && ...));

    public:
        using underlying_t = underlying_t<base>;
        template<typename F0, typename... Args>
        using possible_results_t = possible_results_t<as_same_cvref<base, F0>, Args...>;

        template<typename... Fs_In,
                 REQUIRES(sizeof...(Fs_In) == sizeof...(Fs)),
                 REQUIRES(std::is_constructible_v<base, Fs_In&&...>)>
        constexpr merged_return(Fs_In&&... fs) noexcept(
            std::is_nothrow_constructible_v<base, Fs_In&&...>)
            : base((Fs_In &&) fs...)
        {
        }

        using base::operator();
    };

    template<typename F>
    inline constexpr auto is_merged_return = instantiation_of<merged_return, F>;

    template<typename... Fs, REQUIRES(sizeof...(Fs) != 1 || !(is_merged_return<Fs> && ...))>
    merged_return(Fs && ...)->merged_return<std::decay_t<Fs>...>;

    template<typename... Fs>
    merged_return(merged_return<Fs...> &&)->merged_return<Fs...>;

    template<typename... Fs>
    merged_return(const merged_return<Fs...>&)->merged_return<Fs...>;

    template<typename... Fs>
    using merged_return_t = decltype(merged_return(std::declval<Fs>()...));
HASKELL_TRAITS_END

#endif /* HASKELL_TRAITS_DETAIL_FUNC_HPP */