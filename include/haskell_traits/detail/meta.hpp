#ifndef HASKELL_TRAITS_DETAIL_META_HPP
#define HASKELL_TRAITS_DETAIL_META_HPP

#include <functional>
#include <type_traits>

#define HASKELL_TRAITS_BEGIN                                                                       \
    namespace haskell_traits                                                                       \
    {                                                                                              \
        inline namespace v1                                                                        \
        {
#define HASKELL_TRAITS_END                                                                         \
    }                                                                                              \
    }
#define HASKELL_TRAITS_DETAIL_BEGIN                                                                \
    HASKELL_TRAITS_BEGIN                                                                           \
        namespace detail                                                                           \
        {
#define HASKELL_TRAITS_DETAIL_END                                                                  \
    HASKELL_TRAITS_END                                                                             \
    }

#define PP_CONCAT(a, ...) _PP_CONCAT(a, __VA_ARGS__)
#define _PP_CONCAT(a, ...) a##__VA_ARGS__

#define REQUIRES(...)                                                                              \
    int PP_CONCAT(__concept_check, __LINE__)                                                       \
        = 42,                                                                                      \
        std::enable_if_t < PP_CONCAT(__concept_check, __LINE__) == 43 || (__VA_ARGS__), int > = 42

#define REQUIRESF(...) std::enable_if_t<(__VA_ARGS__), int> = 42

#define REQUIRES_(...) std::enable_if_t<(__VA_ARGS__)>
#define NOEXCEPT_RETURNS(...)                                                                      \
    noexcept(noexcept(__VA_ARGS__)) { return __VA_ARGS__; }
#define DECLTYPE_NOEXCEPT_RETURNS(...)                                                             \
    noexcept(noexcept(__VA_ARGS__))->decltype(__VA_ARGS__) { return __VA_ARGS__; }

HASKELL_TRAITS_BEGIN
    template<typename T>
    using uncvref = std::remove_cv_t<std::remove_reference_t<T>>;

    template<typename T>
    using _t = typename T::type;

    template<typename T>
    struct id
    {
        using type = T;
    };

    template<typename T>
    using id_t = _t<id<T>>;

    template<typename T, typename As>
    using as_same_const = std::conditional_t<std::is_const<As>::value, std::add_const_t<T>, T>;

    template<typename T, typename As>
    using as_same_volatile
        = std::conditional_t<std::is_volatile<As>::value, std::add_volatile_t<T>, T>;

    template<typename T, typename As>
    using as_same_cv = as_same_const<as_same_volatile<T, As>, As>;

    // no-ref=rvalue ref
    template<typename T, typename As>
    using as_same_ref = std::conditional_t<
        std::is_lvalue_reference<As>::value,
        std::add_lvalue_reference_t<T>,
        std::conditional_t<std::is_rvalue_reference<T>::value, std::add_rvalue_reference_t<T>, T>>;

    template<typename T, typename As>
    using as_same_cvref = as_same_ref<as_same_cv<T, std::remove_reference_t<As>>, As>;

    struct nonesuch
    {
        nonesuch()                = delete;
        ~nonesuch()               = delete;
        nonesuch(nonesuch const&) = delete;
        void operator=(nonesuch const&) = delete;
    };
    struct nonesuch2
    {
    };

    namespace detail
    {
        template<class Default, class AlwaysVoid, template<class...> class Op, class... Args>
        struct detector
        {
            using value_t = std::false_type;
            using type    = Default;
        };

        template<class Default, template<class...> class Op, class... Args>
        struct detector<Default, std::void_t<Op<Args...>>, Op, Args...>
        {
            // Note that std::void_t is a C++17 feature
            using value_t = std::true_type;
            using type    = Op<Args...>;
        };

    } // namespace detail

    template<template<class...> class Op, class... Args>
    inline constexpr auto is_detected =
        typename detail::detector<nonesuch, void, Op, Args...>::value_t{};

    template<template<class...> class Op, class... Args>
    using detected_t = _t<detail::detector<nonesuch, void, Op, Args...>>;

    template<class Default, template<class...> class Op, class... Args>
    using detected_or = _t<detail::detector<Default, void, Op, Args...>>;

    template<typename F, typename... Args>
    using callable_ = decltype(std::invoke(std::declval<F>(), std::declval<Args>()...));

    template<typename F, typename... Args>
    inline constexpr auto callable = is_detected<callable_, F, Args...>;

    template<typename F, typename... Args>
    using result_t = decltype(std::invoke(std::declval<F>(), std::declval<Args>()...));

    template<bool b, typename F, typename... Args>
    struct noexcept_callable_ : std::false_type
    {
    };

    template<typename F, typename... Args>
    struct noexcept_callable_<true, F, Args...>
        : std::bool_constant<noexcept(std::invoke(std::declval<F>(), std::declval<Args>()...))>
    {
    };

    template<typename F, typename... Args>
    inline constexpr auto noexcept_callable
        = _t<noexcept_callable_<callable<F, Args...>, F, Args...>>{};

    template<template<typename...> typename F, typename G, typename = void>
    struct instantiation_of_ : std::false_type
    {
    };

    template<template<typename...> typename F, template<typename> typename G, typename... Args>
    struct instantiation_of_<F, G<Args...>, std::void_t<F<Args...>>>
        : _t<std::is_same<F<Args...>, G<Args...>>>
    {
    };

    template<template<typename...> typename F, typename G>
    inline constexpr auto instantiation_of = _t<instantiation_of_<F, uncvref<G>>>{};

    template<typename F, typename G>
    struct same_template_ : std::false_type
    {
    };

    template<template<typename...> typename F, typename... Args, typename... Args2>
    struct same_template_<F<Args...>, F<Args2...>> : std::true_type
    {
    };

    template<typename F, typename G>
    inline constexpr auto same_template = _t<same_template_<uncvref<F>, uncvref<G>>>{};

    template<typename R, typename T>
    R unary_func(T) noexcept;

    template<typename R, typename T>
    using unary_func_t = decltype(&unary_func<R, T>);

    template<template<typename...> typename Desired, typename... Ts>
    using detected_or_unary_nonesuch_t
        = detected_or<unary_func_t<nonesuch2, nonesuch2>, Desired, Ts...>;
HASKELL_TRAITS_END

#endif /* HASKELL_TRAITS_DETAIL_META_HPP */