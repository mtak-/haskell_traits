#ifndef HASKELL_TRAITS_APPLICATIVE_HPP
#define HASKELL_TRAITS_APPLICATIVE_HPP

#include <haskell_traits/functor.hpp>

HASKELL_TRAITS_BEGIN
    template<typename A>
    struct applicative_impl
    {
        // ********* required functions and typedefs +/- cvrefs *********
        // static A<U> apure(U);
        // static A<U> aapply(A<T>, A<F>);
    };
HASKELL_TRAITS_END

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename T>
    using apurable_ = std::enable_if_t<
        std::is_same<uncvref<decltype(applicative_impl<T>::apure(std::declval<bound_to<T>>()))>,
                     T>{}>;

    template<typename T, typename F>
    using aapplyable_ = std::enable_if_t<std::is_same<
        uncvref<decltype(applicative_impl<T>::aapply(std::declval<T>(), std::declval<F>()))>,
        uncvref<rebind<T, uncvref<result_t<bound_to<F>, bound_to<T>>>>>>{}>;
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename T>
    inline constexpr auto apurable = is_detected<detail::apurable_, uncvref<T>>;

    template<typename T, typename F>
    inline constexpr auto aapplyable = is_detected<detail::aapplyable_, uncvref<T>, uncvref<F>>;

    template<typename T,
             typename U = bound_to<T>,
             REQUIRES(apurable<T> && is_bound_to<T, U>),
             typename Impl = applicative_impl<uncvref<T>>>
    constexpr auto apure_strict(U && u) DECLTYPE_NOEXCEPT_RETURNS(Impl::apure((U &&) u));

    template<typename T,
             typename F,
             REQUIRES(aapplyable<T&&, F&&>),
             typename Impl = applicative_impl<uncvref<T>>>
    constexpr auto aapply(T && t, F && f)
        DECLTYPE_NOEXCEPT_RETURNS(Impl::aapply((T &&) t, (F &&) f));
HASKELL_TRAITS_END

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename T>
    using applicative_default_f_t_
        = rebind<uncvref<T>, unary_func_t<nonesuch2, const bound_to<T>&>>;

    template<typename T>
    using applicative_default_f_t = detected_or_unary_nonesuch_t<applicative_default_f_t_, T>;
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename T, typename F = detail::applicative_default_f_t<T>>
    inline constexpr auto applicative
        = std::bool_constant<functor<T> && apurable<T> && aapplyable<T, F>>{};

    template<typename T, typename C, typename F = detail::applicative_default_f_t<T>>
    inline constexpr auto applicative_of
        = std::bool_constant<applicative<T, F> && is_bound_to<T, C>>{};
HASKELL_TRAITS_END

#define APPLICATIVE_ASSERTS(...)                                                                   \
    FUNCTOR_ASSERTS(__VA_ARGS__);                                                                  \
    static_assert(::haskell_traits::applicative<__VA_ARGS__>,                                      \
                  #__VA_ARGS__ " does not satisfy applicative")

#endif /* HASKELL_TRAITS_APPLICATIVE_HPP */