#ifndef HASKELL_TRAITS_FUNCTOR_HPP
#define HASKELL_TRAITS_FUNCTOR_HPP

#include <haskell_traits/rebindable.hpp>

HASKELL_TRAITS_BEGIN
    template<typename F>
    struct functor_impl
    {
        // ********* required functions and typedefs +/- cvrefs *********
        // static F<U> fmap(F<T>, f);
    };
HASKELL_TRAITS_END

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename T, typename F>
    using fmappable_ = std::enable_if_t<std::is_same<
        uncvref<decltype(functor_impl<uncvref<T>>::fmap(std::declval<T>(), std::declval<F>()))>,
        uncvref<rebind<T, uncvref<result_t<F, bound_to<T>>>>>>{}>;
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename T, typename F>
    inline constexpr auto fmappable = is_detected<detail::fmappable_, T, F>;

    template<typename T,
             typename F,
             REQUIRES(fmappable<T&&, F&&>),
             typename Impl = functor_impl<uncvref<T>>>
    constexpr auto fmap(T && t, F && f) DECLTYPE_NOEXCEPT_RETURNS(Impl::fmap((T &&) t, (F &&) f));
HASKELL_TRAITS_END

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename T>
    using functor_default_f_t_ = unary_func_t<nonesuch2, const bound_to<T>&>;

    template<typename T>
    using functor_default_f_t = detected_or_unary_nonesuch_t<functor_default_f_t_, T>;
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename T, typename F = detail::functor_default_f_t<T>>
    inline constexpr auto functor = std::bool_constant<fmappable<T, F>>{};

    template<typename T, typename C, typename F = detail::functor_default_f_t<T>>
    inline constexpr auto functor_of = std::bool_constant<functor<T, F> && is_bound_to<T, C>>{};
HASKELL_TRAITS_END

#define FUNCTOR_ASSERTS(...)                                                                       \
    REBINDABLE_ASSERTS(__VA_ARGS__);                                                               \
    static_assert(::haskell_traits::fmappable<                                                     \
                      __VA_ARGS__,                                                                 \
                      ::haskell_traits::unary_func_t<                                              \
                          ::haskell_traits::nonesuch2,                                             \
                          const ::haskell_traits::bound_to<__VA_ARGS__>&>>,                        \
                  #__VA_ARGS__ " does not correctly implement functor_impl::fmap");                \
    static_assert(::haskell_traits::fmappable<                                                     \
                      __VA_ARGS__,                                                                 \
                      ::haskell_traits::unary_func_t<                                              \
                          ::haskell_traits::nonesuch2&,                                            \
                          const ::haskell_traits::bound_to<__VA_ARGS__>&>>,                        \
                  #__VA_ARGS__ " does not correctly implement functor_impl::fmap");                \
    static_assert(::haskell_traits::fmappable<                                                     \
                      __VA_ARGS__,                                                                 \
                      ::haskell_traits::unary_func_t<                                              \
                          ::haskell_traits::nonesuch2&&,                                           \
                          const ::haskell_traits::bound_to<__VA_ARGS__>&>>,                        \
                  #__VA_ARGS__ " does not correctly implement functor_impl::fmap");                \
    static_assert(::haskell_traits::fmappable<                                                     \
                      __VA_ARGS__,                                                                 \
                      ::haskell_traits::unary_func_t<                                              \
                          const ::haskell_traits::nonesuch2&,                                      \
                          const ::haskell_traits::bound_to<__VA_ARGS__>&>>,                        \
                  #__VA_ARGS__ " does not correctly implement functor_impl::fmap");                \
    static_assert(::haskell_traits::fmappable<                                                     \
                      __VA_ARGS__,                                                                 \
                      ::haskell_traits::unary_func_t<                                              \
                          const ::haskell_traits::nonesuch2&&,                                     \
                          const ::haskell_traits::bound_to<__VA_ARGS__>&>>,                        \
                  #__VA_ARGS__ " does not correctly implement functor_impl::fmap");                \
    static_assert(::haskell_traits::functor<__VA_ARGS__>, #__VA_ARGS__ " does not satisfy functor")

#endif /* HASKELL_TRAITS_FUNCTOR_HPP */