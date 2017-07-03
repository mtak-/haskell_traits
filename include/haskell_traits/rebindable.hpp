#ifndef HASKELL_TRAITS_REBINDABLE_HPP
#define HASKELL_TRAITS_REBINDABLE_HPP

#include <haskell_traits/detail/meta.hpp>

HASKELL_TRAITS_BEGIN
    template<typename T>
    struct rebind_impl
    {
        // ********* required functions and typedefs +/- cvrefs *********
        // template<typename U>
        // using rebind = M<U>;

        // using bound_to = T;
    };

    template<typename T>
    using bound_to = typename rebind_impl<uncvref<T>>::bound_to;
    template<typename T, typename U>
    using rebind = as_same_cvref<typename rebind_impl<uncvref<T>>::template rebind<U>, T>;
HASKELL_TRAITS_END

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename T, typename U>
    using rebindable_ = std::enable_if_t<
        same_template<rebind<T, U>, T> && std::is_same_v<bound_to<rebind<T, U>>, U>>;
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename T, typename U = nonesuch2>
    inline constexpr auto rebindable
        = std::bool_constant<is_detected<detail::rebindable_, T, U> && is_detected<bound_to, T>>{};
HASKELL_TRAITS_END

#define REBINDABLE_ASSERTS(...)                                                                    \
    static_assert(::haskell_traits::is_detected<::haskell_traits::bound_to, __VA_ARGS__>,          \
                  #__VA_ARGS__ " does not correctly implement rebind_impl::bound_to");             \
    static_assert(::haskell_traits::is_detected<::haskell_traits::rebind,                          \
                                                __VA_ARGS__,                                       \
                                                ::haskell_traits::nonesuch2>,                      \
                  #__VA_ARGS__ " does not correctly implement rebind_impl::rebind_to");            \
    static_assert(::haskell_traits::rebindable<__VA_ARGS__>,                                       \
                  #__VA_ARGS__ " does not satisfy rebindable")

#endif /* HASKELL_TRAITS_REBINDABLE_HPP */