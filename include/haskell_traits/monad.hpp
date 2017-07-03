#ifndef HASKELL_TRAITS_MONAD_HPP
#define HASKELL_TRAITS_MONAD_HPP

#include <haskell_traits/functor.hpp>

HASKELL_TRAITS_BEGIN
    template<typename M>
    struct monad_impl
    {
        // ********* required functions and typedefs +/- cvrefs *********
        // template<typename T>
        // static M<T> mreturn(const T&);

        // result must be the same monad, tho inner type may vary
        // template<typename T, typename F>
        // static result_t<const func<F>&, const T&> bind(const M<T>&, const func<F>&);
    };
HASKELL_TRAITS_END

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename T>
    using mreturnable_ = std::enable_if_t<
        std::is_same<uncvref<decltype(monad_impl<T>::mreturn(std::declval<bound_to<T>>()))>, T>{}>;

    template<typename T, typename F>
    using mbind_returns_correct_template_
        = std::enable_if_t<same_template<uncvref<result_t<F, bound_to<T>>>, T>>;

    template<typename T, typename F>
    using mbind_returns_correct_type_ = std::enable_if_t<
        std::is_same<uncvref<decltype(monad_impl<T>::mbind(std::declval<T>(), std::declval<F>()))>,
                     uncvref<result_t<F, bound_to<T>>>>{}>;
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename T>
    inline constexpr auto mreturnable = is_detected<detail::mreturnable_, uncvref<T>>;

    template<typename T, typename F>
    inline constexpr auto mbind_returns_correct_template
        = is_detected<detail::mbind_returns_correct_template_, uncvref<T>, uncvref<F>>;

    template<typename T, typename F>
    inline constexpr auto mbind_returns_correct_type
        = is_detected<detail::mbind_returns_correct_type_, uncvref<T>, uncvref<F>>;

    template<typename T, typename F>
    inline constexpr auto mbindable = std::bool_constant<
        mbind_returns_correct_template<T, F> && mbind_returns_correct_type<T, F>>{};

    template<typename T,
             typename U = bound_to<T>,
             REQUIRES(mreturnable<T> && std::is_same_v<uncvref<U>, uncvref<bound_to<T>>>),
             typename Impl = monad_impl<uncvref<T>>>
    auto mreturn(U && u) DECLTYPE_NOEXCEPT_RETURNS(Impl::mreturn((U &&) u));

    template<typename T,
             typename F,
             REQUIRES(mbindable<T&&, F&&>),
             typename Impl = monad_impl<uncvref<T>>>
    auto mbind(T && t, F && f) DECLTYPE_NOEXCEPT_RETURNS(Impl::mbind((T &&) t, (F &&) f));

    template<typename T>
    inline constexpr auto monad = std::bool_constant<
        functor<T> && mreturnable<T> && mbindable<T, unary_func_t<T, const bound_to<T>&>>>{};
HASKELL_TRAITS_END

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename T, typename C>
    using monad_of_
        = std::enable_if_t<monad<T> && std::is_same_v<uncvref<bound_to<T>>, uncvref<C>>>;
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename T, typename C>
    inline constexpr auto monad_of = is_detected<detail::monad_of_, T, C>;
HASKELL_TRAITS_END

#define MONAD_ASSERTS(...)                                                                         \
    FUNCTOR_ASSERTS(__VA_ARGS__);                                                                  \
    static_assert(::haskell_traits::mreturnable<__VA_ARGS__>,                                      \
                  #__VA_ARGS__ " does not correctly implement monad_impl::mreturn");               \
    static_assert(::haskell_traits::mbind_returns_correct_template<                                \
                      __VA_ARGS__,                                                                 \
                      ::haskell_traits::unary_func_t<__VA_ARGS__,                                  \
                                                     ::haskell_traits::bound_to<__VA_ARGS__>>>,    \
                  #__VA_ARGS__                                                                     \
                  " monad_impl::mbind returns a value not in the original monad (template)");      \
    static_assert(::haskell_traits::mbind_returns_correct_template<                                \
                      __VA_ARGS__,                                                                 \
                      ::haskell_traits::unary_func_t<                                              \
                          ::haskell_traits::rebind<__VA_ARGS__, ::haskell_traits::nonesuch2>,      \
                          ::haskell_traits::bound_to<__VA_ARGS__>>>,                               \
                  #__VA_ARGS__                                                                     \
                  " monad_impl::mbind returns a value not in the original monad (template)");      \
    static_assert(::haskell_traits::mbind_returns_correct_type<                                    \
                      __VA_ARGS__,                                                                 \
                      ::haskell_traits::unary_func_t<__VA_ARGS__,                                  \
                                                     ::haskell_traits::bound_to<__VA_ARGS__>>>,    \
                  #__VA_ARGS__                                                                     \
                  " monad_impl::mbind returns a type that doesn't match the return type of the "   \
                  "function passed in");                                                           \
    static_assert(::haskell_traits::mbind_returns_correct_type<                                    \
                      __VA_ARGS__,                                                                 \
                      ::haskell_traits::unary_func_t<                                              \
                          ::haskell_traits::rebind<__VA_ARGS__, ::haskell_traits::nonesuch2>,      \
                          ::haskell_traits::bound_to<__VA_ARGS__>>>,                               \
                  #__VA_ARGS__                                                                     \
                  " monad_impl::mbind returns a type that doesn't match the return type of the "   \
                  "function passed in");                                                           \
    static_assert(::haskell_traits::mbindable<                                                     \
                      __VA_ARGS__,                                                                 \
                      ::haskell_traits::unary_func_t<__VA_ARGS__,                                  \
                                                     ::haskell_traits::bound_to<__VA_ARGS__>>>,    \
                  #__VA_ARGS__ " does not correctly implement monad_impl::mbind");                 \
    static_assert(::haskell_traits::mbindable<                                                     \
                      __VA_ARGS__,                                                                 \
                      ::haskell_traits::unary_func_t<                                              \
                          ::haskell_traits::rebind<__VA_ARGS__, ::haskell_traits::nonesuch2>,      \
                          ::haskell_traits::bound_to<__VA_ARGS__>>>,                               \
                  #__VA_ARGS__ " does not correctly implement monad_impl::mbind");                 \
    static_assert(::haskell_traits::monad<__VA_ARGS__>, #__VA_ARGS__ " does not satisfy monad")

#endif /* HASKELL_TRAITS_MONAD_HPP */