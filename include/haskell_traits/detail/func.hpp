#ifndef HASKELL_TRAITS_DETAIL_FUNC_HPP
#define HASKELL_TRAITS_DETAIL_FUNC_HPP

#include <haskell_traits/detail/meta.hpp>

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename F, typename = void>
    struct func_impl : private F
    {
        using underlying_t = F;

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
        using underlying_t = F;

        template<typename... Args, REQUIRES(std::is_constructible_v<F, Args&&...>)>
        constexpr func_impl(Args&&... args) noexcept(std::is_nothrow_constructible_v<F, Args&&...>)
            : f((Args &&) args...)
        {
        }

        // TODO: add support for finals class types taking explicit template parameters
        template<typename... Args, REQUIRES(callable<F&, Args...>)>
            constexpr result_t<F&, Args&&...>
            operator()(Args&&... args) & noexcept(noexcept_callable<F&, Args&&...>)
        {
            return std::invoke(f, (Args &&) args...);
        }

        template<typename... Args, REQUIRES(callable<const F&, Args...>)>
        constexpr result_t<const F&, Args&&...>
        operator()(Args&&... args) const & noexcept(noexcept_callable<const F&, Args&&...>)
        {
            return std::invoke(f, (Args &&) args...);
        }

        template<typename... Args, REQUIRES(callable<F&&, Args...>)>
            constexpr result_t<F&&, Args&&...>
            operator()(Args&&... args) && noexcept(noexcept_callable<F&&, Args&&...>)
        {
            return std::invoke(std::move(f), (Args &&) args...);
        }

        template<typename... Args, REQUIRES(callable<const F&&, Args...>)>
        constexpr result_t<const F&&, Args&&...>
        operator()(Args&&... args) const && noexcept(noexcept_callable<const F&&, Args&&...>)
        {
            return std::invoke(std::move(f), (Args &&) args...);
        }
    };
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename F>
    struct func : private detail::func_impl<F>
    {
        static_assert(!std::is_reference_v<F>);
        static_assert(!instantiation_of<haskell_traits::func, F>);

        using underlying_t = F;

        using detail::func_impl<F>::func_impl;
        using detail::func_impl<F>::operator();
    };

    template<typename F, REQUIRES(!instantiation_of<func, F>)>
    func(F && f)->func<uncvref<F>>;
    template<typename F, REQUIRES(instantiation_of<func, F>)>
    func(F && f)->func<typename uncvref<F>::underlying_t>;

    template<typename F>
    using func_t = decltype(func{std::declval<F>()});

    // template<typename F, REQUIRES(!instantiation_of<func, F>)>
    // constexpr func<uncvref<F>> make_func(F && f) NOEXCEPT_RETURNS(func<uncvref<F>>((F &&) f));
    //
    // template<typename F, REQUIRES(instantiation_of<func, F>)>
    // constexpr func<typename uncvref<F>::underlying_t> make_func(F && f)
    //     NOEXCEPT_RETURNS(func<typename uncvref<F>::underlying_t>((F &&) f));
    //
    // template<typename F>
    // using func_t = decltype(make_func(std::declval<F>()));

    template<typename... Funcs>
    struct merged : private Funcs...
    {
        static_assert((instantiation_of<func, Funcs> && ...));
        static_assert((!std::is_reference_v<Funcs> && ...));

        template<typename... Funcs_In, REQUIRES(sizeof...(Funcs) == sizeof...(Funcs_In))>
        constexpr merged(Funcs_In&&... funcs) noexcept(
            (std::is_nothrow_constructible_v<Funcs, Funcs_In&&> && ...))
            : Funcs((Funcs_In &&) funcs)...
        {
        }

        using Funcs::operator()...;
    };

    template<typename... Funcs>
    merged(Funcs && ...)->merged<func_t<Funcs&&>...>;

    template<typename... Funcs>
    using merged_t = decltype(merged{std::declval<Funcs>()...});
HASKELL_TRAITS_END

#endif /* HASKELL_TRAITS_DETAIL_FUNC_HPP */