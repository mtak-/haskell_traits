#ifndef HASKELL_TRAITS_DETAIL_FUNC_HPP
#define HASKELL_TRAITS_DETAIL_FUNC_HPP

#include <haskell_traits/detail/meta.hpp>

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename F, typename = void>
    struct func_impl : private F
    {
        using underlying_t = F;

        template<typename... Args, REQUIRES(std::is_constructible_v<F, Args&&...>)>
        func_impl(Args&&... args) noexcept(std::is_nothrow_constructible_v<F, Args&&...>)
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
        func_impl(Args&&... args) noexcept(std::is_nothrow_constructible_v<F, Args&&...>)
            : f((Args &&) args...)
        {
        }

        template<typename... Args, REQUIRES(callable<F&, Args...>)>
            result_t<F&, Args&&...>
            operator()(Args&&... args) & noexcept(noexcept_callable<F&, Args&&...>)
        {
            return std::invoke(f, (Args &&) args...);
        }

        template<typename... Args, REQUIRES(callable<const F&, Args...>)>
        result_t<const F&, Args&&...>
        operator()(Args&&... args) const & noexcept(noexcept_callable<const F&, Args&&...>)
        {
            return std::invoke(f, (Args &&) args...);
        }

        template<typename... Args, REQUIRES(callable<F&&, Args...>)>
            result_t<F&&, Args&&...>
            operator()(Args&&... args) && noexcept(noexcept_callable<F&&, Args&&...>)
        {
            return std::invoke(std::move(f), (Args &&) args...);
        }

        template<typename... Args, REQUIRES(callable<const F&&, Args...>)>
        result_t<const F&&, Args&&...>
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
        using underlying_t = F;

        using detail::func_impl<F>::func_impl;
        using detail::func_impl<F>::operator();
    };

    template<typename F, REQUIRES(!instantiation_of<func, F>)>
    func(F && f)->func<uncvref<F>>;
    template<typename F, REQUIRES(instantiation_of<func, F>)>
    func(F && f)->func<typename uncvref<F>::underlying_t>;
HASKELL_TRAITS_END

#endif /* HASKELL_TRAITS_DETAIL_FUNC_HPP */