#ifndef HASKELL_TRAITS_DETAIL_FUNC_HPP
#define HASKELL_TRAITS_DETAIL_FUNC_HPP

#include <haskell_traits/detail/meta.hpp>

HASKELL_TRAITS_DETAIL_BEGIN
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

        // TODO: add support for finals class types taking explicit template parameters
        template<typename... Args, REQUIRES(callable<F&, Args...>)>
            constexpr result_t<F&, Args&&...>
            operator()(Args&&... args) & noexcept(noexcept_callable<F&, Args&&...>)
        {
            return haskell_traits::invoke(f, (Args &&) args...);
        }

        template<typename... Args, REQUIRES(callable<const F&, Args...>)>
        constexpr result_t<const F&, Args&&...>
        operator()(Args&&... args) const & noexcept(noexcept_callable<const F&, Args&&...>)
        {
            return haskell_traits::invoke(f, (Args &&) args...);
        }

        template<typename... Args, REQUIRES(callable<F&&, Args...>)>
            constexpr result_t<F&&, Args&&...>
            operator()(Args&&... args) && noexcept(noexcept_callable<F&&, Args&&...>)
        {
            return haskell_traits::invoke(std::move(f), (Args &&) args...);
        }

        template<typename... Args, REQUIRES(callable<const F&&, Args...>)>
        constexpr result_t<const F&&, Args&&...>
        operator()(Args&&... args) const && noexcept(noexcept_callable<const F&&, Args&&...>)
        {
            return haskell_traits::invoke(std::move(f), (Args &&) args...);
        }
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
    using func_t = decltype(func{std::declval<F>()});

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

    template<typename Func, typename... Funcs>
    struct Constraint
    {
        template<typename Expected, typename F, typename... Args>
        static inline constexpr auto callable_count
            = (int(callable_returns<Expected, as_same_cvref<Funcs, F>, Args...>) + ... + 0)
              + (int(callable_returns<Expected,
                                      as_same_cvref<Funcs, F>,
                                      expected_result<Expected>,
                                      Args...>)
                 + ...
                 + 0);

        template<typename F, typename... Args>
        static inline constexpr auto callable_count_strict
            = (int(callable<as_same_cvref<Funcs, F>, Args...>) + ... + 0);

        template<typename Expected, typename F, typename... Args>
        static inline constexpr auto can_call
            = callable_count<Expected, F, Args...> == 1
              && callable_returns<Expected, as_same_cvref<Func, F>, Args...>;

        template<typename Expected, typename F, typename... Args>
        static inline constexpr auto can_call_expected
            = callable_count<Expected, F, Args...> == 1
              && callable_returns<Expected,
                                  as_same_cvref<Func, F>,
                                  expected_result<Expected>,
                                  Args...>;

        template<typename F, typename... Args>
        static inline constexpr auto can_call_strict
            = callable_count_strict<F, Args...> == 1 && callable<as_same_cvref<Func, F>, Args...>;
    };

    template<typename Constraint, typename Expected, typename F, typename... Args>
    inline constexpr auto constrained_can_call
        = Constraint::template can_call<Expected, F, Args...>;

    template<typename Constraint, typename Expected, typename F, typename... Args>
    inline constexpr auto constrained_can_call_expected
        = Constraint::template can_call_expected<Expected, F, Args...>;

    template<typename Constraint, typename F, typename... Args>
    inline constexpr auto constrained_can_call_strict
        = Constraint::template can_call_strict<F, Args...>;

    // TODO: doesn't quite pick the correct ref qualified member functions
    // just because callable_count_ > 1 doesn't mean that there's ambiguity
    // deciding which function to call is hard tho...
    template<typename F, typename Constraint>
    struct overload_return_member : private F
    {
        static_assert(is_func<F>);

        using F::F;

        template<typename Expected,
                 typename... Args,
                 REQUIRES(constrained_can_call<Constraint, Expected, F&, Args&&...>)>
            constexpr result_t<F&, Args&&...> operator()(expected_result<Expected>, Args&&... args)
            & noexcept(noexcept(Expected(static_cast<F&> (*this)((Args &&) args...))))
        {
            return static_cast<F&>(*this)((Args &&) args...);
        }

        template<typename Expected,
                 typename... Args,
                 REQUIRES(constrained_can_call<Constraint, Expected, F&&, Args&&...>)>
            constexpr result_t<F&&, Args&&...> operator()(expected_result<Expected>, Args&&... args)
            && noexcept(noexcept(Expected(std::move(static_cast<F&>(*this))((Args &&) args...))))
        {
            return std::move(static_cast<F&>(*this))((Args &&) args...);
        }

        template<typename Expected,
                 typename... Args,
                 REQUIRES(constrained_can_call<Constraint, Expected, const F&, Args&&...>)>
        constexpr result_t<const F&, Args&&...>
        operator()(expected_result<Expected>, Args&&... args) const & noexcept(
            noexcept(Expected(static_cast<const F&> (*this)((Args &&) args...))))
        {
            return static_cast<const F&>(*this)((Args &&) args...);
        }

        template<typename Expected,
                 typename... Args,
                 REQUIRES(constrained_can_call<Constraint, Expected, const F&&, Args&&...>)>
        constexpr result_t<const F&&, Args&&...>
        operator()(expected_result<Expected>, Args&&... args) const && noexcept(
            noexcept(Expected(std::move(static_cast<const F&>(*this))((Args &&) args...))))
        {
            return std::move(static_cast<const F&>(*this))((Args &&) args...);
        }

        template<typename Expected,
                 typename... Args,
                 REQUIRES(constrained_can_call_expected<Constraint, Expected, F&, Args&&...>)>
            constexpr result_t<F&, expected_result<Expected>, Args&&...>
            operator()(expected_result<Expected> e, Args&&... args)
            & noexcept(noexcept(Expected(static_cast<F&> (*this)(e, (Args &&) args...))))
        {
            return static_cast<F&>(*this)(e, (Args &&) args...);
        }

        template<typename Expected,
                 typename... Args,
                 REQUIRES(constrained_can_call_expected<Constraint, Expected, F&&, Args&&...>)>
            constexpr result_t<F&&, expected_result<Expected>, Args&&...>
            operator()(expected_result<Expected> e, Args&&... args)
            && noexcept(noexcept(Expected(std::move(static_cast<F&>(*this))(e, (Args &&) args...))))
        {
            return std::move(static_cast<F&>(*this))(e, (Args &&) args...);
        }

        template<typename Expected,
                 typename... Args,
                 REQUIRES(constrained_can_call_expected<Constraint, Expected, const F&, Args&&...>)>
        constexpr result_t<const F&, expected_result<Expected>, Args&&...>
        operator()(expected_result<Expected> e, Args&&... args) const & noexcept(
            noexcept(Expected(static_cast<const F&> (*this)(e, (Args &&) args...))))
        {
            return static_cast<const F&>(*this)(e, (Args &&) args...);
        }

        template<
            typename Expected,
            typename... Args,
            REQUIRES(constrained_can_call_expected<Constraint, Expected, const F&&, Args&&...>)>
        constexpr result_t<const F&&, expected_result<Expected>, Args&&...>
        operator()(expected_result<Expected> e, Args&&... args) const && noexcept(
            noexcept(Expected(std::move(static_cast<const F&>(*this))(e, (Args &&) args...))))
        {
            return std::move(static_cast<const F&>(*this))(e, (Args &&) args...);
        }

        template<typename... Args, REQUIRES(constrained_can_call_strict<Constraint, F&, Args&&...>)>
            constexpr result_t<F&, Args&&...> operator()(Args&&... args)
            & noexcept(noexcept(static_cast<F&> (*this)((Args &&) args...)))
        {
            return static_cast<F&>(*this)((Args &&) args...);
        }

        template<typename... Args,
                 REQUIRES(constrained_can_call_strict<Constraint, F&&, Args&&...>)>
            constexpr result_t<F&&, Args&&...> operator()(Args&&... args)
            && noexcept(noexcept(std::move(static_cast<F&>(*this))((Args &&) args...)))
        {
            return std::move(static_cast<F&>(*this))((Args &&) args...);
        }

        template<typename... Args,
                 REQUIRES(constrained_can_call_strict<Constraint, const F&, Args&&...>)>
        constexpr result_t<const F&, Args&&...> operator()(Args&&... args) const & noexcept(
            noexcept(static_cast<const F&> (*this)((Args &&) args...)))
        {
            return static_cast<const F&>(*this)((Args &&) args...);
        }

        template<typename... Args,
                 REQUIRES(constrained_can_call_strict<Constraint, const F&&, Args&&...>)>
        constexpr result_t<const F&&, Args&&...> operator()(Args&&... args) const && noexcept(
            noexcept(std::move(static_cast<const F&>(*this))((Args &&) args...)))
        {
            return std::move(static_cast<const F&>(*this))((Args &&) args...);
        }
    };
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename... Funcs>
    struct overload_return
        : private detail::overload_return_member<Funcs, detail::Constraint<Funcs, Funcs...>>...
    {
        template<typename... Funcs_In,
                 REQUIRES((std::is_constructible_v<Funcs, Funcs_In&&> && ...))>
        constexpr overload_return(Funcs_In&&... fs) noexcept(
            (std::is_nothrow_constructible_v<Funcs, Funcs_In&&> && ...))
            : detail::overload_return_member<Funcs, detail::Constraint<Funcs, Funcs...>>(
                  (Funcs_In &&) fs)...
        {
        }

        using detail::overload_return_member<Funcs, detail::Constraint<Funcs, Funcs...>>::
        operator()...;
    };

    template<typename... Fs>
    overload_return(Fs && ... fs)->overload_return<func_t<uncvref<Fs>>...>;

    template<typename... Fs>
    using overload_return_t = decltype(func{std::declval<Fs>()...});
HASKELL_TRAITS_END

#endif /* HASKELL_TRAITS_DETAIL_FUNC_HPP */