#ifndef HASKELL_TRAITS_LAZY_HPP
#define HASKELL_TRAITS_LAZY_HPP

#include <haskell_traits/detail/func.hpp>

#include <tuple>

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename From, typename To>
    struct is_convertible_no_rvalue_to_lvalue
        : std::bool_constant<
              std::is_same_v<
                  uncvref<From>,
                  uncvref<
                      To>> && std::is_convertible_v<From, To> && !(std::is_rvalue_reference_v<std::add_rvalue_reference_t<From>> && std::is_lvalue_reference_v<To>)&&!(!std::is_reference_v<From> && std::is_reference_v<To>)>
    {
    };

    template<typename From, typename To>
    inline constexpr auto is_convertible_no_rvalue_to_lvalue_v
        = _t<is_convertible_no_rvalue_to_lvalue<From, To>>{};
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename L>
    struct lazy : private L
    {
        static_assert(!std::is_reference_v<L>);
        static_assert(!instantiation_of<haskell_traits::lazy, L>);

    private:
        using lvalue_ref       = L&;
        using const_lvalue_ref = const L&;
        using rvalue_ref       = L&&;
        using const_rvalue_ref = const L&&;

        template<typename T, typename ref_type>
        static inline constexpr auto can_call_overload = std::bool_constant<
            callable<
                ref_type,
                expected_result<
                    T>> && detail::is_convertible_no_rvalue_to_lvalue_v<detected_or<nonesuch, result_t, ref_type, expected_result<T>>, T>>{};

        // clang-format off
        // clang format really sucks here
        template<typename T, typename ref_type>
        static inline constexpr auto                   can_call_overload_
            = std::bool_constant<callable<ref_type, expected_result<T>>&& detail::is_convertible_no_rvalue_to_lvalue_v<
                  detected_or<nonesuch, result_t, ref_type, expected_result<T>>,
                  T> && !std::is_reference_v<T> && !std::is_reference_v<detected_or<nonesuch&, result_t, ref_type, expected_result<T>>> && !std::is_const_v<T>>{};
        // clang-format on

    public:
        using underlying_t = L;

        template<typename LIn, REQUIRES(std::is_constructible_v<L, LIn&&>)>
        constexpr lazy(LIn&& lin) noexcept(std::is_nothrow_constructible_v<L, LIn&&>)
            : L((LIn &&) lin)
        {
        }

#define HASKELL_TRAITS_CALL_OP(trait, t_cvref, cvref)                                              \
    template<typename T, REQUIRES(trait<T t_cvref, L cvref>)>                                      \
    constexpr operator T t_cvref() cvref noexcept(                                                 \
        noexcept(static_cast<T t_cvref>(std::declval<L cvref>()(expected_result<T t_cvref>{}))))   \
    {                                                                                              \
        return static_cast<L cvref>(*this)(expected_result<T t_cvref>{});                          \
    }                                                                                              \
/**/
#define HASKELL_TRAITS_CALL_OP2(trait, t_cvref)                                                    \
    HASKELL_TRAITS_CALL_OP(trait, t_cvref, &)                                                      \
    HASKELL_TRAITS_CALL_OP(trait, t_cvref, &&)                                                     \
    HASKELL_TRAITS_CALL_OP(trait, t_cvref, const&)                                                 \
    HASKELL_TRAITS_CALL_OP(trait, t_cvref, const&&)                                                \
        /**/

        HASKELL_TRAITS_CALL_OP2(can_call_overload_, /**/)
        HASKELL_TRAITS_CALL_OP2(can_call_overload, &)
        HASKELL_TRAITS_CALL_OP2(can_call_overload, &&)
        HASKELL_TRAITS_CALL_OP2(can_call_overload, const&)
        HASKELL_TRAITS_CALL_OP2(can_call_overload, const&&)
#undef HASKELL_TRAITS_CALL_OP2
#undef HASKELL_TRAITS_CALL_OP
    };

    template<typename F, REQUIRES(!instantiation_of<lazy, F>)>
    lazy(F && f)->lazy<uncvref<F>>;
    template<typename F, REQUIRES(instantiation_of<lazy, F>)>
    lazy(F && f)->lazy<typename uncvref<F>::underlying_t>;

    template<typename F>
    using lazy_t = decltype(lazy{std::declval<F>()});

    template<typename F, typename... Args>
    struct lazy_helper : private F, private std::tuple<Args...>
    {
        static_assert(sizeof...(Args) > 0);

    private:
        using tuple_type = std::tuple<Args...>;

        static inline constexpr auto iseq = std::make_index_sequence<sizeof...(Args)>{};

        template<typename F0, typename Expected, typename Tup, std::size_t... Is>
        static constexpr auto call_(F0&&                      f,
                                    expected_result<Expected> e,
                                    Tup&&                     tup,
                                    std::integer_sequence<std::size_t, Is...>)
            DECLTYPE_NOEXCEPT_RETURNS(((F0 &&) f)(e, std::get<Is>((Tup &&) tup)...));

        template<typename F0, typename Expected, typename Tup>
        static constexpr auto call(F0&& f, expected_result<Expected> e, Tup&& tup)
            DECLTYPE_NOEXCEPT_RETURNS(call_((F0 &&) f, e, (Tup &&) tup, iseq));

        template<typename F0, typename Expected>
        using my_result_t = decltype(call(std::declval<F0>(),
                                          std::declval<expected_result<Expected>>(),
                                          std::declval<as_same_cvref<tuple_type, F0>>()));

        template<typename F0, typename Expected>
        static inline constexpr auto my_callable = is_detected<my_result_t, F0, Expected>;

    public:
        template<typename F_In,
                 typename... Args_In,
                 REQUIRES(std::is_constructible_v<
                              F,
                              F_In&&> && (std::is_constructible_v<Args, Args_In&&> && ...))>
        constexpr lazy_helper(F_In&& f, Args_In&&... args) noexcept(
            std::is_nothrow_constructible_v<
                F,
                F_In&&> && (std::is_nothrow_constructible_v<Args, Args_In&&> && ...))
            : F((F_In &&) f)
            , tuple_type((Args_In &&) args...)
        {
        }

        template<typename Expected>
            constexpr my_result_t<F&, Expected> operator()(expected_result<Expected> e)
            & noexcept(noexcept(call(static_cast<F&>(*this), e, static_cast<tuple_type&>(*this))))
        {
            return call(static_cast<F&>(*this), e, static_cast<tuple_type&>(*this));
        }

        template<typename Expected>
            constexpr my_result_t<F&&, Expected> operator()(expected_result<Expected> e)
            && noexcept(noexcept(call(std::move(static_cast<F&>(*this)),
                                      e,
                                      std::move(static_cast<tuple_type&>(*this)))))
        {
            return call(std::move(static_cast<F&>(*this)),
                        e,
                        std::move(static_cast<tuple_type&>(*this)));
        }

        template<typename Expected>
        constexpr my_result_t<const F&, Expected>
        operator()(expected_result<Expected> e) const & noexcept(
            noexcept(call(static_cast<const F&>(*this), e, static_cast<const tuple_type&>(*this))))
        {
            return call(static_cast<const F&>(*this), e, static_cast<const tuple_type&>(*this));
        }

        template<typename Expected>
        constexpr my_result_t<const F&&, Expected> operator()(expected_result<Expected> e)
            const && noexcept(noexcept(call(std::move(static_cast<const F&>(*this)),
                                            e,
                                            std::move(static_cast<const tuple_type&>(*this)))))
        {
            return call(std::move(static_cast<const F&>(*this)),
                        e,
                        std::move(static_cast<const tuple_type&>(*this)));
        }
    };

    template<typename F, typename... Args, REQUIRES(sizeof...(Args) > 0)>
    lazy_helper(F && f, Args && ... args)->lazy_helper<uncvref<F>, uncvref<Args>...>;

    template<typename... Funcs>
    struct lazy_overload_return : private merged_return_t<Funcs...>
    {
    private:
        using base = merged_return_t<Funcs...>;

        template<typename B, typename... Args>
        using lazy_type = decltype(lazy(lazy_helper(std::declval<B>(), std::declval<Args>()...)));

        template<typename B, typename... Args>
        using strict_type = decltype(std::declval<B>()(std::declval<Args>()...));

    public:
        using merged_return_t<Funcs...>::merged::merged;

        template<typename... Args, REQUIRES(!callable<base&, Args&&...>)>
            constexpr lazy_type<base&, Args&&...> operator()(Args&&... args)
            & NOEXCEPT_RETURNS(lazy(lazy_helper(static_cast<base&>(*this), (Args &&) args...)));

        template<typename... Args, REQUIRES(!callable<base&&, Args&&...>)>
            constexpr lazy_type<base&&, Args&&...> operator()(Args&&... args)
            && NOEXCEPT_RETURNS(lazy(lazy_helper(std::move(static_cast<base&>(*this)),
                                                 (Args &&) args...)));

        template<typename... Args, REQUIRES(!callable<const base&, Args&&...>)>
        constexpr lazy_type<const base&, Args&&...>
        operator()(Args&&... args) const & NOEXCEPT_RETURNS(
            lazy(lazy_helper(static_cast<const base&>(*this), (Args &&) args...)));

        template<typename... Args, REQUIRES(!callable<const base&&, Args&&...>)>
        constexpr lazy_type<const base&&, Args&&...>
        operator()(Args&&... args) const && NOEXCEPT_RETURNS(
            lazy(lazy_helper(std::move(static_cast<const base&>(*this)), (Args &&) args...)));

        template<typename... Args, REQUIRES(callable<base&, Args&&...>)>
            constexpr strict_type<base&, Args&&...> operator()(Args&&... args)
            & NOEXCEPT_RETURNS(static_cast<base&> (*this)((Args &&) args...));

        template<typename... Args, REQUIRES(callable<base&&, Args&&...>)>
            constexpr strict_type<base&&, Args&&...> operator()(Args&&... args)
            && NOEXCEPT_RETURNS(std::move(static_cast<base&>(*this))((Args &&) args...));

        template<typename... Args, REQUIRES(callable<const base&, Args&&...>)>
        constexpr strict_type<const base&, Args&&...> operator()(Args&&... args)
            const & NOEXCEPT_RETURNS(static_cast<const base&> (*this)((Args &&) args...));

        template<typename... Args, REQUIRES(callable<const base&&, Args&&...>)>
        constexpr strict_type<const base&&, Args&&...>
        operator()(Args&&... args) const && NOEXCEPT_RETURNS(
            std::move(static_cast<const base&>(*this))((Args &&) args...));
    };

    template<typename... Fs>
    lazy_overload_return(Fs && ... fs)->lazy_overload_return<func_t<Fs>...>;
HASKELL_TRAITS_END

#endif /* HASKELL_TRAITS_LAZY_HPP */