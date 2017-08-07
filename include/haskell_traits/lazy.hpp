#ifndef HASKELL_TRAITS_LAZY_HPP
#define HASKELL_TRAITS_LAZY_HPP

#include <haskell_traits/detail/func.hpp>

#include <tuple>

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename F0, typename Expected, typename Tup, std::size_t... Is>
    constexpr auto call_(F0 && f,
                         expected_result<Expected> e,
                         Tup && tup,
                         std::integer_sequence<std::size_t, Is...>)
        DECLTYPE_NOEXCEPT_RETURNS(((F0 &&) f)(e, std::get<Is>((Tup &&) tup)...));

    template<typename F0, typename Expected, typename Tup>
    constexpr auto call(F0 && f, expected_result<Expected> e, Tup && tup) DECLTYPE_NOEXCEPT_RETURNS(
        detail::call_((F0 &&) f,
                      e,
                      (Tup &&) tup,
                      std::make_index_sequence<std::tuple_size<uncvref<Tup>>{}>{}));

    template<typename F, typename... Args>
    struct lazy_helper : private F, private std::tuple<Args...>
    {
    private:
        using tuple_type = std::tuple<Args...>;

        template<typename F0, typename Expected>
        using my_result_t = decltype(detail::call(std::declval<F0>(),
                                                  std::declval<expected_result<Expected>>(),
                                                  std::declval<as_same_cvref<tuple_type, F0>>()));

        template<typename F0, typename Expected>
        static inline constexpr auto my_callable = is_detected<my_result_t, F0, Expected>;

    public:
        using underlying_t = underlying_t<F>;

        template<typename F0, typename... Args0>
        using possible_results_t
            = possible_results_t<as_same_cvref<underlying_t, F0>, Args0..., Args...>;

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

#define HASKELL_TRAITS_CALL_OP(cvref)                                                              \
    template<typename Expected, REQUIRES(my_callable<F cvref, Expected>)>                          \
    constexpr my_result_t<F cvref, Expected> operator()(expected_result<Expected> e)               \
        cvref noexcept(                                                                            \
            noexcept(call(static_cast<F cvref>(*this), e, static_cast<tuple_type cvref>(*this))))  \
    {                                                                                              \
        return call(static_cast<F cvref>(*this), e, static_cast<tuple_type cvref>(*this));         \
    }                                                                                              \
        /**/
        HASKELL_TRAITS_CALL_OP(&)
        HASKELL_TRAITS_CALL_OP(&&)
        HASKELL_TRAITS_CALL_OP(const&)
        HASKELL_TRAITS_CALL_OP(const&&)
#undef HASKELL_TRAITS_CALL_OP
    };

    template<typename F, typename... Args, REQUIRES(!instantiation_of<lazy_helper, F>)>
    lazy_helper(F && f, Args && ... args)->lazy_helper<uncvref<F>, uncvref<Args>...>;

    template<typename F, typename... Args>
    lazy_helper(lazy_helper<F, Args...> && f)->lazy_helper<F, Args...>;

    template<typename F, typename... Args>
    lazy_helper(const lazy_helper<F, Args...>& f)->lazy_helper<F, Args...>;

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
    template<typename F, typename... Args>
    struct lazy : private detail::lazy_helper<func_t<F>, Args...>
    {
    private:
        using L = detail::lazy_helper<func_t<F>, Args...>;

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

        template<typename... Args_In, REQUIRES(std::is_constructible_v<L, Args_In&&...>)>
        constexpr lazy(Args_In&&... args) noexcept(std::is_nothrow_constructible_v<L, Args_In&&...>)
            : L((Args_In &&) args...)
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

    template<typename F, typename... Args, REQUIRES(!instantiation_of<lazy, F>)>
    lazy(F && f, Args && ... args)->lazy<std::decay_t<F>, uncvref<Args>...>;

    template<typename F, typename... Args>
    lazy(lazy<F, Args...> && f)->lazy<F, Args...>;

    template<typename F, typename... Args>
    lazy(const lazy<F, Args...>& f)->lazy<F, Args...>;

    template<typename F>
    using lazy_t = decltype(lazy{std::declval<F>()});

    template<typename F>
    struct lazy_func_return : private func_t<F>
    {
    private:
        using base = func_t<F>;

        template<typename B, typename... Args>
        using lazy_type = decltype(lazy(std::declval<B>(), std::declval<Args>()...));

        template<typename B, typename... Args>
        using strict_type = decltype(std::declval<B>()(std::declval<Args>()...));

    public:
        using underlying_t = underlying_t<func_t<F>>;

        template<typename F0, typename... Args>
        using possible_results_t = possible_results_t<as_same_cvref<underlying_t, F0>, Args...>;

        template<typename... Args, REQUIRES(std::is_constructible_v<base, Args&&...>)>
        constexpr lazy_func_return(Args&&... args) noexcept(
            std::is_nothrow_constructible_v<base, Args&&...>)
            : base((Args &&) args...)
        {
        }

#define HASKELL_TRAITS_CALL_OP(cvref)                                                              \
    template<typename... Args,                                                                     \
             REQUIRES(!callable<base cvref,                                                        \
                                Args&&...> && !empty<possible_results_t<base cvref, Args&&...>>)>  \
    constexpr lazy_type<base cvref, Args&&...> operator()(Args&&... args)                          \
        cvref NOEXCEPT_RETURNS(lazy(static_cast<base cvref>(*this), (Args &&) args...));           \
        /**/
        HASKELL_TRAITS_CALL_OP(&)
        HASKELL_TRAITS_CALL_OP(&&)
        HASKELL_TRAITS_CALL_OP(const&)
        HASKELL_TRAITS_CALL_OP(const&&)
#undef HASKELL_TRAITS_CALL_OP

#define HASKELL_TRAITS_CALL_OP(cvref)                                                              \
    template<typename... Args, REQUIRES(callable<base cvref, Args&&...>)>                          \
    constexpr strict_type<base cvref, Args&&...> operator()(Args&&... args) cvref                  \
                                                 NOEXCEPT_RETURNS(static_cast<base cvref> (*this)((Args &&) args...));
        /**/
        HASKELL_TRAITS_CALL_OP(&)
        HASKELL_TRAITS_CALL_OP(&&)
        HASKELL_TRAITS_CALL_OP(const&)
        HASKELL_TRAITS_CALL_OP(const&&)
#undef HASKELL_TRAITS_CALL_OP
    };

    template<typename F, REQUIRES(!instantiation_of<lazy_func_return, F>)>
    lazy_func_return(F && f)->lazy_func_return<std::decay_t<F>>;

    template<typename F>
    lazy_func_return(lazy_func_return<F> && f)->lazy_func_return<F>;

    template<typename F>
    lazy_func_return(const lazy_func_return<F>& f)->lazy_func_return<F>;

    template<typename F>
    using lazy_func_return_t = decltype(lazy_func_return(std::declval<F>()));

    template<typename... Fs>
    struct lazy_merged_return : private lazy_func_return_t<merged_return_t<Fs...>>
    {
    private:
        using base = lazy_func_return_t<merged_return_t<Fs...>>;

    public:
        template<typename... Fs_In,
                 REQUIRES(sizeof...(Fs_In) == sizeof...(Fs)),
                 REQUIRES(std::is_constructible_v<base, Fs_In&&...>)>
        constexpr lazy_merged_return(Fs_In&&... fs) noexcept(
            std::is_nothrow_constructible_v<base, Fs_In&&...>)
            : base((Fs_In &&) fs...)
        {
        }

        using base::operator();
    };

    template<typename... Fs,
             REQUIRES(sizeof...(Fs) != 1 || (!instantiation_of<lazy_merged_return, Fs> && ...))>
    lazy_merged_return(Fs && ... f)->lazy_merged_return<std::decay_t<Fs>...>;

    template<typename F>
    lazy_merged_return(lazy_merged_return<F> && f)->lazy_merged_return<F>;

    template<typename F>
    lazy_merged_return(const lazy_merged_return<F>& f)->lazy_merged_return<F>;
HASKELL_TRAITS_END

#endif /* HASKELL_TRAITS_LAZY_HPP */