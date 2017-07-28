#ifndef HASKELL_TRAITS_LAZY_HPP
#define HASKELL_TRAITS_LAZY_HPP

#include <haskell_traits/detail/func.hpp>

#include <tuple>

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename L, typename TemplateArgs, typename Args, typename = void>
    struct result_template;
    template<typename L, typename... TemplateArgs, typename... Args>
    struct result_template<
        L,
        type_list<TemplateArgs...>,
        type_list<Args...>,
        std::void_t<decltype(
            std::declval<L>().template operator()<TemplateArgs...>(std::declval<Args>()...))>>
    {
        using type = decltype(
            std::declval<L>().template operator()<TemplateArgs...>(std::declval<Args>()...));
    };

    template<typename L, typename TemplateArgs, typename Args>
    using result_template_t = _t<result_template<L, TemplateArgs, Args>>;

    template<typename L, typename TemplateArgs, typename Args>
    inline constexpr auto callable_template = is_detected<result_template_t, L, TemplateArgs, Args>;

    template<typename L>
    using lazy_result_t = result_t<L>;

    template<typename L, typename... TemplateArgs>
    using lazy_result_template_t = result_template_t<L, type_list<TemplateArgs...>, type_list<>>;

    template<typename L, typename... TemplateArgs>
    inline constexpr auto lazy_template = is_detected<lazy_result_template_t, L, TemplateArgs...>;

    struct apply_fn
    {
    private:
        template<typename... TemplateExtras, typename F, typename Tup, std::size_t... Is>
        static auto call(F&& f, Tup&& tup, std::integer_sequence<std::size_t, Is...>)
            DECLTYPE_NOEXCEPT_RETURNS(static_cast<F&&>(f).template operator()<TemplateExtras...>(
                std::get<Is>((Tup &&) tup)...));

    public:
        template<typename... TemplateExtras, typename F, typename Tup>
        constexpr auto operator()(F&& f, Tup&& tup) const
            DECLTYPE_NOEXCEPT_RETURNS(apply_fn::call<TemplateExtras...>(
                (F &&) f,
                (Tup &&) tup,
                std::make_index_sequence<std::tuple_size<uncvref<Tup>>{}>{}));
    } inline constexpr apply{};
HASKELL_TRAITS_DETAIL_END

HASKELL_TRAITS_BEGIN
    template<typename L>
    struct lazy : private func_t<L>
    {
        static_assert(!std::is_reference_v<L>);
        static_assert(!instantiation_of<haskell_traits::lazy, L>);

    private:
        using lvalue_ref       = func_t<L>&;
        using const_lvalue_ref = const func_t<L>&;
        using rvalue_ref       = func_t<L>&&;
        using const_rvalue_ref = const func_t<L>&&;

    public:
        using underlying_t = L;

        using func_t<L>::func_t;

        template<typename T,
                 REQUIRES(callable<lvalue_ref>&& std::
                              is_convertible_v<detected_or<nonesuch, result_t, lvalue_ref>, T>)>
            constexpr operator T() & noexcept(noexcept(T(std::declval<lvalue_ref>()())))
        {
            return static_cast<lvalue_ref>(*this)();
        }

        template<typename T,
                 REQUIRES(callable<rvalue_ref>&& std::
                              is_convertible_v<detected_or<nonesuch, result_t, rvalue_ref>, T>)>
            constexpr operator T() && noexcept(noexcept(T(std::declval<rvalue_ref>()())))
        {
            return std::move(static_cast<lvalue_ref>(*this))();
        }

        template<
            typename T,
            REQUIRES(callable<const_lvalue_ref>&& std::
                         is_convertible_v<detected_or<nonesuch, result_t, const_lvalue_ref>, T>)>
        constexpr operator T() const & noexcept(noexcept(T(std::declval<const_lvalue_ref>()())))
        {
            return static_cast<const_lvalue_ref>(*this)();
        }

        template<
            typename T,
            REQUIRES(callable<const_rvalue_ref>&& std::
                         is_convertible_v<detected_or<nonesuch, result_t, const_rvalue_ref>, T>)>
        constexpr operator T() const && noexcept(noexcept(T(std::declval<const_rvalue_ref>()())))
        {
            return std::move(static_cast<const_lvalue_ref>(*this))();
        }

        template<typename T,
                 REQUIRES(!callable<lvalue_ref>),
                 REQUIRES(detail::lazy_template<lvalue_ref, T>&& std::is_convertible_v<
                          detected_or<nonesuch, detail::lazy_result_template_t, lvalue_ref, T>,
                          T>)>
            constexpr operator T()
            & noexcept(noexcept(T(std::declval<lvalue_ref>().template operator()<T>())))
        {
            return static_cast<lvalue_ref>(*this).template operator()<T>();
        }

        template<typename T,
                 REQUIRES(!callable<rvalue_ref>),
                 REQUIRES(detail::lazy_template<rvalue_ref, T>&& std::is_convertible_v<
                          detected_or<nonesuch, detail::lazy_result_template_t, rvalue_ref, T>,
                          T>)>
            constexpr operator T()
            && noexcept(noexcept(T(std::declval<rvalue_ref>().template operator()<T>())))
        {
            return std::move(static_cast<lvalue_ref>(*this)).template operator()<T>();
        }

        template<
            typename T,
            REQUIRES(!callable<const_lvalue_ref>),
            REQUIRES(detail::lazy_template<const_lvalue_ref, T>&& std::is_convertible_v<
                     detected_or<nonesuch, detail::lazy_result_template_t, const_lvalue_ref, T>,
                     T>)>
        constexpr operator T() const & noexcept(
            noexcept(T(std::declval<const_lvalue_ref>().template operator()<T>())))
        {
            return static_cast<const_lvalue_ref>(*this).template operator()<T>();
        }

        template<
            typename T,
            REQUIRES(!callable<const_rvalue_ref>),
            REQUIRES(detail::lazy_template<const_rvalue_ref, T>&& std::is_convertible_v<
                     detected_or<nonesuch, detail::lazy_result_template_t, const_rvalue_ref, T>,
                     T>)>
        constexpr operator T() const && noexcept(
            noexcept(T(std::declval<const_rvalue_ref>().template operator()<T>())))
        {
            return std::move(static_cast<const_lvalue_ref>(*this)).template operator()<T>();
        }
    };

    template<typename F, REQUIRES(!instantiation_of<lazy, F>)>
    lazy(F && f)->lazy<uncvref<F>>;
    template<typename F, REQUIRES(instantiation_of<lazy, F>)>
    lazy(F && f)->lazy<typename uncvref<F>::underlying_t>;

    template<typename F>
    using lazy_t = decltype(lazy{std::declval<F>()});

    template<typename L>
    struct func_return : private func_t<L>
    {
        static_assert(!std::is_reference_v<L>);
        static_assert(!instantiation_of<haskell_traits::func_return, L>);

    private:
        using lvalue_ref       = func_t<L>&;
        using const_lvalue_ref = const func_t<L>&;
        using rvalue_ref       = func_t<L>&&;
        using const_rvalue_ref = const func_t<L>&&;

    public:
        using underlying_t = L;

        using func_t<L>::func_t;

        template<typename T,
                 typename... Args,
                 REQUIRES(callable<lvalue_ref, Args&&...>&& std::is_convertible_v<
                          detected_or<nonesuch, result_t, lvalue_ref, Args&&...>,
                          T>)>
            constexpr result_t<lvalue_ref, Args&&...> operator()(Args&&... args)
            & noexcept(noexcept(T(std::declval<lvalue_ref>()((Args &&) args...))))
        {
            return static_cast<lvalue_ref>(*this)((Args &&) args...);
        }

        template<typename T,
                 typename... Args,
                 REQUIRES(callable<rvalue_ref, Args&&...>&& std::is_convertible_v<
                          detected_or<nonesuch, result_t, rvalue_ref, Args&&...>,
                          T>)>
            constexpr result_t<rvalue_ref, Args&&...> operator()(Args&&... args)
            && noexcept(noexcept(T(std::declval<rvalue_ref>()((Args &&) args...))))
        {
            return std::move(static_cast<lvalue_ref>(*this))((Args &&) args...);
        }

        template<typename T,
                 typename... Args,
                 REQUIRES(callable<const_lvalue_ref, Args&&...>&& std::is_convertible_v<
                          detected_or<nonesuch, result_t, const_lvalue_ref, Args&&...>,
                          T>)>
        constexpr result_t<const_lvalue_ref, Args&&...> operator()(Args&&... args) const & noexcept(
            noexcept(T(std::declval<const_lvalue_ref>()((Args &&) args...))))
        {
            return static_cast<const_lvalue_ref>(*this)((Args &&) args...);
        }

        template<typename T,
                 typename... Args,
                 REQUIRES(callable<const_rvalue_ref, Args&&...>&& std::is_convertible_v<
                          detected_or<nonesuch, result_t, const_rvalue_ref, Args&&...>,
                          T>)>
        constexpr result_t<const_rvalue_ref, Args&&...> operator()(Args&&... args)
            const && noexcept(noexcept(T(std::declval<const_rvalue_ref>()((Args &&) args...))))
        {
            return std::move(static_cast<const_lvalue_ref>(*this))((Args &&) args...);
        }

        template<
            typename T,
            typename... Args,
            REQUIRES(!callable<lvalue_ref, Args&&...>),
            REQUIRES(detail::callable_template<lvalue_ref, type_list<T>, type_list<Args&&...>>&&
                         std::is_convertible_v<detected_or<nonesuch,
                                                           detail::result_template_t,
                                                           lvalue_ref,
                                                           type_list<T>,
                                                           type_list<Args&&...>>,
                                               T>)>
            constexpr detail::result_template_t<lvalue_ref, type_list<T>, type_list<Args&&...>>
            operator()(Args&&... args)
            & noexcept(noexcept(T(std::declval<lvalue_ref>().template operator()<T>((Args &&)
                                                                                        args...))))
        {
            return static_cast<lvalue_ref>(*this).template operator()<T>((Args &&) args...);
        }

        template<
            typename T,
            typename... Args,
            REQUIRES(!callable<rvalue_ref, Args&&...>),
            REQUIRES(detail::callable_template<rvalue_ref, type_list<T>, type_list<Args&&...>>&&
                         std::is_convertible_v<detected_or<nonesuch,
                                                           detail::result_template_t,
                                                           rvalue_ref,
                                                           type_list<T>,
                                                           type_list<Args&&...>>,
                                               T>)>
            constexpr detail::result_template_t<rvalue_ref, type_list<T>, type_list<Args&&...>>
            operator()(Args&&... args)
            && noexcept(noexcept(T(std::declval<rvalue_ref>().template operator()<T>((Args &&)
                                                                                         args...))))
        {
            return std::move(static_cast<lvalue_ref>(*this))
                .template operator()<T>((Args &&) args...);
        }

        template<
            typename T,
            typename... Args,
            REQUIRES(!callable<const_lvalue_ref, Args&&...>),
            REQUIRES(
                detail::callable_template<const_lvalue_ref, type_list<T>, type_list<Args&&...>>&&
                    std::is_convertible_v<detected_or<nonesuch,
                                                      detail::result_template_t,
                                                      const_lvalue_ref,
                                                      type_list<T>,
                                                      type_list<Args&&...>>,
                                          T>)>
        constexpr detail::result_template_t<const_lvalue_ref, type_list<T>, type_list<Args&&...>>
        operator()(Args&&... args) const & noexcept(
            noexcept(T(std::declval<const_lvalue_ref>().template operator()<T>((Args &&) args...))))
        {
            return static_cast<const_lvalue_ref>(*this).template operator()<T>((Args &&) args...);
        }

        template<
            typename T,
            typename... Args,
            REQUIRES(!callable<const_rvalue_ref, Args&&...>),
            REQUIRES(
                detail::callable_template<const_rvalue_ref, type_list<T>, type_list<Args&&...>>&&
                    std::is_convertible_v<detected_or<nonesuch,
                                                      detail::result_template_t,
                                                      const_rvalue_ref,
                                                      type_list<T>,
                                                      type_list<Args&&...>>,
                                          T>)>
        constexpr detail::result_template_t<const_rvalue_ref, type_list<T>, type_list<Args&&...>>
        operator()(Args&&... args) const && noexcept(
            noexcept(T(std::declval<const_rvalue_ref>().template operator()<T>((Args &&) args...))))
        {
            return std::move(static_cast<const_lvalue_ref>(*this))
                .template operator()<T>((Args &&) args...);
        }
    };

    template<typename F, REQUIRES(!instantiation_of<func_return, F>)>
    func_return(F && f)->func_return<uncvref<F>>;
    template<typename F, REQUIRES(instantiation_of<func_return, F>)>
    func_return(F && f)->func_return<typename uncvref<F>::underlying_t>;

    template<typename F, typename... Args>
    struct lazy_helper : private func_t<F>, private std::tuple<Args...>
    {
        static_assert(sizeof...(Args) > 0);

    private:
        using args_list  = type_list<Args...>;
        using func_type  = func_t<F>;
        using tuple_type = std::tuple<Args...>;

    public:
        template<typename F_In,
                 typename... Args_In,
                 REQUIRES(std::is_constructible_v<
                              F,
                              F_In&&> && (std::is_constructible_v<Args, Args_In&&> && ...))>
        constexpr lazy_helper(F_In&& f, Args_In&&... args) noexcept(
            std::is_nothrow_constructible_v<
                func_type,
                F_In&&> && (std::is_nothrow_constructible_v<Args, Args_In&&> && ...))
            : func_type((F_In &&) f)
            , tuple_type((Args_In &&) args...)
        {
        }

        template<
            typename... TemplateExtras,
            REQUIRES(
                detail::callable_template<func_type&, type_list<TemplateExtras...>, args_list>)>
            constexpr detail::result_template_t<func_type&, type_list<TemplateExtras...>, args_list>
            operator()()
            & NOEXCEPT_RETURNS(detail::apply.template operator()<TemplateExtras...>(
                  static_cast<func_type&>(*this),
                  static_cast<tuple_type&>(*this)));

        template<typename... TemplateExtras,
                 REQUIRES(detail::callable_template<const func_type&,
                                                    type_list<TemplateExtras...>,
                                                    args_list>)>
        constexpr detail::result_template_t<const func_type&,
                                            type_list<TemplateExtras...>,
                                            args_list>
        operator()() const & NOEXCEPT_RETURNS(detail::apply.template operator()<TemplateExtras...>(
            static_cast<const func_type&>(*this),
            static_cast<const tuple_type&>(*this)));

        template<
            typename... TemplateExtras,
            REQUIRES(
                detail::callable_template<func_type&&, type_list<TemplateExtras...>, args_list>)>
            constexpr detail::
                result_template_t<func_type&&, type_list<TemplateExtras...>, args_list>
                operator()()
            && NOEXCEPT_RETURNS(detail::apply.template operator()<TemplateExtras...>(
                   static_cast<func_type&&>(std::move(*this)),
                   static_cast<tuple_type&&>(std::move(*this))));

        template<typename... TemplateExtras,
                 REQUIRES(detail::callable_template<const func_type&&,
                                                    type_list<TemplateExtras...>,
                                                    args_list>)>
        constexpr detail::result_template_t<const func_type&&,
                                            type_list<TemplateExtras...>,
                                            args_list>
        operator()() const && NOEXCEPT_RETURNS(detail::apply.template operator()<TemplateExtras...>(
            static_cast<const func_type&&>(std::move(*this)),
            static_cast<const tuple_type&&>(std::move(*this))));
    };

    template<typename F, typename... Args, REQUIRES(sizeof...(Args) > 0)>
    lazy_helper(F && f, Args && ... args)->lazy_helper<uncvref<F>, uncvref<Args>...>;
HASKELL_TRAITS_END

#endif /* HASKELL_TRAITS_LAZY_HPP */