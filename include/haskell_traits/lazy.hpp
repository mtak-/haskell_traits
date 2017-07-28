#ifndef HASKELL_TRAITS_LAZY_HPP
#define HASKELL_TRAITS_LAZY_HPP

#include <haskell_traits/detail/func.hpp>

HASKELL_TRAITS_DETAIL_BEGIN
    template<typename L>
    using lazy_result_t = result_t<L>;

    template<typename L, typename... TemplateArgs>
    using lazy_result_template_t
        = decltype(std::declval<L>().template operator()<TemplateArgs...>());

    template<typename L, typename... TemplateArgs>
    inline constexpr auto lazy_template = is_detected<lazy_result_template_t, L, TemplateArgs...>;
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

    // template<typename F, REQUIRES(!instantiation_of<lazy, F>)>
    // constexpr lazy<uncvref<F>> make_lazy(F && f) NOEXCEPT_RETURNS(lazy<uncvref<F>>((F &&) f));
    //
    // template<typename F, REQUIRES(instantiation_of<lazy, F>)>
    // constexpr lazy<typename uncvref<F>::underlying_t> make_lazy(F && f)
    //     NOEXCEPT_RETURNS(lazy<typename uncvref<F>::underlying_t>((F &&) f));
    //
    // template<typename F>
    // using lazy_t = decltype(make_lazy(std::declval<F>()));

    template<typename T, typename U>
    inline constexpr auto is_lazy_same = std::bool_constant<
        std::is_same_v<uncvref<T>,
                       uncvref<U>> || (instantiation_of<lazy, T> && std::is_convertible_v<T, U>)>{};
HASKELL_TRAITS_END

#endif /* HASKELL_TRAITS_LAZY_HPP */