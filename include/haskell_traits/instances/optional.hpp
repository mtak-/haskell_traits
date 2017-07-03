#ifndef HASKELL_TRAITS_INSTANCES_OPTIONAL_HPP
#define HASKELL_TRAITS_INSTANCES_OPTIONAL_HPP

#include <haskell_traits/monad.hpp>

#include <optional>

HASKELL_TRAITS_BEGIN
    template<typename T>
    struct rebind_impl<std::optional<T>>
    {
        template<typename U>
        using rebind   = std::optional<U>;
        using bound_to = T;
    };

    template<typename T>
    struct functor_impl<std::optional<T>>
    {
        template<typename U, typename F, REQUIRES(std::is_same_v<std::optional<T>, uncvref<U>>)>
        constexpr static std::optional<uncvref<result_t<F&&, as_same_cvref<T, U&&>>>>
        fmap(U&& u, F&& f) noexcept(noexcept(std::make_optional(std::invoke((F &&) f, *(U&&)u))))
        {
            if (u == std::nullopt)
                return std::nullopt;
            return std::invoke((F &&) f, *(U &&) u);
        }
    };

    template<typename T>
    struct monad_impl<std::optional<T>>
    {
        template<typename U, REQUIRES(std::is_same_v<uncvref<U>, T>)>
        constexpr static std::optional<T>
        mreturn(U&& u) NOEXCEPT_RETURNS(std::optional<T>((U &&) u));

        template<typename U, typename F, REQUIRES(std::is_same_v<std::optional<T>, uncvref<U>>)>
        constexpr static result_t<F&&, as_same_cvref<T, U&&>>
        mbind(U&& u, F&& f) noexcept(noexcept(std::invoke((F &&) f, *(U&&)u)))
        {
            if (u != std::nullopt)
                return std::invoke((F &&) f, *(U &&) u);
            return std::nullopt;
        }
    };
HASKELL_TRAITS_END

#endif /* HASKELL_TRAITS_INSTANCES_OPTIONAL_HPP */