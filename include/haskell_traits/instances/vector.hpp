#ifndef HASKELL_TRAITS_INSTANCES_VECTOR_HPP
#define HASKELL_TRAITS_INSTANCES_VECTOR_HPP

#include <haskell_traits/monad.hpp>

#include <vector>

HASKELL_TRAITS_BEGIN
    template<typename T, typename Alloc>
    struct rebind_impl<std::vector<T, Alloc>>
    {
        template<typename U>
        using rebind
            = std::vector<U, typename std::allocator_traits<Alloc>::template rebind_alloc<U>>;
        using bound_to = T;
    };

    template<typename T, typename Alloc>
    struct functor_impl<std::vector<T, Alloc>>
    {
        template<typename U,
                 typename F,
                 REQUIRES(std::is_same_v<std::vector<T, Alloc>, uncvref<U>>),
                 typename Result
                 = rebind<std::vector<T, Alloc>, uncvref<result_t<F&&, as_same_cvref<T, U&&>>>>>
        static Result fmap(U&& u, F&& f)
        {
            using CVRefElem = as_same_cvref<T, U&&>;
            Result result;
            for (auto&& elem : u)
                result.push_back(std::invoke(f, static_cast<CVRefElem>(elem)));
            return result;
        }
    };

    template<typename T, typename... Args>
    struct monad_impl<std::vector<T, Args...>>
    {
        template<typename U, REQUIRES(std::is_same_v<uncvref<U>, T>)>
        static std::vector<T, Args...> mreturn(U&& u)
        {
            std::vector<T> t;
            t.push_back((U &&) u);
            return t;
        }

        template<typename U,
                 typename F,
                 REQUIRES(std::is_same_v<std::vector<T, Args...>, uncvref<U>>)>
        static result_t<F&, as_same_cvref<T, U&&>> mbind(U&& u, F&& f)
        {
            using CVRefElem = as_same_cvref<T, U&&>;
            result_t<F&, CVRefElem> result;
            for (auto&& elem : u) {
                for (auto&& inner_elem : f(static_cast<CVRefElem>(elem))) {
                    result.emplace_back(std::forward<decltype(inner_elem)>(inner_elem));
                }
            }
            return result;
        }
    };
HASKELL_TRAITS_END

#endif /* HASKELL_TRAITS_INSTANCES_VECTOR_HPP */