#include <haskell_traits/lazy.hpp>

#include "simple_test.hpp"

namespace ht = haskell_traits;

template<auto x>
using idx = std::integral_constant<decltype(x), x>;

struct a
{
    idx<0> operator()(int) &;
    idx<1> operator()(int) const &;
};

struct b
{
    idx<2> operator()(int) &&;
    idx<3> operator()(int) const &&;
};

int main()
{
    ht::overload_return g(a{}, b{});
    static_assert(ht::callable<decltype(g)&, int>);
    static_assert(ht::callable<decltype(g) const &, int>);
    static_assert(std::is_same_v<ht::result_t<ht::uncvref<decltype(g)>&, int>, idx<0>>);
    static_assert(std::is_same_v<ht::result_t<ht::uncvref<decltype(g)>&&, int>, idx<2>>);
    static_assert(std::is_same_v<ht::result_t<ht::uncvref<decltype(g)> const&, int>, idx<1>>);
    static_assert(std::is_same_v<ht::result_t<ht::uncvref<decltype(g)> const&&, int>, idx<3>>);
    static_assert(ht::callable<decltype(g) const &, int>);

    constexpr ht::lazy_overload_return
                   f([](int x) { return x + 1; }, [](int x) noexcept { return std::to_string(x); });
    constexpr auto l = f(42);
    constexpr int  x = l;
    static_assert(x == 43);
    static_assert(!noexcept(int(l)));
    const std::string&& y(l);
    CHECK(y == "42");
    static_assert(noexcept((std::string &&)(l)));
    static_assert(std::is_convertible_v<decltype(l)&, std::string>);
    static_assert(std::is_constructible_v<std::string, decltype(l)&>, "TODO: order conversions");
    static_assert(!std::is_rvalue_reference_v<std::add_rvalue_reference_t<int&>>);
    return test_result();
}