#include <haskell_traits/lazy.hpp>

#include "simple_test.hpp"

namespace ht = haskell_traits;

int main()
{
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