#include <haskell_traits/instances/optional.hpp>
#include <haskell_traits/instances/vector.hpp>
#include <haskell_traits/lazy.hpp>

#include "simple_test.hpp"

#include <memory>
#include <string_view>

MONAD_ASSERTS(std::optional<int>);
MONAD_ASSERTS(const std::optional<int>);
MONAD_ASSERTS(std::optional<int>&);
MONAD_ASSERTS(const std::optional<int>&);
MONAD_ASSERTS(std::optional<int>&&);
MONAD_ASSERTS(const std::optional<int>&&);

MONAD_ASSERTS(std::vector<int>);
MONAD_ASSERTS(const std::vector<int>);
MONAD_ASSERTS(std::vector<int>&);
MONAD_ASSERTS(const std::vector<int>&);
MONAD_ASSERTS(std::vector<int>&&);
MONAD_ASSERTS(const std::vector<int>&&);

MONAD_ASSERTS(std::vector<std::unique_ptr<int>>&);

namespace ht = haskell_traits;

static_assert(ht::apurable<std::vector<int>>);
static_assert(ht::applicative<std::vector<int>>);
static_assert(!ht::apurable<int>);
static_assert(!ht::applicative<int>);
static_assert(!ht::applicative_of<int, int>);
static_assert(!ht::rebindable<int>);
static_assert(!ht::functor<int>);
static_assert(!ht::functor_of<int, int>);
static_assert(!ht::monad<int>);
static_assert(!ht::monad_of<int, int>);
static_assert(!ht::rebindable<int&&>);
static_assert(!ht::applicative<int&&>);
static_assert(!ht::applicative_of<int&&, int&&>);
static_assert(!ht::functor<int&&>);
static_assert(!ht::functor_of<int&&, int&&>);
static_assert(!ht::monad<int&&>);
static_assert(!ht::monad_of<int&&, int&&>);

using namespace haskell_traits::operators;

template<typename G, REQUIRES(ht::monad_of<G, int>)>
constexpr auto bindShowInt(G&& g)
{
    using rebound = ht::uncvref<ht::rebind<G, std::string>>;
    return g >> [](int x) -> rebound { return ht::apure(std::to_string(x)); };
}

template<typename G, REQUIRES(ht::monad<G>)>
constexpr auto bindShowSizeof(G&& g)
{
    using rebound = ht::uncvref<ht::rebind<G, std::string>>;
    return g >> [](const auto& x) -> rebound { return ht::apure(std::to_string(sizeof(x))); };
}

void test3()
{
    int              v = 0;
    std::vector<int> u = ht::apure(v);
    CHECK(u == std::vector<int>{0});

    std::shared_ptr<int>                x{new int{5}};
    auto                                y = ht::apure(std::move(x));
    std::optional<std::shared_ptr<int>> m(y);
    std::optional<std::shared_ptr<int>> z(std::move(y));
    std::optional<std::shared_ptr<int>> w(std::move(y));
    CHECK(z != std::nullopt);
    CHECK(**z == 5);
    CHECK(x == nullptr);
    CHECK(*w == nullptr);
    CHECK(*m != nullptr);
    CHECK(**m == 5);

    {
        std::vector                   x{0, 1, 2};
        auto                          y = std::move(x) >> [](int x) { return x + 1; };
        std::vector<int>              z = x >> [](int x) { return std::vector{x + 2}; };
        std::vector<std::vector<int>> w = x >> [](int x) { return std::vector{x + 2}; };
        static_assert(std::is_same_v<decltype(y), std::vector<int>>);
        CHECK(y == std::vector{1, 2, 3});
        CHECK(z == std::vector{2, 3, 4});
        CHECK(w == std::vector<std::vector<int>>{{2}, {3}, {4}});
    }
}

void test()
{
    std::vector<int> x{1, 2, 3, 4, 5, 6, 7};

    std::vector<std::string> xs = bindShowInt(x);
    CHECK(xs == std::vector<std::string>{"1", "2", "3", "4", "5", "6", "7"});

    xs = bindShowSizeof(x);

    const auto sizeString = std::to_string(sizeof(int));
    CHECK(xs
          == std::vector<std::string>{sizeString,
                                      sizeString,
                                      sizeString,
                                      sizeString,
                                      sizeString,
                                      sizeString,
                                      sizeString});
    CHECK(ht::fmap(x, [](const int y) { return std::to_string(y); })
          == std::vector<std::string>{"1", "2", "3", "4", "5", "6", "7"});

    std::vector<std::unique_ptr<int>> z{};
}

auto fIntToString = [](auto x, REQUIRESF(ht::functor_of<decltype(x), int>)) {
    return ht::fmap(x, [](const int y) { return std::to_string(y); });
};

auto z = fIntToString(std::vector<int>{1});

void test2()
{
    constexpr std::optional<int> x{4};
    std::optional<std::string>   xs = bindShowInt(x);
    CHECK(xs == std::string("4"));

    xs = bindShowSizeof(x);
    CHECK(xs == std::to_string(sizeof(int)));

    constexpr std::optional<float> b = ht::fmap(x, [](const int y) { return float(y); });
    static_assert(b == float(4));

    constexpr auto foo = [](std::string_view x) constexpr->int { return x.size(); };
    constexpr std::optional<decltype(foo)>    f{foo};
    constexpr std::optional<std::string_view> i{"foobar"};
    static_assert(ht::aapply(i, f) == 6);
    static_assert(i >> f == 6);
}

int main()
{
    test();
    test2();
    test3();
    return test_result();
}