#include <haskell_traits/instances/optional.hpp>
#include <haskell_traits/instances/vector.hpp>

#include "simple_test.hpp"

#include <memory>

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

MONAD_ASSERTS(std::vector<std::unique_ptr<int>>&&);

namespace ht = haskell_traits;

template<typename G, REQUIRES(ht::monad_of<G, int>)>
auto bindShowInt(G&& g)
{
    using rebound = ht::uncvref<ht::rebind<G, std::string>>;
    return ht::mbind(g, [](int x) -> rebound { return ht::mreturn<rebound>(std::to_string(x)); });
}

template<typename G, REQUIRES(ht::monad<G>)>
auto bindShowSizeof(G&& g)
{
    using rebound = ht::uncvref<ht::rebind<G, std::string>>;
    return ht::mbind(g, [](const auto& x) -> rebound {
        return ht::mreturn<rebound>(std::to_string(sizeof(x)));
    });
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
}

void test2()
{
    std::optional<int>         x{4};
    std::optional<std::string> xs = bindShowInt(x);
    CHECK(xs == std::string("4"));

    xs = bindShowSizeof(x);
    CHECK(xs == std::to_string(sizeof(int)));

    std::optional<float> b = ht::fmap(x, [](const int y) { return float(y); });
    CHECK(b == float(4));
}

int main()
{
    test();
    test2();
    return test_result();
}