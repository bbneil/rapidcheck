#pragma once

#include "rapidcheck/gen/Arbitrary.h"
#include "rapidcheck/gen/GenDummy.h"

namespace rc {
namespace gen {
namespace detail {


// MSVC HACK: there used to be a really nice macro and template solution here
// that doesn't work with MSVC

template <typename T, typename Allocator>
struct DefaultArbitrary<std::vector<T, Allocator>> {
  static Gen<std::vector<T, Allocator>> arbitrary() {
    return gen::container<std::vector<T, Allocator>>(
        gen::arbitrary<T>());
  }
};

template <typename T, typename Allocator>
struct DefaultArbitrary<std::deque<T, Allocator>> {
  static Gen<std::deque<T, Allocator>> arbitrary() {
    return gen::container<std::deque<T, Allocator>>(
        gen::arbitrary<T>());
  }
};

template <typename T, typename Allocator>
struct DefaultArbitrary<std::forward_list<T, Allocator>> {
  static Gen<std::forward_list<T, Allocator>> arbitrary() {
    return gen::container<std::forward_list<T, Allocator>>(
        gen::arbitrary<T>());
  }
};

template <typename T, typename Allocator>
struct DefaultArbitrary<std::list<T, Allocator>> {
  static Gen<std::list<T, Allocator>> arbitrary() {
    return gen::container<std::list<T, Allocator>>(
        gen::arbitrary<T>());
  }
};

template <typename Key, typename Compare, typename Allocator>
struct DefaultArbitrary<std::set<Key, Compare, Allocator>> {
  static Gen<std::set<Key, Compare, Allocator>> arbitrary() {
    return gen::container<std::set<Key, Compare, Allocator>>(
        gen::arbitrary<Key>());
  }
};

template <typename Key, typename Compare, typename Allocator>
struct DefaultArbitrary<std::multiset<Key, Compare, Allocator>> {
  static Gen<std::multiset<Key, Compare, Allocator>> arbitrary() {
    return gen::container<std::multiset<Key, Compare, Allocator>>(
        gen::arbitrary<Key>());
  }
};

template <typename Key, typename Hash, typename KeyEqual, typename Allocator>
struct DefaultArbitrary<std::unordered_set<Key, Hash, KeyEqual, Allocator>> {
  static Gen<std::unordered_set<Key, Hash, KeyEqual, Allocator>> arbitrary() {
    return gen::container<std::unordered_set<Key, Hash, KeyEqual, Allocator>>(
        gen::arbitrary<Key>());
  }
};

template <typename Key, typename Hash, typename KeyEqual, typename Allocator>
struct DefaultArbitrary<std::unordered_multiset<Key, Hash, KeyEqual, Allocator>> {
  static Gen<std::unordered_multiset<Key, Hash, KeyEqual, Allocator>> arbitrary() {
    return gen::container<std::unordered_multiset<Key, Hash, KeyEqual, Allocator>>(
        gen::arbitrary<Key>());
  }
};

template <typename Key, typename T, typename Compare, typename Allocator>
struct DefaultArbitrary<std::map<Key, T, Compare, Allocator>> {
  static Gen<std::map<Key, T, Compare, Allocator>> arbitrary() {
    return gen::container<std::map<Key, T, Compare, Allocator>>(
      gen::arbitrary<Key>(), gen::arbitrary<T>());
  }
};

template <typename Key, typename T, typename Compare, typename Allocator>
struct DefaultArbitrary<std::multimap<Key, T, Compare, Allocator>> {
  static Gen<std::multimap<Key, T, Compare, Allocator>> arbitrary() {
    return gen::container<std::multimap<Key, T, Compare, Allocator>>(
      gen::arbitrary<Key>(), gen::arbitrary<T>());
  }
};

template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator>

struct DefaultArbitrary<std::unordered_map<Key, T, Hash, KeyEqual, Allocator>> {
  static Gen<std::unordered_map<Key, T, Hash, KeyEqual, Allocator>> arbitrary() {
    return gen::container<std::unordered_map<Key, T, Hash, KeyEqual, Allocator>>(
      gen::arbitrary<Key>(), gen::arbitrary<T>());
  }
};

template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator>
struct DefaultArbitrary<std::unordered_multimap<Key, T, Hash, KeyEqual, Allocator>> {
  static Gen<std::unordered_multimap<Key, T, Hash, KeyEqual, Allocator>> arbitrary() {
    return gen::container<std::unordered_multimap<Key, T, Hash, KeyEqual, Allocator>>(
      gen::arbitrary<Key>(), gen::arbitrary<T>());
  }
};

// std::array is a bit special since it has non-type template params
template <typename T, std::size_t N>
struct DefaultArbitrary<std::array<T, N>> {
  static Gen<std::array<T, N>> arbitrary() {
    return gen::container<std::array<T, N>>(gen::arbitrary<T>());
  }
};

} // namespace detail

template <typename Container, typename... Ts>
Gen<Container> container(Gen<Ts>... gens) {
  using Strategy = detail::GenericContainerStrategy<Container>;
  detail::ContainerHelper<Container, Strategy> helper{Strategy()};

  return detail::GenDummy<Container, Gen<Ts>...>(helper, gens...);
}

template <typename Container, typename... Ts>
Gen<Container> container(std::size_t count, Gen<Ts>... gens) {
  using Strategy = detail::GenericContainerStrategy<Container>;
  detail::ContainerHelper<Container, Strategy> helper{Strategy()};


  return detail::GenDummy<Container, Gen<Ts>...>(helper, gens...);
}

template <typename Container, typename T>
Gen<Container> unique(Gen<T> gen) {
  return gen::uniqueBy<Container>(std::move(gen),
                                  [](const T &x) -> const T & { return x; });
}

template <typename Container, typename T, typename F>
Gen<Container> uniqueBy(Gen<T> gen, F &&f) {
  using Strategy = detail::UniqueContainerStrategy<Decay<F>>;
  detail::ContainerHelper<Container, Strategy> helper(
      Strategy(std::forward<F>(f)));

  return [=](const Random &random, int size) {
    return helper.generate(random, size, gen);
  };
}

} // namespace gen
} // namespace rc
