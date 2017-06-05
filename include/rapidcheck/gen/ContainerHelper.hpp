/*!
  Originally part of Container.hpp. Separated out as a depdendency of GenDummy.hpp
*/
#include <tuple>
#include "rapidcheck/gen/Tuple.h"
#include "rapidcheck/gen/detail/ShrinkValueIterator.h"
#include "rapidcheck/shrink/Shrink.h"
#include "rapidcheck/shrinkable/Create.h"

#define MAX_TRIES 100

namespace rc {
namespace gen {
namespace detail {

RC_SFINAE_TRAIT(IsAssociativeContainer, typename T::key_type)
RC_SFINAE_TRAIT(IsMapContainer, typename T::mapped_type)

template <typename T>
using Shrinkables = std::vector<Shrinkable<T>>;

template <typename K, typename V>
using ShrinkablePairs = std::vector<Shrinkable<std::pair<K, V>>>;

template <typename Container, typename T>
Container toContainer(const Shrinkables<T> &shrinkables) {
  return Container(makeShrinkValueIterator(begin(shrinkables)),
                   makeShrinkValueIterator(end(shrinkables)));
}

template <typename T, typename Predicate>
Shrinkables<T> generateShrinkables(const Random &random,
                                   const int size,
                                   std::size_t count,
                                   const Gen<T> &gen,
                                   const Predicate &predicate) {
  Random r(random);
  Shrinkables<T> shrinkables;
  shrinkables.reserve(count);

  int currentSize = size;
  int tries = 0;
  while (shrinkables.size() < count) {
    auto shrinkable = gen(r.split(), currentSize);
    if (predicate(shrinkable)) {
      shrinkables.push_back(std::move(shrinkable));
      tries = 0;
    } else {
      ++tries;
      if (tries >= MAX_TRIES) {
        throw GenerationFailure("Gave up trying to generate " +
                                std::to_string(count) +
                                " values for container");
      }
      ++currentSize;
    }
  }

  return shrinkables;
}

template <typename Container>
class CollectionStrategy {
public:
  template <typename T>
  Shrinkables<T> generateElements(const Random &random,
                                  const int size,
                                  std::size_t count,
                                  const Gen<T> &gen) const {
    return generateShrinkables(random, size, count, gen, fn::constant(true));
  }

  template <typename T>
  Seq<Shrinkables<T>> shrinkElements(const Shrinkables<T> &shrinkables) const {
    return shrink::eachElement(
        shrinkables, [](const Shrinkable<T> &s) { return s.shrinks(); });
  }
};

template <typename Container,
          bool = IsAssociativeContainer<Container>::value,
          bool = IsMapContainer<Container>::value>
class GenericContainerStrategy : public CollectionStrategy<Container> {};

template <typename Set>
class GenericContainerStrategy<Set, true, false> {
public:
  template <typename T>
  Shrinkables<T> generateElements(const Random &random,
                                  const int size,
                                  std::size_t count,
                                  const Gen<T> &gen) const {
    Set set;
    return generateShrinkables(random,
                               size,
                               count,
                               gen,
                               [&](const Shrinkable<T> &s) {
                                 // We want only values that can be inserted
                                 return set.insert(s.value()).second;
                               });
  }

  template <typename T>
  Seq<Shrinkables<T>> shrinkElements(const Shrinkables<T> &shrinkables) const {
    // We use a shared_ptr here both because T might not be copyable and
    // because we don't really need to copy it since we don't modify it.
    std::shared_ptr<const Set> set =
        std::make_shared<Set>(toContainer<Set>(shrinkables));
    return shrink::eachElement(
        shrinkables,
        [=](const Shrinkable<T> &s) {
          return seq::filter(s.shrinks(),
                             [=](const Shrinkable<T> &x) {
                               // Here we filter out shrinks that collide with
                               // another value in the set because that would
                               // produce an identical set.
                               return set->find(x.value()) == set->end();
                             });
        });
  }
};

template <typename Map>
class GenericContainerStrategy<Map, true, true> {
public:
  template <typename K, typename V>
  ShrinkablePairs<K, V> generateElements(const Random &random,
                                         const int size,
                                         std::size_t count,
                                         const Gen<K> &keyGen,
                                         const Gen<V> &valueGen) const {
    Random r(random);
    Map map;
    auto dummyValue = valueGen(Random(), 0);
    auto keyShrinkables = generateShrinkables(
        r.split(),
        size,
        count,
        keyGen,
        [&](const Shrinkable<K> &s) {
          // We want only keys that can be inserted
          return map.insert(std::make_pair(s.value(), dummyValue.value()))
              .second;
        });

    auto valueShrinkables =
        generateShrinkables(r, size, count, valueGen, fn::constant(true));

    ShrinkablePairs<K, V> shrinkablePairs;
    shrinkablePairs.reserve(count);
    for (std::size_t i = 0; i < count; ++i) {
      shrinkablePairs.push_back(shrinkable::pair(
          std::move(keyShrinkables[i]), std::move(valueShrinkables[i])));
    }

    return shrinkablePairs;
  }

  template <typename K, typename V>
  Seq<ShrinkablePairs<K, V>>
  shrinkElements(const ShrinkablePairs<K, V> &shrinkablePairs) const {
    // We use a shared_ptr here both because K and V might not be copyable
    // and because we don't really need to copy it since we don't modify it.
    std::shared_ptr<const Map> map =
        std::make_shared<Map>(toContainer<Map>(shrinkablePairs));
    return shrink::eachElement(
        shrinkablePairs,
        [=](const Shrinkable<std::pair<K, V>> &elem) {
          return seq::filter(
              elem.shrinks(),
              [=](const Shrinkable<std::pair<K, V>> &elemShrink) {
                // Here we filter out values with keys that collide
                // with other keys of the map. However, if the key
                // is the same, that means that something else
                // in this shrink since we expect shrinks to not
                // equal the original.
                // NOTE: This places the restriction that the key must
                // have an equality operator that works but that's
                // usually true for types used as keys anyway.
                const auto shrinkValue = elemShrink.value();
                return (map->find(shrinkValue.first) == map->end()) ||
                    (shrinkValue.first == elem.value().first);
              });
        });
  }
};

template <typename MultiMap>
class MultiMapStrategy : public CollectionStrategy<MultiMap> {
public:
  template <typename K, typename V>
  ShrinkablePairs<K, V> generateElements(const Random &random,
                                         const int size,
                                         std::size_t count,
                                         const Gen<K> &keyGen,
                                         const Gen<V> &valueGen) const {
    // We treat this as a normal collection since we don't need to worry
    // about duplicate keys et.c.
    return CollectionStrategy<MultiMap>::generateElements(
        random, size, count, gen::pair(keyGen, valueGen));
  }
};

template <typename Key, typename Compare, typename Allocator>
class GenericContainerStrategy<std::multiset<Key, Compare, Allocator>,
                               true,
                               false>
    : public CollectionStrategy<std::multiset<Key, Compare, Allocator>> {};

template <typename Key, typename Hash, typename KeyEqual, typename Allocator>
class GenericContainerStrategy<
    std::unordered_multiset<Key, Hash, KeyEqual, Allocator>,
    true,
    false>
    : public CollectionStrategy<
          std::unordered_multiset<Key, Hash, KeyEqual, Allocator>> {};

template <typename Key, typename T, typename Compare, typename Allocator>
class GenericContainerStrategy<std::multimap<Key, T, Compare, Allocator>,
                               true,
                               true>
    : public MultiMapStrategy<std::multimap<Key, T, Compare, Allocator>> {};

template <typename Key,
          typename T,
          typename Hash,
          typename KeyEqual,
          typename Allocator>
class GenericContainerStrategy<
    std::unordered_multimap<Key, T, Hash, KeyEqual, Allocator>,
    true,
    true>
    : public MultiMapStrategy<
          std::unordered_multimap<Key, T, Hash, KeyEqual, Allocator>> {};

template <typename F>
class UniqueContainerStrategy {
public:
  template <typename Arg>
  explicit UniqueContainerStrategy(Arg &&arg)
      : m_f(std::move(arg)) {}

  template <typename T>
  Shrinkables<T> generateElements(const Random &random,
                                  const int size,
                                  std::size_t count,
                                  const Gen<T> &gen) const {
    using Key = Decay<typename std::result_of<F(T)>::type>;
    std::set<Key> values;
    return detail::generateShrinkables(random,
                                       size,
                                       count,
                                       gen,
                                       [&](const Shrinkable<T> &s) {
                                         return values.insert(m_f(s.value()))
                                             .second;
                                       });
  }

  template <typename T>
  Seq<Shrinkables<T>>
  shrinkElements(const Shrinkables<T> &shrinkables) const {
    using Key = Decay<typename std::result_of<F(T)>::type>;
    const auto keys = std::make_shared<std::set<Key>>();
    for (const auto &shrinkable : shrinkables) {
      keys->insert(m_f(shrinkable.value()));
    }

    return shrink::eachElement(shrinkables,
                               [=](const Shrinkable<T> &s) {
                                 const auto valueKey = m_f(s.value());
                                 return seq::filter(
                                     s.shrinks(),
                                     [=](const Shrinkable<T> &shrink) {
                                       const auto shrinkKey = m_f(shrink.value());
                                       return (!(valueKey < shrinkKey) &&
                                               !(shrinkKey < valueKey)) ||
                                           keys->count(shrinkKey) == 0;
                                     });
                               });
  }

private:
  F m_f;
};

template <typename Container, typename Strategy>
class ContainerHelper {
public:
  explicit ContainerHelper(Strategy &&strategy)
      : m_strategy(std::move(strategy)) {}

  template <typename... Ts>
  Shrinkable<Container>
  generate(const Random &random, const int size, const Gen<Ts> &... gens) const {
    const auto strategy = m_strategy;
    auto r = random;
    std::size_t count = r.split().next() % (size + 1);
    auto shrinkables = strategy.generateElements(r, size, count, gens...);

    using Elements = decltype(shrinkables);
    return shrinkable::map(
        shrinkable::shrinkRecur(std::move(shrinkables),
                                [=](const Elements &elements) {
                                  return seq::concat(
                                      shrink::removeChunks(elements),
                                      strategy.shrinkElements(elements));
                                }),
        &toContainer<Container, typename Elements::value_type::ValueType>);
  }

  template <typename... Ts>
  Shrinkable<Container> generate(std::size_t count,
                                 const Random &random,
                                 const int size,
                                 const Gen<Ts> &... gens) const {
    const auto strategy = m_strategy;
    auto shrinkables = strategy.generateElements(random, size, count, gens...);

    using Elements = decltype(shrinkables);
    return shrinkable::map(
        shrinkable::shrinkRecur(std::move(shrinkables),
                                [=](const Elements &elements) {
                                  return strategy.shrinkElements(elements);
                                }),
        &toContainer<Container, typename Elements::value_type::ValueType>);
  }

private:
  Strategy m_strategy;
};

template <typename T, std::size_t N, typename Strategy>
class ContainerHelper<std::array<T, N>, Strategy> {
public:
  using Array = std::array<T, N>;

  explicit ContainerHelper(Strategy &&strategy)
      : m_strategy(std::move(strategy)) {}

  template <typename U>
  Shrinkable<Array>
  generate(const Random &random, const int size, const Gen<U> &gen) const {
    const auto strategy = m_strategy;
    auto shrinkables = strategy.generateElements(random, size, N, gen);

    return shrinkable::map(
        shrinkable::shrinkRecur(std::move(shrinkables),
                                [=](const Shrinkables<U> &elements) {
                                  return strategy.shrinkElements(elements);
                                }),
        [](const Shrinkables<U> &elements) {
          Array array;
          for (std::size_t i = 0; i < N; ++i) {
            array[i] = elements[i].value();
          }
          return array;
        });
  }

  template <typename U>
  Shrinkable<Array> generate(std::size_t count,
                             const Random &random,
                             const int size,
                             const Gen<U> &gen) const {
    if (count != N) {
      throw GenerationFailure(
          "Count must be equal to length of array for std::array");
    }
    return generate(random, size, gen);
  }

private:
  Strategy m_strategy;
};



// user invokes this



} //namespace detail
} //namespace gen
} //namespace rc 
