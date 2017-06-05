//! GCC 4.8.4 Hack to avoid usage of variadic templates in lambda functions.
#include <tuple>
#include <rapidcheck/gen/ContainerHelper.h>

namespace rc {
namespace gen {
namespace detail {

/*!
  Replaces a lambda function that calls generate and returns its result from a ContainerHelper.
*/
template <typename Container, typename... T>
class GenDummy {
  
  public:
    using Strategy = GenericContainerStrategy<Container>;
    GenDummy(ContainerHelper<Container, Strategy> &h, T... items) : gens(std::make_tuple(items...)), helper(h) {}    
  
    Shrinkable<Container> operator()(const Random &random, const int size) const 
      noexcept {
      
      typedef typename std::tuple<const Random, const int, T...> Tuple;
      typedef typename std::decay<Tuple>::type ttype;
    
      return TupImpl<Tuple, 0 == std::tuple_size<ttype>::value, std::tuple_size<ttype>::value>::call(helper, std::forward<Tuple>(std::tuple_cat(std::make_tuple(random, size), gens)));
    }
  
  private:
    std::tuple<T...> gens;
    ContainerHelper<Container, Strategy>& helper;
   
     
    /*!
      Helper structs to unpack a tuple into an argument pack and call a function on it.
    */
    template <typename Tuple, bool Done, int Total, int... N>
    struct TupImpl
    {
      static Shrinkable<Container> call(ContainerHelper<Container, Strategy> &helper, Tuple && t)
      {
          return TupImpl<Tuple, Total == 1 + sizeof...(N), Total, N..., sizeof...(N)>::call(helper, std::forward<Tuple>(t));
      }
    };
    
    template <typename Tuple, int Total, int... N>
    struct TupImpl<Tuple, true, Total, N...>
    {
      static Shrinkable<Container> call(ContainerHelper<Container, Strategy> &helper, Tuple && t)
      {
          return helper.generate(std::get<N>(std::forward<Tuple>(t))...);
      }
    };
    

};

} //namespace detail
} //namespace gen
} //namespace rc 
