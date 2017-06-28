add_library(rapidcheck_boost INTERFACE)
target_link_libraries(rapidcheck_boost INTERFACE rapidcheck)
target_include_directories(rapidcheck_boost INTERFACE include)
install(DIRECTORY "include/" DESTINATION "include" PATTERN ".*" EXCLUDE)

if (RC_ENABLE_TESTS)
  add_subdirectory(test)
endif()
