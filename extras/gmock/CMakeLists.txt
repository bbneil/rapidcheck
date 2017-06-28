add_library(rapidcheck_gmock INTERFACE)
target_link_libraries(rapidcheck_gmock INTERFACE rapidcheck)
target_include_directories(rapidcheck_gmock INTERFACE include)
install(DIRECTORY "include/" DESTINATION "include" PATTERN ".*" EXCLUDE)

if (RC_ENABLE_TESTS)
  add_subdirectory(test)
endif()
