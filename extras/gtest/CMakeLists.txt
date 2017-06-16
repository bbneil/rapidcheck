add_library(rapidcheck_gtest INTERFACE)
target_include_directories(rapidcheck_gtest INTERFACE include)
target_link_libraries(rapidcheck_gtest INTERFACE rapidcheck)
install(DIRECTORY "include/" DESTINATION "include" PATTERN ".*" EXCLUDE)
