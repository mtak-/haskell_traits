function(make_test t)
    add_executable(${t} ${t}.cpp)
    target_compile_definitions(${t} PRIVATE HASKELL_TRAITS_TESTNAME="${t}")
    add_test(test.${t} ${t})
endfunction()

make_test(func)
make_test(basic_tests)