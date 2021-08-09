# it returns expected values

    Code
      r1
    Output
      # A tibble: 20 x 13
               y x          f       trajectory target rebase_group   fix_y  mean   lpl
           <dbl> <date>     <chr>        <dbl>  <dbl>        <dbl>   <dbl> <dbl> <dbl>
       1 -0.560  2020-01-02 no fac~         NA     NA            0 -0.560  0.142 -3.01
       2 -0.230  2020-01-03 no fac~         NA     NA            0 -0.230  0.142 -3.01
       3  1.56   2020-01-04 no fac~         NA     NA            0  1.56   0.142 -3.01
       4  0.0705 2020-01-05 no fac~         NA     NA            0  0.0705 0.142 -3.01
       5  0.129  2020-01-06 no fac~         NA     NA            0  0.129  0.142 -3.01
       6  1.72   2020-01-07 no fac~         NA     NA            0  1.72   0.142 -3.01
       7  0.461  2020-01-08 no fac~         NA     NA            0  0.461  0.142 -3.01
       8 -1.27   2020-01-09 no fac~         NA     NA            0 -1.27   0.142 -3.01
       9 -0.687  2020-01-10 no fac~         NA     NA            0 -0.687  0.142 -3.01
      10 -0.446  2020-01-11 no fac~         NA     NA            0 -0.446  0.142 -3.01
      11  1.22   2020-01-12 no fac~         NA     NA            0  1.22   0.142 -3.01
      12  0.360  2020-01-13 no fac~         NA     NA            0  0.360  0.142 -3.01
      13  0.401  2020-01-14 no fac~         NA     NA            0  0.401  0.142 -3.01
      14  0.111  2020-01-15 no fac~         NA     NA            0  0.111  0.142 -3.01
      15 -0.556  2020-01-16 no fac~         NA     NA            0 -0.556  0.142 -3.01
      16  1.79   2020-01-17 no fac~         NA     NA            0  1.79   0.142 -3.01
      17  0.498  2020-01-18 no fac~         NA     NA            0  0.498  0.142 -3.01
      18 -1.97   2020-01-19 no fac~         NA     NA            0 -1.97   0.142 -3.01
      19  0.701  2020-01-20 no fac~         NA     NA            0  0.701  0.142 -3.01
      20 -0.473  2020-01-21 no fac~         NA     NA            0 -0.473  0.142 -3.01
      # ... with 4 more variables: upl <dbl>, outside_limits <lgl>,
      #   relative_to_mean <dbl>, close_to_limits <lgl>

---

    Code
      r2
    Output
      # A tibble: 20 x 13
               y x          f       trajectory target rebase_group   fix_y  mean   lpl
           <dbl> <date>     <chr>        <dbl>  <dbl>        <dbl>   <dbl> <dbl> <dbl>
       1 -0.560  2020-01-02 no fac~         NA     NA            0 -0.560  0.669 -2.38
       2 -0.230  2020-01-03 no fac~         NA     NA            0 -0.230  0.669 -2.38
       3  1.56   2020-01-04 no fac~         NA     NA            0  1.56   0.669 -2.38
       4  0.0705 2020-01-05 no fac~         NA     NA            0  0.0705 0.669 -2.38
       5  0.129  2020-01-06 no fac~         NA     NA            0  0.129  0.669 -2.38
       6  1.72   2020-01-07 no fac~         NA     NA            0  1.72   0.669 -2.38
       7  0.461  2020-01-08 no fac~         NA     NA            0  0.461  0.669 -2.38
       8 -1.27   2020-01-09 no fac~         NA     NA            0 -1.27   0.669 -2.38
       9 -0.687  2020-01-10 no fac~         NA     NA            0 -0.687  0.669 -2.38
      10 -0.446  2020-01-11 no fac~         NA     NA            0 -0.446  0.669 -2.38
      11  1.22   2020-01-12 no fac~         NA     NA            0  1.22   0.669 -2.38
      12  0.360  2020-01-13 no fac~         NA     NA            0  0.360  0.669 -2.38
      13  0.401  2020-01-14 no fac~         NA     NA            0  0.401  0.669 -2.38
      14  0.111  2020-01-15 no fac~         NA     NA            0  0.111  0.669 -2.38
      15 10      2020-01-16 no fac~         NA     NA            0 10      0.669 -2.38
      16  1.79   2020-01-17 no fac~         NA     NA            0  1.79   0.669 -2.38
      17  0.498  2020-01-18 no fac~         NA     NA            0  0.498  0.669 -2.38
      18 -1.97   2020-01-19 no fac~         NA     NA            0 -1.97   0.669 -2.38
      19  0.701  2020-01-20 no fac~         NA     NA            0  0.701  0.669 -2.38
      20 -0.473  2020-01-21 no fac~         NA     NA            0 -0.473  0.669 -2.38
      # ... with 4 more variables: upl <dbl>, outside_limits <lgl>,
      #   relative_to_mean <dbl>, close_to_limits <lgl>

---

    Code
      r3
    Output
      # A tibble: 20 x 13
               y x          f       trajectory target rebase_group   fix_y  mean   lpl
           <dbl> <date>     <chr>        <dbl>  <dbl>        <dbl>   <dbl> <dbl> <dbl>
       1 -0.560  2020-01-02 no fac~         NA     NA            0 -0.560  0.669 -4.60
       2 -0.230  2020-01-03 no fac~         NA     NA            0 -0.230  0.669 -4.60
       3  1.56   2020-01-04 no fac~         NA     NA            0  1.56   0.669 -4.60
       4  0.0705 2020-01-05 no fac~         NA     NA            0  0.0705 0.669 -4.60
       5  0.129  2020-01-06 no fac~         NA     NA            0  0.129  0.669 -4.60
       6  1.72   2020-01-07 no fac~         NA     NA            0  1.72   0.669 -4.60
       7  0.461  2020-01-08 no fac~         NA     NA            0  0.461  0.669 -4.60
       8 -1.27   2020-01-09 no fac~         NA     NA            0 -1.27   0.669 -4.60
       9 -0.687  2020-01-10 no fac~         NA     NA            0 -0.687  0.669 -4.60
      10 -0.446  2020-01-11 no fac~         NA     NA            0 -0.446  0.669 -4.60
      11  1.22   2020-01-12 no fac~         NA     NA            0  1.22   0.669 -4.60
      12  0.360  2020-01-13 no fac~         NA     NA            0  0.360  0.669 -4.60
      13  0.401  2020-01-14 no fac~         NA     NA            0  0.401  0.669 -4.60
      14  0.111  2020-01-15 no fac~         NA     NA            0  0.111  0.669 -4.60
      15 10      2020-01-16 no fac~         NA     NA            0 10      0.669 -4.60
      16  1.79   2020-01-17 no fac~         NA     NA            0  1.79   0.669 -4.60
      17  0.498  2020-01-18 no fac~         NA     NA            0  0.498  0.669 -4.60
      18 -1.97   2020-01-19 no fac~         NA     NA            0 -1.97   0.669 -4.60
      19  0.701  2020-01-20 no fac~         NA     NA            0  0.701  0.669 -4.60
      20 -0.473  2020-01-21 no fac~         NA     NA            0 -0.473  0.669 -4.60
      # ... with 4 more variables: upl <dbl>, outside_limits <lgl>,
      #   relative_to_mean <dbl>, close_to_limits <lgl>

