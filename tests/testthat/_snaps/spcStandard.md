# it returns expected values

    Code
      r
    Output
      # A tibble: 20 x 19
      # Groups:   f, rebaseGroup [1]
               y     x f        rebase     n target trajectory movingrange rebaseGroup
           <dbl> <int> <chr>     <dbl> <int>  <dbl>      <dbl>       <dbl>       <int>
       1 -0.560      1 no facet      0     1     NA         NA     NA                1
       2 -0.230      2 no facet      0     2     NA         NA      0.330            1
       3  1.56       3 no facet      0     3     NA         NA      1.79             1
       4  0.0705     4 no facet      0     4     NA         NA      1.49             1
       5  0.129      5 no facet      0     5     NA         NA      0.0588           1
       6  1.72       6 no facet      0     6     NA         NA      1.59             1
       7  0.461      7 no facet      0     7     NA         NA      1.25             1
       8 -1.27       8 no facet      0     8     NA         NA      1.73             1
       9 -0.687      9 no facet      0     9     NA         NA      0.578            1
      10 -0.446     10 no facet      0    10     NA         NA      0.241            1
      11  1.22      11 no facet      0    11     NA         NA      1.67             1
      12  0.360     12 no facet      0    12     NA         NA      0.864            1
      13  0.401     13 no facet      0    13     NA         NA      0.0410           1
      14  0.111     14 no facet      0    14     NA         NA      0.290            1
      15 -0.556     15 no facet      0    15     NA         NA      0.667            1
      16  1.79      16 no facet      0    16     NA         NA      2.34             1
      17  0.498     17 no facet      0    17     NA         NA      1.29             1
      18 -1.97      18 no facet      0    18     NA         NA      2.46             1
      19  0.701     19 no facet      0    19     NA         NA      2.67             1
      20 -0.473     20 no facet      0    20     NA         NA      1.17             1
      # ... with 10 more variables: fixPointsRN <int>, mean <dbl>,
      #   movingrangeaverage <dbl>, lpl <dbl>, upl <dbl>, nlpl <dbl>, nupl <dbl>,
      #   outsideLimits <dbl>, relativeToMean <dbl>, closeToLimits <dbl>

