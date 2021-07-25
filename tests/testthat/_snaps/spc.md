# it outputs expected content

    Plot the Dots SPC options:
    ================================
    valueField:           'y'
    dateField:            'x'
    facetField:           not set
    rebase:               not set
    fixAfterNPoints:      not set
    improvementDirection: 'increase'
    target:               not set
    trajectory:           not set
    --------------------------------
    # A tibble: 1 x 7
       mean   lpl   upl     n common_cause special_cause_improve~ special_cause_con~
      <dbl> <dbl> <dbl> <int>        <dbl>                  <int>              <int>
    1 0.142 -3.01  3.29    20           20                      0                  0

---

    Plot the Dots SPC options:
    ================================
    valueField:           'y'
    dateField:            'x'
    facetField:           not set
    rebase:               'rebase'
    fixAfterNPoints:      not set
    improvementDirection: 'increase'
    target:               not set
    trajectory:           not set
    --------------------------------
    # A tibble: 2 x 8
      rebaseGroup  mean   lpl   upl     n common_cause special_cause_improvement
            <int> <dbl> <dbl> <dbl> <int>        <dbl>                     <int>
    1           1 0.132 -2.80  3.06     9            9                         0
    2           2 0.149 -3.43  3.73    11           11                         0
    # ... with 1 more variable: special_cause_concern <int>

---

    Plot the Dots SPC options:
    ================================
    valueField:           'y'
    dateField:            'x'
    facetField:           'facet'
    rebase:               not set
    fixAfterNPoints:      not set
    improvementDirection: 'increase'
    target:               not set
    trajectory:           not set
    --------------------------------
    # A tibble: 2 x 8
          f   mean   lpl   upl     n common_cause special_cause_im~ special_cause_c~
      <dbl>  <dbl> <dbl> <dbl> <int>        <dbl>             <int>            <int>
    1     0 0.0746 -2.60  2.75    10           10                 0                0
    2     1 0.209  -3.28  3.70    10           10                 0                0

---

    Plot the Dots SPC options:
    ================================
    valueField:           'y'
    dateField:            'x'
    facetField:           'facet'
    rebase:               'rebase'
    fixAfterNPoints:      not set
    improvementDirection: 'increase'
    target:               not set
    trajectory:           not set
    --------------------------------
    # A tibble: 3 x 9
          f rebaseGroup   mean   lpl   upl     n common_cause special_cause_improve~
      <dbl>       <int>  <dbl> <dbl> <dbl> <int>        <dbl>                  <int>
    1     0           1  0.132 -2.80 3.06      9            9                      0
    2     0           2 -0.446 -1.09 0.196     1            1                      0
    3     1           1  0.209 -3.28 3.70     10           10                      0
    # ... with 1 more variable: special_cause_concern <int>

