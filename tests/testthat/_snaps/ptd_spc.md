# it outputs expected content

    Plot the Dots SPC options:
    ================================
    value_field:          'y'
    date_field:           'x'
    facet_field:          not set
    rebase:               not set
    fix_after_n_points:   not set
    improvement_direction:'increase'
    target:               not set
    trajectory:           not set
    screen_outliers:      'TRUE'
    --------------------------------
    # A tibble: 1 x 7
       mean   lpl   upl     n common_cause special_cause_improvement special_cause~1
      <dbl> <dbl> <dbl> <int>        <dbl>                     <int>           <int>
    1 0.142 -3.01  3.29    20           20                         0               0
    # ... with abbreviated variable name 1: special_cause_concern

---

    Plot the Dots SPC options:
    ================================
    value_field:          'y'
    date_field:           'x'
    facet_field:          not set
    rebase:               '2020-01-01'
    fix_after_n_points:   not set
    improvement_direction:'increase'
    target:               not set
    trajectory:           not set
    screen_outliers:      'TRUE'
    --------------------------------
    # A tibble: 1 x 8
      rebase_group  mean   lpl   upl     n common_cause special_cause_impr~1 speci~2
             <dbl> <dbl> <dbl> <dbl> <int>        <dbl>                <int>   <int>
    1            0 0.142 -3.01  3.29    20           20                    0       0
    # ... with abbreviated variable names 1: special_cause_improvement,
    #   2: special_cause_concern

---

    Plot the Dots SPC options:
    ================================
    value_field:          'y'
    date_field:           'x'
    facet_field:          'facet'
    rebase:               not set
    fix_after_n_points:   not set
    improvement_direction:'increase'
    target:               not set
    trajectory:           not set
    screen_outliers:      'TRUE'
    --------------------------------
    # A tibble: 2 x 8
          f   mean   lpl   upl     n common_cause special_cause_improvement specia~1
      <dbl>  <dbl> <dbl> <dbl> <int>        <dbl>                     <int>    <int>
    1     0 0.0746 -2.60  2.75    10           10                         0        0
    2     1 0.209  -3.28  3.70    10           10                         0        0
    # ... with abbreviated variable name 1: special_cause_concern

---

    Plot the Dots SPC options:
    ================================
    value_field:          'y'
    date_field:           'x'
    facet_field:          'facet'
    rebase:               '2020-01-01'
    fix_after_n_points:   not set
    improvement_direction:'increase'
    target:               not set
    trajectory:           not set
    screen_outliers:      'TRUE'
    --------------------------------
    # A tibble: 2 x 9
          f rebase_group   mean   lpl   upl     n common_cause special_cau~1 speci~2
      <dbl>        <dbl>  <dbl> <dbl> <dbl> <int>        <dbl>         <int>   <int>
    1     0            0 0.0746 -2.60  2.75    10           10             0       0
    2     1            0 0.209  -3.28  3.70    10           10             0       0
    # ... with abbreviated variable names 1: special_cause_improvement,
    #   2: special_cause_concern

---

    Plot the Dots SPC options:
    ================================
    value_field:          'y'
    date_field:           'x'
    facet_field:          not set
    rebase:               not set
    fix_after_n_points:   not set
    improvement_direction:'increase'
    target:               '0.5'
    trajectory:           not set
    screen_outliers:      'TRUE'
    --------------------------------
    # A tibble: 1 x 8
       mean   lpl   upl     n common_cause special_cause_improvement speci~1 assur~2
      <dbl> <dbl> <dbl> <int>        <dbl>                     <int>   <int> <chr>  
    1 0.142 -3.01  3.29    20           20                         0       0 a      
    # ... with abbreviated variable names 1: special_cause_concern,
    #   2: assurance_type

