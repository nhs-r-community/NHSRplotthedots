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
       mean   lpl   upl     n common_cause special_cause_improvement
      <dbl> <dbl> <dbl> <int>        <dbl>                     <int>
    1 0.142 -3.01  3.29    20           20                         0
    # i 1 more variable: special_cause_concern <int>

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
      rebase_group  mean   lpl   upl     n common_cause special_cause_improvement
             <dbl> <dbl> <dbl> <dbl> <int>        <dbl>                     <int>
    1            0 0.142 -3.01  3.29    20           20                         0
    # i 1 more variable: special_cause_concern <int>

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
          f   mean   lpl   upl     n common_cause special_cause_improvement
      <dbl>  <dbl> <dbl> <dbl> <int>        <dbl>                     <int>
    1     0 0.0746 -2.60  2.75    10           10                         0
    2     1 0.209  -3.28  3.70    10           10                         0
    # i 1 more variable: special_cause_concern <int>

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
          f rebase_group   mean   lpl   upl     n common_cause
      <dbl>        <dbl>  <dbl> <dbl> <dbl> <int>        <dbl>
    1     0            0 0.0746 -2.60  2.75    10           10
    2     1            0 0.209  -3.28  3.70    10           10
    # i 2 more variables: special_cause_improvement <int>,
    #   special_cause_concern <int>

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
       mean   lpl   upl     n common_cause special_cause_improvement
      <dbl> <dbl> <dbl> <int>        <dbl>                     <int>
    1 0.142 -3.01  3.29    20           20                         0
    # i 2 more variables: special_cause_concern <int>, assurance_type <chr>

