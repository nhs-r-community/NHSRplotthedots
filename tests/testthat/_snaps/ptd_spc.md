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
    # A tibble: 1 x 8
      mean_col   lpl   upl     n common_cause special_cause_improvement
         <dbl> <dbl> <dbl> <int>        <int>                     <int>
    1    0.142 -3.01  3.29    20           20                         0
    # i 2 more variables: special_cause_concern <int>, variation_type <chr>

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
    # A tibble: 1 x 9
      rebase_group mean_col   lpl   upl     n common_cause special_cause_improvement
             <dbl>    <dbl> <dbl> <dbl> <int>        <int>                     <int>
    1            0    0.142 -3.01  3.29    20           20                         0
    # i 2 more variables: special_cause_concern <int>, variation_type <chr>

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
    # A tibble: 2 x 9
          f mean_col   lpl   upl     n common_cause special_cause_improvement
      <dbl>    <dbl> <dbl> <dbl> <int>        <int>                     <int>
    1     0   0.0746 -2.60  2.75    10           10                         0
    2     1   0.209  -3.28  3.70    10           10                         0
    # i 2 more variables: special_cause_concern <int>, variation_type <chr>

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
    # A tibble: 2 x 10
          f rebase_group mean_col   lpl   upl     n common_cause
      <dbl>        <dbl>    <dbl> <dbl> <dbl> <int>        <int>
    1     0            0   0.0746 -2.60  2.75    10           10
    2     1            0   0.209  -3.28  3.70    10           10
    # i 3 more variables: special_cause_improvement <int>,
    #   special_cause_concern <int>, variation_type <chr>

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
    # A tibble: 1 x 9
      mean_col   lpl   upl     n common_cause special_cause_improvement
         <dbl> <dbl> <dbl> <int>        <int>                     <int>
    1    0.142 -3.01  3.29    20           20                         0
    # i 3 more variables: special_cause_concern <int>, variation_type <chr>,
    #   assurance_type <chr>

# summary with a target

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
    # A tibble: 1 x 9
      mean_col   lpl   upl     n common_cause special_cause_improvement
         <dbl> <dbl> <dbl> <int>        <int>                     <int>
    1    0.142 -3.01  3.29    20           20                         0
    # i 3 more variables: special_cause_concern <int>, variation_type <chr>,
    #   assurance_type <chr>

