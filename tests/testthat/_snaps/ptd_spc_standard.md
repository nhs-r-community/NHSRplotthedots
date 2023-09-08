# it returns expected values

    Code
      pillar::glimpse(r1)
    Output
      Rows: 20
      Columns: 12
      $ y                <dbl> -0.56047565, -0.23017749, 1.55870831, 0.07050839, 0.1~
      $ x                <date> 2020-01-02, 2020-01-03, 2020-01-04, 2020-01-05, 2020~
      $ f                <chr> "no facet", "no facet", "no facet", "no facet", "no f~
      $ trajectory       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
      $ rebase_group     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
      $ fix_y            <dbl> -0.56047565, -0.23017749, 1.55870831, 0.07050839, 0.1~
      $ mean_col         <dbl> 0.1416238, 0.1416238, 0.1416238, 0.1416238, 0.1416238~
      $ lpl              <dbl> -3.01138, -3.01138, -3.01138, -3.01138, -3.01138, -3.~
      $ upl              <dbl> 3.294627, 3.294627, 3.294627, 3.294627, 3.294627, 3.2~
      $ outside_limits   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS~
      $ relative_to_mean <dbl> -1, -1, 1, -1, -1, 1, 1, -1, -1, -1, 1, 1, 1, -1, -1,~
      $ close_to_limits  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS~

---

    Code
      pillar::glimpse(r2)
    Output
      Rows: 20
      Columns: 12
      $ y                <dbl> -0.56047565, -0.23017749, 1.55870831, 0.07050839, 0.1~
      $ x                <date> 2020-01-02, 2020-01-03, 2020-01-04, 2020-01-05, 2020~
      $ f                <chr> "no facet", "no facet", "no facet", "no facet", "no f~
      $ trajectory       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
      $ rebase_group     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
      $ fix_y            <dbl> -0.56047565, -0.23017749, 1.55870831, 0.07050839, 0.1~
      $ mean_col         <dbl> 0.6694159, 0.6694159, 0.6694159, 0.6694159, 0.6694159~
      $ lpl              <dbl> -2.383666, -2.383666, -2.383666, -2.383666, -2.383666~
      $ upl              <dbl> 3.722497, 3.722497, 3.722497, 3.722497, 3.722497, 3.7~
      $ outside_limits   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS~
      $ relative_to_mean <dbl> -1, -1, 1, -1, -1, 1, -1, -1, -1, -1, 1, -1, -1, -1, ~
      $ close_to_limits  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS~

---

    Code
      pillar::glimpse(r3)
    Output
      Rows: 20
      Columns: 12
      $ y                <dbl> -0.56047565, -0.23017749, 1.55870831, 0.07050839, 0.1~
      $ x                <date> 2020-01-02, 2020-01-03, 2020-01-04, 2020-01-05, 2020~
      $ f                <chr> "no facet", "no facet", "no facet", "no facet", "no f~
      $ trajectory       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
      $ rebase_group     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
      $ fix_y            <dbl> -0.56047565, -0.23017749, 1.55870831, 0.07050839, 0.1~
      $ mean_col         <dbl> 0.6694159, 0.6694159, 0.6694159, 0.6694159, 0.6694159~
      $ lpl              <dbl> -4.596625, -4.596625, -4.596625, -4.596625, -4.596625~
      $ upl              <dbl> 5.935457, 5.935457, 5.935457, 5.935457, 5.935457, 5.9~
      $ outside_limits   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS~
      $ relative_to_mean <dbl> -1, -1, 1, -1, -1, 1, -1, -1, -1, -1, 1, -1, -1, -1, ~
      $ close_to_limits  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS~

