Assignment B-1
================
Steven Duan
2023-10-27

### Setup R Library

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(datateachr)
library(testthat)
```

    ## 
    ## 载入程辑包：'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

### Exercise 1 & 2: Make a Function and Document it

``` r
#' @title Function group_statistics 
#' @description This function is used to bundle a specific group_by() %>%      
#' summarise() workflow.
#' This function output summary statistics based on corresponding dataset and 
#' factors.
#' @param data is the input dataset.
#' @param group_by_x is the variable to be grouped of.
#' @param summarise_x is the variable to be summarized with.
#' @return the mean, median, range and variance of summarise_x group by group_by_x.
#' @export
group_statistics <- function(data, group_by_x, summarise_x) {
  if(is.null(data[[as.character(substitute(group_by_x))]])) {
    stop("Error. Your input group_by_x does not contain a valid column name from your dataset.\n")
  }
  if(!is.numeric(data[[as.character(substitute(summarise_x))]])) {
    stop("Error. This function only works for numeric input for the variable to be summarized with.\n")
  }
  result <- data %>% 
    group_by({{ group_by_x }}) %>% 
    summarise(Mean = mean({{ summarise_x }}, na.rm=T),
              Median = median({{ summarise_x }}, na.rm=T),
              range =  diff(range({{ summarise_x }}), na.rm=T),
              variance = var({{ summarise_x }}), na.rm=T)
  return(result)
}
```

### Exercise 3: Example

``` r
#Example 1
#Type of group_by_x: numeric, Type of summarise_x: numeric
#The function is supposed to work and output corresponding summary statistics for the variable `diameter` grouped by `civic_number`.
example1 <- group_statistics(vancouver_trees, civic_number, diameter)
example1
```

    ## # A tibble: 8,349 × 6
    ##    civic_number  Mean Median range variance na.rm
    ##           <dbl> <dbl>  <dbl> <dbl>    <dbl> <lgl>
    ##  1            0  5.14   5      1      0.143 TRUE 
    ##  2            1 10.4   10     23     28.4   TRUE 
    ##  3            2 10.6    6.5   27.5   61.8   TRUE 
    ##  4            3  8.49   6.5   34     51.6   TRUE 
    ##  5            4 12.4   10.5   26.5   70.3   TRUE 
    ##  6            5  8.95   8     23     33.4   TRUE 
    ##  7            6 10.5    8.62  29.5   55.9   TRUE 
    ##  8            7 13.7   10.8   28     72.2   TRUE 
    ##  9            8 12.4   10.8   37.2   90.3   TRUE 
    ## 10            9 10.6    9.1   25.5   47.8   TRUE 
    ## # ℹ 8,339 more rows

``` r
#Example 2
#Type of group_by_x: character, Type of summarise_x: numeric
#The function is supposed to work and output corresponding summary statistics for the variable `diameter` grouped by `genus_name`
example2 <- group_statistics(vancouver_trees, genus_name, diameter)
example2
```

    ## # A tibble: 97 × 6
    ##    genus_name   Mean Median range variance na.rm
    ##    <chr>       <dbl>  <dbl> <dbl>    <dbl> <lgl>
    ##  1 ABIES       12.9   12     41.5    94.4  TRUE 
    ##  2 ACER        10.6    8    317      76.8  TRUE 
    ##  3 AESCULUS    23.7   25     64      91.5  TRUE 
    ##  4 AILANTHUS   15.9   19.5   18.5    74.7  TRUE 
    ##  5 ALBIZIA      6      6      0      NA    TRUE 
    ##  6 ALNUS       17.5   17.5   40      79.9  TRUE 
    ##  7 AMELANCHIER  3.21   3     20       3.22 TRUE 
    ##  8 ARALIA       6.81   6.12   9      15.4  TRUE 
    ##  9 ARAUCARIA   11.4    8.5   29      98.4  TRUE 
    ## 10 ARBUTUS     18.4   17.5   27     114.   TRUE 
    ## # ℹ 87 more rows

``` r
#Example 3
#Type of group_by_x: date, Type of summarise_x: numeric
#The function is supposed to work and output corresponding summary statistics for the variable `diameter` grouped by `date_planted`
example3 <- group_statistics(vancouver_trees, date_planted, diameter)
example3
```

    ## # A tibble: 3,995 × 6
    ##    date_planted  Mean Median range variance na.rm
    ##    <date>       <dbl>  <dbl> <dbl>    <dbl> <lgl>
    ##  1 1989-10-27   15.2   15.2   1.5      1.12 TRUE 
    ##  2 1989-10-30   12.8   13.8  11.5     19.6  TRUE 
    ##  3 1989-10-31   15.1   15.5   5.75     2.90 TRUE 
    ##  4 1989-11-02    9.41   7.75 16       25.7  TRUE 
    ##  5 1989-11-03   12     11     6        7.5  TRUE 
    ##  6 1989-11-06   13     13     8       32    TRUE 
    ##  7 1989-11-07   13.9   13.8  21.5     17.5  TRUE 
    ##  8 1989-11-08   11.5   11    21       15.5  TRUE 
    ##  9 1989-11-09    6.8    8    10       15.2  TRUE 
    ## 10 1989-11-10   14.6   16    21.5     30.5  TRUE 
    ## # ℹ 3,985 more rows

``` r
#Example 5
#Type of group_by_x: null, Type of summarise_x: numeric
#The function is supposed to fail because the input group_by_x is not a column in the dataset
example5 <- group_statistics(vancouver_trees, empty_name, diameter)
```

    ## Error in group_statistics(vancouver_trees, empty_name, diameter): Error. Your input group_by_x does not contain a valid column name from your dataset.

``` r
#Example 6
#Type of group_by_x: character, Type of summarise_x: character
#The function is supposed to fail because the input summarise_x is not numeric.
example6 <- group_statistics(vancouver_trees, assigned, on_street)
```

    ## Error in group_statistics(vancouver_trees, assigned, on_street): Error. This function only works for numeric input for the variable to be summarized with.

## Exercise 4: Test the Function

### Test 1: Vector with different types

``` r
test_that("Test 1:", {
  expect_equal(class(group_statistics(vancouver_trees, root_barrier, diameter))[1], "tbl_df")
  expect_equal(class(group_statistics(vancouver_trees, civic_number, diameter))[1], "tbl_df")
  expect_equal(class(group_statistics(vancouver_trees, date_planted, diameter))[1], "tbl_df")
})
```

    ## Test passed 🌈

### Test 2: Vector with NA’s

``` r
test_that("Test 2:", {
  expect_error(group_statistics(vancouver_trees, NA, diameter))
  expect_error(group_statistics(vancouver_trees, civic_number, NA))
  expect_error(group_statistics(NA, civic_number, diameter))
})
```

    ## Test passed 🎉

### Test 3: Vector of length 0

``` r
test_that("Test 3:", {
  expect_error(group_statistics(vancouver_trees, " ", diameter))
  expect_error(group_statistics(vancouver_trees, civic_number, numeric(0)))
})
```

    ## Test passed 😀
