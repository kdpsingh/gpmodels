
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpmodels

## A Grammar of Prediction Models

This package provides a grammar for data preparation and evaluation of
fixed-origin and rolling-origin prediction models using data collected
at irregular intervals.

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## Installation

You can install the GitHub version of gpmodels with:

``` r
remotes::install_github('ML4LHS/gpmodels')
```

## How to set up a time\_frame()

Start by loading and package and defining your `time_frame()`. A
`time_frame` is simply a list with the class `time_frame` and contains
all the key information needed to describe both your fixed dataset (such
as demographics, one row per patient) and your temporal dataset (one row
per observation linked to a timestamp).

``` r
library(gpmodels)
```

``` r
library(magrittr)
library(lubridate)

future::plan('multisession')

unlink(file.path(tempdir(), 'gpmodels_dir', '*.*'))

tf = time_frame(fixed_data = sample_fixed_data,
               temporal_data = sample_temporal_data %>% dplyr::filter(id %in% 1:100),
               fixed_id = 'id',
               fixed_start = 'admit_time',
               fixed_end = 'dc_time',
               temporal_id = 'id',
               temporal_time = 'time',
               temporal_variable = 'variable',
               temporal_category = 'category',
               temporal_value = 'value',
               step = hours(6),
               max_length = days(7), # optional parameter to limit to first 7 days of hospitalization
               output_folder = file.path(tempdir(), 'gpmodels_dir'),
               create_folder = TRUE)
```

## Let’s look at the automatically generated data dictionaries

``` r
names(tf)
#>  [1] "fixed_data"         "temporal_data"      "fixed_id"           "fixed_start"        "fixed_end"          "temporal_id"       
#>  [7] "temporal_time"      "temporal_variable"  "temporal_value"     "temporal_category"  "step"               "max_length"        
#> [13] "step_units"         "output_folder"      "fixed_data_dict"    "temporal_data_dict" "chunk_size"

tf$step
#> [1] 6

tf$step_units
#> [1] "hour"

tf$fixed_data_dict
#>      variable     class
#> 1          id   integer
#> 2         sex character
#> 3         age   numeric
#> 4        race character
#> 5 baseline_cr   numeric
#> 6  admit_time   POSIXct
#> 7     dc_time   POSIXct

tf$temporal_data_dict
#>   variable     class
#> 1       cr   numeric
#> 2  cr_abnl character
#> 3  cr_high character
#> 4      med character
```

## Let’s dummy code the temporal categorical variables

``` r
tf = tf %>% 
  pre_dummy_code()
```

This affects only the temporal data and not the fixed data.

``` r
tf$fixed_data_dict
#>      variable     class
#> 1          id   integer
#> 2         sex character
#> 3         age   numeric
#> 4        race character
#> 5 baseline_cr   numeric
#> 6  admit_time   POSIXct
#> 7     dc_time   POSIXct

tf$temporal_data_dict
#>              variable   class
#> 1                  cr numeric
#> 2        cr_abnl_high numeric
#> 3         cr_abnl_low numeric
#> 4      cr_abnl_normal numeric
#> 5          cr_high_no numeric
#> 6         cr_high_yes numeric
#> 7   med_acetaminophen numeric
#> 8         med_aspirin numeric
#> 9 med_diphenhydramine numeric
```

## Let’s add some predictors and outcomes

The default method writes output to the folder defined in your
`time_frame`. When you write your output to file, you are allowed to
chain together `add_predictors()` and `add_outcomes()` functions. This
is possble because these functions invisibly return a `time_frame`.

If, however, you set `output_file` to `FALSE`, then your actual output
is returned (rather than the `time_frame`) so you cannot chain
functions.

``` r
tf %>%           
  add_rolling_predictors(variables = 'cr', # Note: You can supply a vector of variables
                         lookback = hours(12), 
                         window = hours(6), 
                         stats = c(mean = mean,
                                   min = min,
                                   max = max,
                                   median = median,
                                   length = length)) %>%
  add_baseline_predictors(variables = 'cr', # add baseline creatinine
                          lookback = days(90),
                          offset = hours(10),
                          stats = c(min = min)) %>%
  add_growing_predictors(variables = 'cr', # cumulative max creatinine since admission
                         stats = c(max = max)) %>%
  add_rolling_predictors(category = 'med', # Note: category is always a regular expression 
                         lookback = days(7),
                         stats = c(sum = sum)) %>% 
  add_rolling_outcomes(variables = 'cr',
                       lookahead = hours(24), 
                       stats = c(max = max))
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 1540
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#> The output file was written to: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/rolling_predictors_variables_cr_2021_07_12_01_33_50.csv
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 100
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#> The output file was written to: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/baseline_predictors_variables_cr_2021_07_12_01_33_52.csv
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 1540
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#> The output file was written to: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/growing_predictors_variables_cr_2021_07_12_01_34_17.csv
#> Joining, by = "id"
#> Processing category: med...
#> Allocating memory...
#> Number of rows in final output: 1540
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#> The output file was written to: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/rolling_predictors_category_med_2021_07_12_01_34_48.csv
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 1540
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#> The output file was written to: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/rolling_outcomes_variables_cr_2021_07_12_01_35_19.csv
```

## Let’s combine our output into a single data frame

You can provide `combine_output()` with a set of data frames separated
by commas. Or, you can provide a vector of file names using the `files`
argument. If you leave `files` blank, it will automatically find all the
`.csv` files from the `output_folder` of your `time_frame`.

This resulting frame is essentially ready for modeling (using
`tidymodels`, for example). Make sure to keep individual patients in the
same fold if you divide this dataset into multiple folds.

``` r
model_data = combine_output(tf)
#> Reading file: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/baseline_predictors_variables_cr_2021_07_12_01_33_52.csv...
#> Reading file: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/growing_predictors_variables_cr_2021_07_12_01_34_17.csv...
#> Reading file: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/rolling_outcomes_variables_cr_2021_07_12_01_35_19.csv...
#> Reading file: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/rolling_predictors_category_med_2021_07_12_01_34_48.csv...
#> Reading file: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/rolling_predictors_variables_cr_2021_07_12_01_33_50.csv...
#> Joining, by = "id"
#> Joining, by = "id"
#> Joining, by = c("id", "time")
#> Joining, by = c("id", "time")
#> Joining, by = c("id", "time")

head(model_data)
#>   id  sex      age  race baseline_cr          admit_time             dc_time baseline_cr_min_2160 time growing_cr_max
#> 1  1 male 66.15955 asian    1.001175 2019-06-02 00:49:23 2019-06-08 10:38:23                   NA    0             NA
#> 2  1 male 66.15955 asian    1.001175 2019-06-02 00:49:23 2019-06-08 10:38:23                   NA    6             NA
#> 3  1 male 66.15955 asian    1.001175 2019-06-02 00:49:23 2019-06-08 10:38:23                   NA   12       1.039322
#> 4  1 male 66.15955 asian    1.001175 2019-06-02 00:49:23 2019-06-08 10:38:23                   NA   18       1.217020
#> 5  1 male 66.15955 asian    1.001175 2019-06-02 00:49:23 2019-06-08 10:38:23                   NA   24       1.217020
#> 6  1 male 66.15955 asian    1.001175 2019-06-02 00:49:23 2019-06-08 10:38:23                   NA   30       1.217020
#>   outcome_cr_max_24 med_acetaminophen_sum_168 med_aspirin_sum_168 med_diphenhydramine_sum_168 cr_length_06 cr_length_12 cr_max_06
#> 1          1.217020                         0                   0                           0            1            1  1.003659
#> 2          1.217020                         0                   0                           0            0            1  1.003659
#> 3          1.217020                         1                   0                           0            1            0  1.039322
#> 4          1.179722                         1                   0                           0            2            1  1.217020
#> 5          1.274939                         1                   0                           0            1            2  1.179722
#> 6          1.274939                         1                   0                           0            3            1  1.165989
#>   cr_max_12 cr_mean_06 cr_mean_12 cr_median_06 cr_median_12 cr_min_06 cr_min_12
#> 1  1.030098   1.003659   1.030098     1.003659     1.030098 1.0036587  1.030098
#> 2  1.003659   1.003659   1.003659     1.003659     1.003659 1.0036587  1.003659
#> 3        NA   1.039322         NA     1.039322           NA 1.0393216        NA
#> 4  1.039322   1.109985   1.039322     1.109985     1.039322 1.0029506  1.039322
#> 5  1.217020   1.179722   1.109985     1.179722     1.109985 1.1797219  1.002951
#> 6  1.179722   1.069630   1.179722     1.096827     1.179722 0.9460735  1.179722
```

## Testing time\_frame without writing output to files

If you want to simply test `time_frame`, you may prefer not to write
your output to file. You can accomplish this by setting `output_file` to
`FALSE`.

``` r
tf %>% 
  add_rolling_predictors(variables = 'cr',
                         lookback = hours(12), 
                         window = hours(6), 
                         stats = c(mean = mean,
                                   min = min,
                                   max = max,
                                   median = median,
                                   length = length),
                         output_file = FALSE) %>% 
  head()
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 1540
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#>   id time cr_length_06 cr_length_12 cr_max_06 cr_max_12 cr_mean_06 cr_mean_12 cr_median_06 cr_median_12 cr_min_06 cr_min_12
#> 1  1    0            1            1  1.003659  1.030098   1.003659   1.030098     1.003659     1.030098 1.0036587  1.030098
#> 2  1    6            0            1  1.003659  1.003659   1.003659   1.003659     1.003659     1.003659 1.0036587  1.003659
#> 3  1   12            1            0  1.039322        NA   1.039322         NA     1.039322           NA 1.0393216        NA
#> 4  1   18            2            1  1.217020  1.039322   1.109985   1.039322     1.109985     1.039322 1.0029506  1.039322
#> 5  1   24            1            2  1.179722  1.217020   1.179722   1.109985     1.179722     1.109985 1.1797219  1.002951
#> 6  1   30            3            1  1.165989  1.179722   1.069630   1.179722     1.096827     1.179722 0.9460735  1.179722
```

## You can also supply a vector of variables

``` r
tf %>% 
  add_rolling_predictors(variables = c('cr', 'med_aspirin'),
                         lookback = weeks(1), 
                         stats = c(length = length),
                         output_file = FALSE) %>% 
  head()
#> Joining, by = "id"
#> Processing variables: cr, med_aspirin...
#> Allocating memory...
#> Number of rows in final output: 1540
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#>   id time cr_length_168 med_aspirin_length_168
#> 1  1    0             2                      0
#> 2  1    6             2                      0
#> 3  1   12             3                      0
#> 4  1   18             5                      0
#> 5  1   24             6                      0
#> 6  1   30             9                      0
```

## Category accepts regular expressions

``` r
tf %>% 
  add_rolling_predictors(category = 'lab|med',
                         lookback = hours(12), 
                         stats = c(length = length),
                         output_file = FALSE) %>% 
  head()
#> Joining, by = "id"
#> Processing category: lab|med...
#> Allocating memory...
#> Number of rows in final output: 1540
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#>   id time cr_length_12 med_acetaminophen_length_12 med_aspirin_length_12 med_diphenhydramine_length_12
#> 1  1    0            2                           0                     0                             0
#> 2  1    6            1                           0                     0                             0
#> 3  1   12            1                           1                     0                             0
#> 4  1   18            3                           1                     0                             0
#> 5  1   24            3                           0                     0                             0
#> 6  1   30            4                           0                     0                             0
```

## Let’s benchmark the performance on our package

### Running in parallel

``` r
benchmark_results = list()

# future::plan('multisession')

benchmark_results[['multisession']] = 
  microbenchmark::microbenchmark(
    tf %>% 
      add_rolling_predictors(variable = 'cr',
                             lookback = hours(48), 
                             window = hours(6), 
                             stats = c(mean = mean,
                                       min = min,
                                       max = max,
                                       median = median,
                                       length = length)),
    times = 1
  )
```

### Running in parallel with a chunk\_size of 20

``` r

tf_with_chunks = tf
tf_with_chunks$chunk_size = 20

benchmark_results[['multisession with chunk_size 20']] = 
  microbenchmark::microbenchmark(
    tf_with_chunks %>% 
      add_rolling_predictors(variable = 'cr',
                             lookback = hours(48), 
                             window = hours(6), 
                             stats = c(mean = mean,
                                       min = min,
                                       max = max,
                                       median = median,
                                       length = length)),
    times = 1
  )
#> Processing chunk # 1 out of 5...
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 270
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#> The output file was written to: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/chunk_1_rolling_predictors_variables_cr_2021_07_12_01_37_39.csv
#> Processing chunk # 2 out of 5...
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 294
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#> The output file was written to: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/chunk_2_rolling_predictors_variables_cr_2021_07_12_01_37_46.csv
#> Processing chunk # 3 out of 5...
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 309
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#> The output file was written to: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/chunk_3_rolling_predictors_variables_cr_2021_07_12_01_37_53.csv
#> Processing chunk # 4 out of 5...
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 345
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#> The output file was written to: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/chunk_4_rolling_predictors_variables_cr_2021_07_12_01_38_00.csv
#> Processing chunk # 5 out of 5...
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 322
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#> The output file was written to: C:\Users\KARAN_~1\AppData\Local\Temp\Rtmp6PiqU2/gpmodels_dir/chunk_5_rolling_predictors_variables_cr_2021_07_12_01_38_07.csv
```

### Running in serial

``` r
future::plan('sequential')

benchmark_results[['sequential']] = 
  microbenchmark::microbenchmark(
  tf %>% 
    add_rolling_predictors(variable = 'cr',
                           lookback = hours(48), 
                           window = hours(6), 
                           stats = c(mean = mean,
                                     min = min,
                                     max = max,
                                     median = median,
                                     length = length)),
  times = 1
  )
```

## Benchmark results

``` r
benchmark_results
#> $multisession
#> Unit: seconds
#>                                                                                                                                                                                   expr
#>  tf %>% add_rolling_predictors(variable = "cr", lookback = hours(48),      window = hours(6), stats = c(mean = mean, min = min, max = max,          median = median, length = length))
#>       min       lq     mean   median       uq      max neval
#>  32.66706 32.66706 32.66706 32.66706 32.66706 32.66706     1
#> 
#> $`multisession with chunk_size 20`
#> Unit: seconds
#>                                                                                                                                                                                               expr
#>  tf_with_chunks %>% add_rolling_predictors(variable = "cr", lookback = hours(48),      window = hours(6), stats = c(mean = mean, min = min, max = max,          median = median, length = length))
#>       min       lq     mean   median       uq      max neval
#>  33.79288 33.79288 33.79288 33.79288 33.79288 33.79288     1
#> 
#> $sequential
#> Unit: seconds
#>                                                                                                                                                                                   expr
#>  tf %>% add_rolling_predictors(variable = "cr", lookback = hours(48),      window = hours(6), stats = c(mean = mean, min = min, max = max,          median = median, length = length))
#>       min       lq     mean   median       uq      max neval
#>  127.0335 127.0335 127.0335 127.0335 127.0335 127.0335     1
```
