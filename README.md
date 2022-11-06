
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggEvalue: plot E value using ggplot2.

## :bar_chart: Overview

The [ggEvalue](need%20a%20link) can calculate the E value and 95% CI,
then plot it.

## :arrow_double_down: Installation

You can install the development version of **ggEvalue** from Github
with:

``` r
devtools::install_github('lizhiwei1994/ggEvalue')
```

## :beginner: Usage

Say that we have a HR and 95% CI corresponding to **1.07 (1.05, 1.09)**.

We can get E value curve by running the code below.

``` r
library(ggEvalue)
# HR and 95% CI: 1.07 (1.05, 1.09)
estimate = 1.07
lo       = 1.05
hi       = 1.09

ggEvalue(estimate, lo, hi)
```

<img src="man/figures/unnamed-chunk-2-1.png" width="100%" />

As you can see, the text overlap in the above figure. We can set the
range of x and y axis to show clearer text.

``` r
ggEvalue(estimate, lo, hi, 
         xlim = c(1,2), ylim = c(1,2))
```

<img src="man/figures/unnamed-chunk-3-1.png" width="100%" />

Now, the text can be clearly shown in the figure.

If you want to change the color of lines, points and text in the figure,
you can do the following.

The first color is for 95% CI’s line, point and text. The second is for
E value.

``` r
ggEvalue(estimate, lo, hi, 
         xlim = c(1,2), ylim = c(1,2),
         point.col = c('red', 'blue'),
         line.col = c('red', 'blue'),
         text.col = c('red', 'blue'))
```

<img src="man/figures/unnamed-chunk-4-1.png" width="100%" />

Since `ggEvalue` returns a ggplot object, you can use `+` to add other
ggplot plotting functions.

``` r
library(ggplot2)

p = ggEvalue(estimate, lo, hi, 
         xlim = c(1,2), ylim = c(1,2),
         point.col = c('red', 'blue'),
         line.col = c('red', 'blue'),
         text.col = c('red', 'blue'))

p + labs(title = 'This is a title')
```

<img src="man/figures/unnamed-chunk-5-1.png" width="100%" />

``` r

p + theme_gray()
```

<img src="man/figures/unnamed-chunk-5-2.png" width="100%" />

## :page_with_curl: About Author

Zhiwei Li (<lizhiwei@ccmu.edu.cn>)

Department of Epidemiology and Health Statistics

School of Public Health, Capital Medical University

No.10 Xitoutiao, Youanmen Wai Street

Beijing, 100069

## :page_with_curl: Citation

If my R package is useful to you, please cite it.

Li Z (2022). *ggEvalue: plot E value using ggplot2.* R package version
0.1.0.

``` r
@Manual{,
  title = {ggEvalue: plot E value using ggplot2},
  author = {Zhiwei Li},
  year = {2022},
  note = {R package version 0.1.0},
}
```
