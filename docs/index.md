---
title: "ITEMS: Longitudinal Data Analysis"
author: "Tessa L. Johnson"
date: "2020-07-12"
output: 
   bookdown::gitbook:
     self_contained: TRUE
     split_by: "section+number"
github-repo: tessaleejohnson/ITEMS-Longitudinal
url: 'http\://tessaleejohnson.github.io/ITEMS-Longitudinal/'
description: "ITEMS module for learning longitudinal data analysis in R."
---



# NLSY Example

In this example, we will be using data from the National Longitudinal Survey of Youth (NLSY). The current subset of data comes from assessments administered to mother-child pairs starting in 1986. Data were restricted to children ages 6-8 with complete data at the first wave of measurement. Data were retained from only one child per mother.

More information about the example data can be found here:

*  [Curran, P. J. (1997). Comparing three modern approaches to longitudinal data analysis: An examination of a single developmental sample. In _Symposium conducted at the meeting of the Society for Research on child Development, Washington, DC._](https://jflournoy.github.io/assets/pdf/srcdmeth.pdf)

# Getting Started

In this worked example, we will follow the guidelines presented in the [ITEMS](https://ncme.elevate.commpartners.com/) Longitudinal Data Analysis module. The basic structure of this guided example is as follows:

*  [Loading and manipulating data][Getting Started]
*  [Performing exploratory data analysis][Exploratory Data Analysis]
*  [Conducting mixed-effects modeling using `nlme::lme`][Model Fitting]
*  [Graphically evaluating modeling assumptions][Model Diagnostics]

Throughout these sections, we present example code for producing display-quality tables and plots for different components of the longitudinal data analysis. While there are several examples in this guide, there are even more ways to examine and present longitudinal data. The documentation for [`ggplot2`](https://ggplot2.tidyverse.org/) and [`gt`](https://gt.rstudio.com/) are great places to go for questions about producing plots and tables, respectively, and the documentation for the [`nlme`](https://cran.r-project.org/web/packages/nlme/nlme.pdf) package is the main resource for questions mixed effects model code. In addition, interested readers will find many helpful examples in Pinhero & Bates (2000):

*  Pinheiro, J., & Bates, D. (2006). _Mixed-effects models in S and S-PLUS._ Springer Science & Business Media.

## Load Data 

To load the data, we use the `readr::read_csv` function. The `na = "-99"` statement tells `readr` how to recode missing data.

During this step, there are a few variables we would like to rename to facilitate pivoting (converting from wide format to long format) later on, so we write a renaming function (`insert_underscore`). The renaming function inserts a "_" character after the initial set of alphabet characters in the original column name but before any non-alphabet characters. The variables we will rename include:

*  ag(1-4) = age at each wave; 
*  an(1-4) = anti-social behavior score at each wave; and 
*  r(1-4) = reading score at each wave.
  
Last, we convert the variable "gen" (child's assigned sex) to a factor variable with the following levels:

*  0 = "female" and
*  1 = "male".


```r
##--TUTORIAL DATASETS--##

# read data
path_dat <- system.file(
  "extdata",
  "anti-read.csv",
  package = "ITEMSlme",
  mustWork = TRUE
)

# renaming function
insert_underscore <- function(names){
  sub("^([[:alpha:]]+)", "\\1_", names)
}

# read in the data with readr::read_csv
dat_wide <- path_dat %>%
  readr::read_csv(., na = "-99", col_types = readr::cols()) %>%
  dplyr::rename_with(
    insert_underscore,
    dplyr::starts_with(c("ag", "an", "r"))
  ) %>%
  dplyr::mutate(
    gen = forcats::fct_recode(as.factor(gen), female = "0", male = "1")
  ) %>%
  print(.)
```

```
## # A tibble: 405 x 17
##       id gen   momage homecog homeemo  an_1  an_2  an_3  an_4   r_1   r_2   r_3
##    <dbl> <fct>  <dbl>   <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1     1 male      27       7      11     1     0     1     0    27    49    50
##  2     2 male      27      10       7     1     1     0     1    31    47    56
##  3     3 fema~     27       7       7     5     0     5     3    36    52    60
##  4     4 male      24       8       8     1     1    NA    NA    18    30    NA
##  5     5 male      26       8       8     2     3     3     1    23    49    NA
##  6     6 fema~     25       6      11     1     0     0     0    21    NA    45
##  7     7 fema~     22       5       5     3    NA     0    10    21    NA    48
##  8     8 fema~     23       1       4     0    NA     0     4    13    NA    37
##  9     9 fema~     24       3       7     5     3     2     0    29    NA    35
## 10    10 fema~     28       9      11     2     3     6     5    45    58    76
## # ... with 395 more rows, and 5 more variables: r_4 <dbl>, ag_1 <dbl>,
## #   ag_2 <dbl>, ag_3 <dbl>, ag_4 <dbl>
```

## Wide to Long 

Because we have 3 repeated measures variables ("ag", "an" and "r"), we use the "names_pattern" argument of `tidyr::pivot_longer`. "names_pattern" uses a regular expression "(.+)_(.+)" in combo with the "names_to" argument c(".value", "wave") to save all of the data from each repeated measures variable under the names "an", "r", and "ag". The new "wave" variable contains information about when each measurement was taken, drawing its values from the number at the end of the original column name. For example, all of the data from the original column "an_2" will be stored in the new "an" variable with a "wave" code of "2". Because there are 4 waves of measurement, there will now be 4 rows for each person with complete data (n.b., children with missing data will have fewer rows), which you can double check by looking at the "id" column.
  
In the `dplyr::mutate` step, we subtract 1 from each value of "wave" because we want our initial measurement to be coded "0". This will have implications for the interpretation of the intercept when we move to the modeling phase.
  
Finally, we rename all our variables to something more informative. This step isn't strictly necessary, but often it is useful for revisiting and collaborating on code to use informative variable names.


```r
# pivot wide to long
dat_long <- dat_wide %>%
  tidyr::pivot_longer(
    cols = c(dplyr::contains("_")),
    names_to = c(".value", "wave"),
    names_pattern = c("(.+)_(.+)")
  ) %>%
  dplyr::mutate(., wave = as.numeric(wave) - 1) %>%
  dplyr::rename(
    person_id = id,
    assigned_sex = gen,
    mom_age = momage,
    home_cog = homecog,
    home_emo = homeemo,
    anti_score = an,
    read_score = r,
    child_age = ag
  ) %>%
  print(.)
```

```
## # A tibble: 1,620 x 9
##    person_id assigned_sex mom_age home_cog home_emo  wave anti_score read_score
##        <dbl> <fct>          <dbl>    <dbl>    <dbl> <dbl>      <dbl>      <dbl>
##  1         1 male              27        7       11     0          1         27
##  2         1 male              27        7       11     1          0         49
##  3         1 male              27        7       11     2          1         50
##  4         1 male              27        7       11     3          0         NA
##  5         2 male              27       10        7     0          1         31
##  6         2 male              27       10        7     1          1         47
##  7         2 male              27       10        7     2          0         56
##  8         2 male              27       10        7     3          1         64
##  9         3 female            27        7        7     0          5         36
## 10         3 female            27        7        7     1          0         52
## # ... with 1,610 more rows, and 1 more variable: child_age <dbl>
```

# Exploratory Data Analysis 

We choose age here rather than wave because children were all different ages at the first wave of measurement and waves were not completed at the exact same time for each child (meaning that some children were measured every 2 years, while others were measured at wave 1, then 3 years later for wave 2, then 2 years later for wave 3, etc.). Anti-social behavior trajectory may have a developmental component that would get masked if we looked across waves rather than age.

During this section, we showcase both the `dplyr::group_by` and `dplyr::summarize` functions, which help us calculate descriptive statistics for each level of `child_age`. Then, we demonstrate the `gt` package, used for producing display-quality tables of results.

## Missingness 

Like many longitudinal datasets, the data used in this example contain missingness. When using mixed-effects models, it is important to understand where and how missing observations appear. As we learned in [Getting Started], the current data were restricted to children with complete data at the first wave only, though children could appear in the dataset if they were missing data on the second, third, or fourth wave of data collection. To better understand missing data on the outcome, anti-social behavior, we provide examples of four different visualization plots.

### Missing Data Setup

Because we plan to analyze our data by *age* and not *wave*, let's first get a sense of how many anti-social scores are missing at each age level. We know that children were measured up to four times each, but children ranged from ages 6 - 15 in the dataset. Even though we don't have an anti-social behavior score for each child at each age (6, 7, 8, 9, 10...), we know that *theoretically* each child could have contributed an anti-social behavior score at each age.

Using the `pivot` functions from the `tidyr` package, we can identify missing data at each age for each child:


```r
dat_miss <- dat_long %>%
  dplyr::select(., person_id, child_age, anti_score) %>%
  dplyr::filter(., !is.na(child_age)) %>%
  tidyr::pivot_wider(
    names_from = "child_age",
    values_from = c("anti_score")
  ) %>%
  tidyr::pivot_longer(
    cols = -person_id,
    names_to = "child_age",
    values_to = "anti_score"
  ) %>%
  dplyr::mutate(
    child_age = as.numeric(child_age),
    anti_miss = dplyr::if_else(is.na(anti_score), 1, 0)
  ) %>%
  dplyr::left_join(., dat_long) %>%
  dplyr::select(., person_id, child_age, wave, anti_score, anti_miss) %>%
  dplyr::mutate(., fill_pal = dplyr::case_when(
    !is.na(wave) ~ wave,
    TRUE ~ 4
  )) %>%
  dplyr::arrange(., anti_miss, child_age, person_id) %>%
  print(.)
```

```
## Joining, by = c("person_id", "child_age", "anti_score")
```

```
## # A tibble: 4,050 x 6
##    person_id child_age  wave anti_score anti_miss fill_pal
##        <dbl>     <dbl> <dbl>      <dbl>     <dbl>    <dbl>
##  1         1         6     0          1         0        0
##  2         5         6     0          2         0        0
##  3         6         6     0          1         0        0
##  4         8         6     0          0         0        0
##  5        11         6     0          1         0        0
##  6        14         6     0          2         0        0
##  7        17         6     0          2         0        0
##  8        23         6     0          0         0        0
##  9        26         6     0          1         0        0
## 10        29         6     0          1         0        0
## # ... with 4,040 more rows
```

### Lollipop Plot 

If children were measured 4 times over 10 possible ages, we could reasonably expect to be missing anti-social behavior scores at a rate of 60\% in each age group, assuming that missingness were distributed uniformly and that children were all measured at evenly spaced intervals. Let's explore this assumption using what is known as a "lollipop plot". We include a horizontal dashed line to indicate the 60\% threshold.

We can also use this plot to highlight places where missingness is much higher (or lower) than what we expect using colors and labels. In plotting, we find that at age 15, about 98\% of possible anti-social behavior scores are missing, suggesting that most children didn't reach age 15 by the end of the study.


```r
# helper for axis limits
age_lim <- c(min(dat_long$child_age, na.rm = TRUE),
             max(dat_long$child_age, na.rm = TRUE))

# adjust dat_miss for lollipop plot
dat_lollipop <- dat_miss %>%
  dplyr::group_by(., child_age) %>%
  dplyr::summarise(
    pct_miss = mean(anti_miss, na.rm = TRUE) * 100
  )
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# plot
dat_lollipop %>%
  ggplot2::ggplot(
    ggplot2::aes(x = child_age, y = pct_miss)
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(x = child_age, xend = child_age, y = 0, yend = pct_miss),
    color = dplyr::if_else(
      dat_lollipop$pct_miss > 95, 
      viridis::plasma(n = 4, end = 0.75)[4], 
      "gray25"
    ), 
    size = dplyr::if_else(dat_lollipop$pct_miss > 95, 1.3, 0.7)
  ) +
  ggplot2::geom_point(
    color = dplyr::if_else(
      dat_lollipop$pct_miss > 95, 
      viridis::plasma(n = 4, end = 0.75)[4], 
      "gray25"
    ), 
    size = dplyr::if_else(dat_lollipop$pct_miss > 95, 5, 2)
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      x = child_age - 0.5,
      y = pct_miss - 5,
      label = paste0(round(pct_miss, 1), "%")
    ),
    data = dat_lollipop %>% dplyr::filter(pct_miss > 95),
    vjust = 1,
    hjust = 0.5
  ) +
  ggplot2::geom_curve(
    ggplot2::aes(
      x = child_age - 0.5, 
      y = pct_miss - 5, 
      xend = child_age, 
      yend = pct_miss),
    data = dat_lollipop %>% dplyr::filter(pct_miss > 95),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc")),
    curvature = -0.5
  ) +
  ggplot2::geom_hline(yintercept = 60, color = "gray50", linetype = "dashed") +
  ggplot2::scale_x_continuous(
    limits = c(age_lim[1] - .5, age_lim[2] + .5),
    breaks = seq(age_lim[1], age_lim[2], 1)
  ) +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::xlab("Child Age") +
  ggplot2::ylab("Percent Missing") +
  ggplot2::ggtitle("Percent of Children Missing Anti-Social Scores by Age")
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_miss-lolli-1.png" width="672" />

### Bar Plot by Pattern

Another helpful way to examine missingness in longitudinal data is to look at the *patterns* of missingness. We can think about this like within-person attrition. In other words, for each person, we calculate the percent of measurement waves they participated in out of the possible 4. There are four possible options in our situation (we ignore order to keep the example simple):

1.  anti-social data are present for each of the four waves,
1.  anti-social data are present for 3 of the four waves (we ignore order, but you could differentiate between individuals who have data for Waves 1, 2, and 3 and individuals who have data for Waves 1, 3, and 4),
1.  anti-social data are present for 2 of the four waves (again, we ignore order), and
1.  anti-social data are present for 1 wave only (in our case, due to the way our sample was defined, we know that people with only 1 wave of data contributed to the first wave).


```r
dat_attrit_l <- dat_miss %>%
  dplyr::group_by(., person_id) %>%
  dplyr::summarise(
    total_waves = sum(1 - anti_miss)
  ) %>%
  dplyr::group_by(., total_waves) %>%
  dplyr::summarise(
    ct_waves = dplyr::n(),
    pct_waves = dplyr::n() / NROW(dat_wide)
  )
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
dat_attrit_l %>%
  ggplot2::ggplot(.) +
  ggplot2::aes(
    x = forcats::fct_reorder(as.factor(total_waves), dplyr::desc(total_waves)),
    y = pct_waves * 100,
    fill = as.factor(total_waves)
  ) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(
    ggplot2::aes(
      y = pct_waves * 100 + 5,
      label = round(pct_waves * 100, 1)
    ),
    size = 3.5
  ) +
  ggplot2::scale_x_discrete(
    labels = c("4" = "Exactly 4 Waves (All)",
               "3" = "Exactly 3 Waves (Any)",
               "2" = "Exactly 2 Waves (Any)",
               "1" = "Exactly 1 Wave (1st)")
  ) +
  ggplot2::scale_fill_viridis_d(end = 0.75, option = "C") +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1,
      margin = ggplot2::margin(t = 0, r = 0, b = 10, l = 0)
    ),
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 10)
    )
  ) +
  ggplot2::ylim(0, 100) +
  ggplot2::xlab("Number of Data Collection Waves Completed") +
  ggplot2::ylab("Percent of Children \n per Attrition Category") +
  ggplot2::ggtitle(
    expression(
      paste("Attrition Patterns of Anti-Social Behavior Scores ",
            "(", n["Wave 1"], " = 405)")
    )
  )
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_miss-attrit-l-1.png" width="672" />

### Bar Plot by Wave

In this next bar plot example, we display across-person attrition. Here, we are merely interested in what percent of our sample (n = 405) contributed anti-social data at each wave.


```r
dat_attrit_c <- dat_miss %>%
  dplyr::filter(., !is.na(wave)) %>%
  dplyr::group_by(., wave) %>%
  dplyr::summarise(
    total_obs = sum(1 - anti_miss),
    pct_obs = sum(1 - anti_miss) / NROW(dat_wide)
  )
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
dat_attrit_c %>%
  ggplot2::ggplot(.) +
  ggplot2::aes(
    x = as.factor(wave),
    y = pct_obs * 100,
    fill = as.factor(total_obs)
  ) +
  ggplot2::geom_bar(
    stat = "identity"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      y = pct_obs * 100 + 5,
      label = round(pct_obs * 100, 1)
    ),
    position = ggplot2::position_dodge(width = 0.9),
    size = 3.5
  ) +
  ggplot2::scale_x_discrete(
    labels = c("0" = "Wave 1",
               "1" = "Wave 2",
               "2" = "Wave 3",
               "3" = "Wave 4")
  ) +
  ggplot2::scale_fill_viridis_d(end = 0.75, option = "C") +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1,
      margin = ggplot2::margin(t = 0, r = 0, b = 10, l = 0)
    ),
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 10)
    )
  ) +
  ggplot2::xlab("Measurement Wave") +
  ggplot2::ylab("Percent of Children \n Responding within Each Wave") +
  ggplot2::ggtitle(
    expression(
      paste("Anti-Social Behavior Score Reporting Rates ",
            "(", n["Wave 1"], " = 405)")
    )
  )
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_miss-attrit-c-1.png" width="672" />

### Raster Plot

This last missingness plot example is called a "raster plot". Whereas the previous plots provided an aggregated glimpse of missing data in our dataset, the raster plot displays the missingness for each individual in our data. Using color to indicate the measurement wave and carefully ordering the data in the plot, we can make sense of a large amount of data all at once.


```r
dat_miss %>%
  ggplot2::ggplot(.) +
  ggplot2::aes(
    x = child_age,
    y = forcats::fct_inorder(as.factor(person_id)),
    fill = as.factor(fill_pal)
  ) +
  ggplot2::geom_raster(hjust = 0.5, vjust = 0.5) +
  ggplot2::scale_x_continuous(
    limits = c(age_lim[1] - .5, age_lim[2] + .5),
    breaks = seq(age_lim[1], age_lim[2], 1)
  ) +
  ggplot2::scale_fill_manual(
    name = "Wave",
    labels = c("1", "2", "3", "4", "Missing Score"),
    values = c(viridis::viridis(4, option = "C", end = 0.75), "grey90")
  ) +
  ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    legend.position = "bottom"
  ) +
  ggplot2::labs(
    x = "Child Age",
    y = "Child ID",
    title = "Missing Anti-Social Scores by Age and Wave"
  ) +
  ggplot2::coord_flip()
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_miss-raster-1.png" width="672" />

### Updated Dataset

In the step above, we learned that very few children were measured at age 15 (in fact, only 8 children were measured at age 15). For the purposes of this guided example, we will remove those 8 children from the analytic dataset to avoid estimation and convergence issues in modeling later on.

_*NOTE: There are better ways to deal with small sample sizes, but those methods are outside the scope of this module. We do not generally recommend deleting observations!*_


```r
# find ids for children who are 15 at wave 4
remove_id <- dat_long %>%
  dplyr::filter(., child_age == 15) %>%
  dplyr::select(., person_id) %>%
  unlist(.)
  
# remove ids identified in remove_id
dat_final <- dat_long %>%
  dplyr::filter(., !(person_id %in% remove_id))

# check new sample size
dat_final %>%
  dplyr::distinct(., person_id) %>%
  NROW(.)
```

```
## [1] 397
```

## Univariate Descriptives

Following our exploration of missing data, we proceed to calculate univariate descriptive statistics. Given what we learned about the distribution of data across age groups in the missing data section, let's move forward using the `dat_final` dataset created in [Updated Dataset].

### Univariate Setup

Using the `dat_final` long dataset, it is fairly straightforward to calculate the means, variances, and number of observations of the anti-social behavior scores within each age. Knowing the means and variances within each age group can help us see whether and how anti-social behavior changes on average over time. In addition, understanding whether the variances are changing over time can help us make modeling decisions later on.

Recall that our new sample size is 397, following the steps in [Updated Data]. Each child was measured up to four times (and we retained a row even when missing their anti-social behavior information), so summing the `n`, or number of observations, should yield 397 * 4 = 1588. Note that 249 observations have a child age listed as `NA`. This number tells us how many anti-social behavior scores were not recorded across all measurement waves and individuals. For more information on how the missing anti-social behavior scores are distributed across individuals, see [Raster Plot].


```r
# calculate descriptive statistics for each level of child's age
moment_tab <- dat_final %>%
  dplyr::group_by(., child_age) %>%
  dplyr::summarize(
    n = dplyr::n(),
    mean = round(mean(anti_score, na.rm = TRUE), 2),
    variance = round(var(anti_score, na.rm = TRUE), 2)) %>%
  dplyr::mutate(., child_age = dplyr::case_when(
    is.na(child_age) ~ "Missing",
    TRUE ~ as.character(child_age)
  )) %>%
  print(.)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 10 x 4
##    child_age     n   mean variance
##    <chr>     <int>  <dbl>    <dbl>
##  1 6           122   1.57     2.78
##  2 7           168   1.55     2.42
##  3 8           138   2.04     3.26
##  4 9           192   1.89     3.82
##  5 10          149   2.15     4.59
##  6 11          169   1.79     3.81
##  7 12          131   1.84     3.07
##  8 13          169   2.25     5.19
##  9 14          101   1.96     4.36
## 10 Missing     249 NaN       NA
```

### Univariate Table

Now that we've calculated our univariate descriptive statistics, we will use the `gt` package to create a display-quality table of results. The `gt` package has a similar ethos to `ggplot2`, where each component of a table can be added on to the previous component. Although the intricacies of both `gt` and `ggplot2` are beyond the scope of this module, we thought it was important to demonstrate ways to create reproducible tables and plots for longitudinal data.


```r
gt_moments <- moment_tab %>%
  gt::gt(.) %>%
  gt::tab_header(
    data = .,
    title = "Univariate Descriptive Statistics for Anti-Social Behavior Scores",
    subtitle = "Means, variances, & number of observations within each age."
  ) %>%
  gt::tab_spanner(
    data = .,
    label = "Summary of Anti-Social Scores by Age", 
    columns = c("n", "mean", "variance")
  ) %>%
  gt::cols_label(
    child_age = "Child Age",
    n = "Num. Obs.",
    mean = "Mean",
    variance = "Variance"
  ) %>%
  gt::tab_options(., row.striping.include_table_body = FALSE) %>%
  gt::tab_style(
    data = .,
    style = gt::cell_borders(
      sides = c("top", "bottom"),
      color = "#ffffff",
      weight = gt::px(0),
      style = "solid"
    ),
    locations = gt::cells_body(
      columns = dplyr::everything(),
      rows = dplyr::everything())
    ) %>%
  gt::cols_align(., align = "center", columns = TRUE) %>%
  gt::fmt_missing(., columns = c("n", "mean", "variance"))

# print plot
gt_moments
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#alfczchymv .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#alfczchymv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#alfczchymv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#alfczchymv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#alfczchymv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#alfczchymv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#alfczchymv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#alfczchymv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#alfczchymv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#alfczchymv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#alfczchymv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#alfczchymv .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#alfczchymv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#alfczchymv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#alfczchymv .gt_from_md > :first-child {
  margin-top: 0;
}

#alfczchymv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#alfczchymv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#alfczchymv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#alfczchymv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#alfczchymv .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#alfczchymv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#alfczchymv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#alfczchymv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#alfczchymv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#alfczchymv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#alfczchymv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#alfczchymv .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#alfczchymv .gt_left {
  text-align: left;
}

#alfczchymv .gt_center {
  text-align: center;
}

#alfczchymv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#alfczchymv .gt_font_normal {
  font-weight: normal;
}

#alfczchymv .gt_font_bold {
  font-weight: bold;
}

#alfczchymv .gt_font_italic {
  font-style: italic;
}

#alfczchymv .gt_super {
  font-size: 65%;
}

#alfczchymv .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="alfczchymv" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="4" class="gt_heading gt_title gt_font_normal" style>Univariate Descriptive Statistics for Anti-Social Behavior Scores</th>
    </tr>
    <tr>
      <th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Means, variances, &amp; number of observations within each age.</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">Child Age</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="3">
        <span class="gt_column_spanner">Summary of Anti-Social Scores by Age</span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Num. Obs.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Variance</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">6</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">122</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.57</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">2.78</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">7</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">168</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.55</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">2.42</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">8</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">138</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">2.04</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">3.26</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">9</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">192</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.89</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">3.82</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">10</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">149</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">2.15</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">4.59</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">11</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">169</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.79</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">3.81</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">12</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">131</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.84</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">3.07</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">13</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">169</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">2.25</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">5.19</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">14</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">101</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.96</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">4.36</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Missing</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">249</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->



## Bivariate Descriptives

One limitation of long-format data is in calculating bivariate descriptive statistics like correlations and covariances. Tidyverse makes converting from long to wide and back fairly simple. To demonstrate, we take our long-format data and convert back to wide in order to calculate the correlation of anti-social behavior scores measured at different ages. These statistics can help us identify possible error covariance structures for our data, important for modeling later on.

Missing data note: Because children were measured generally every 2-3 years, each child will have "missing" data for their anti-social behavior score at ages when they weren't measured (in addition to non-response and other missing data mechanisms). Due to this structure, we treated missing data in the correlation matrix using pairwise complete observations. Missing data treatments like imputation are out of the scope of this module, but suffice it to say that there is a wealth of literature on missing data in longitudinal studies to help guide you.

### Bivariate Setup

Using the `pivot_wider` function like we did in [Univariate Setup], we can create a wide-format dataset with a column for each unique child age (e.g., 6, 7, 8, 9, ...). Then, calculating the correlation matrix proceeds in a straightforward manner. As with calculating the variances by age in [Univariate Setup], calculating the anti-social score correlations by age can help inform us about various modeling choices later.


```r
cor_tab <- dat_final %>%
  dplyr::select(., person_id, child_age, anti_score) %>%
  dplyr::filter(., !is.na(child_age)) %>%
  dplyr::arrange(., child_age, person_id) %>%
  tidyr::pivot_wider(
    names_from = "child_age",
    values_from = c("anti_score")) %>%
  dplyr::select(., -person_id) %>%
  corrr::correlate(., diagonal = 1, use = "pairwise", quiet = TRUE) %>%
  dplyr::mutate(., child_age = as.numeric(rowname), .before = rowname) %>%
  dplyr::select(., -rowname) %>%
  purrr::map_df(., ~format(round(.x, 2), nsmall = 2)) %>%
  print(.)
```

```
## Warning in stats::cor(x = x, y = y, use = use, method = method): the standard
## deviation is zero
```

```
## # A tibble: 9 x 10
##   child_age `6`    `7`    `8`    `9`    `10`   `11`    `12`    `13`  `14`  
##   <chr>     <chr>  <chr>  <chr>  <chr>  <chr>  <chr>   <chr>   <chr> <chr> 
## 1 " 6.00"   "1.00" "  NA" "0.49" "0.31" "0.57" " 0.30" " 0.09" 0.50  "  NA"
## 2 " 7.00"   "  NA" "1.00" "  NA" "0.51" "0.34" " 0.53" " 0.39" 0.38  "0.59"
## 3 " 8.00"   "0.49" "  NA" "1.00" "  NA" "0.56" " 0.27" " 0.44" 0.82  "0.44"
## 4 " 9.00"   "0.31" "0.51" "  NA" "1.00" "0.87" " 0.55" " 0.57" 0.58  "0.00"
## 5 "10.00"   "0.57" "0.34" "0.56" "0.87" "1.00" "   NA" " 0.56" 0.73  "0.55"
## 6 "11.00"   "0.30" "0.53" "0.27" "0.55" "  NA" " 1.00" "-0.15" 0.60  "  NA"
## 7 "12.00"   "0.09" "0.39" "0.44" "0.57" "0.56" "-0.15" " 1.00" 0.65  "0.47"
## 8 "13.00"   "0.50" "0.38" "0.82" "0.58" "0.73" " 0.60" " 0.65" 1.00  "0.84"
## 9 "14.00"   "  NA" "0.59" "0.44" "0.00" "0.55" "   NA" " 0.47" 0.84  "1.00"
```

### Bivariate Table

With a few small tweaks to the raw correlation matrix produced above, we can now use the `gt` package as [before][Univariate Table] to produce a display-quality correlation table. Note that the "---" symbol blocks off the upper triangle of the correlation matrix to reduce visual redundancy, and "NA" symbols within the lower triangle indicate correlations that could not be calculated due to missing data (for example, the correlation for anti-social behavior at age 6 and age 14 is missing because no children were measured at both age 6 and 14).


```r
# table results
cor_tab_upper <- cor_tab %>%
  dplyr::select(., -child_age)

cor_tab_upper[upper.tri(cor_tab_upper, diag = FALSE)] <- NA

gt_cor_tab <- cor_tab_upper %>%
  dplyr::mutate(., child_age = 6:14, .before = "6") %>%
  gt::gt(.) %>%
  gt::tab_header(
    data = ., 
    title = "Correlation of Anti-Social Scores Across Age",
    subtitle = "Complete pairwise observations were retained."
  ) %>%
  gt::tab_spanner(
    data = ., 
    label = "Self-Correlated Scores by Age", 
    columns = tidyselect::matches("\\d")
  ) %>%
  gt::cols_label(., child_age = "Child Age") %>%
  gt::fmt_missing(., columns = tidyselect::matches("\\d")) %>%
  gt::tab_options(., row.striping.include_table_body = FALSE) %>%
  gt::tab_style(
    data = ., 
    style = gt::cell_borders(
      sides = c("top", "bottom"),
      color = "#ffffff",
      weight = gt::px(0),
      style = "solid"
    ),
    locations = gt::cells_body(
      columns = dplyr::everything(),
      rows = dplyr::everything()
    )
  ) %>%
  gt::cols_align(., align = "center", columns = TRUE)

# print plot
gt_cor_tab
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#lxvnvoacrq .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#lxvnvoacrq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lxvnvoacrq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#lxvnvoacrq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#lxvnvoacrq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lxvnvoacrq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lxvnvoacrq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#lxvnvoacrq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#lxvnvoacrq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lxvnvoacrq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lxvnvoacrq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#lxvnvoacrq .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#lxvnvoacrq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#lxvnvoacrq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lxvnvoacrq .gt_from_md > :first-child {
  margin-top: 0;
}

#lxvnvoacrq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lxvnvoacrq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#lxvnvoacrq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#lxvnvoacrq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lxvnvoacrq .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#lxvnvoacrq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lxvnvoacrq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lxvnvoacrq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lxvnvoacrq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lxvnvoacrq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#lxvnvoacrq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lxvnvoacrq .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#lxvnvoacrq .gt_left {
  text-align: left;
}

#lxvnvoacrq .gt_center {
  text-align: center;
}

#lxvnvoacrq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lxvnvoacrq .gt_font_normal {
  font-weight: normal;
}

#lxvnvoacrq .gt_font_bold {
  font-weight: bold;
}

#lxvnvoacrq .gt_font_italic {
  font-style: italic;
}

#lxvnvoacrq .gt_super {
  font-size: 65%;
}

#lxvnvoacrq .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="lxvnvoacrq" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="10" class="gt_heading gt_title gt_font_normal" style>Correlation of Anti-Social Scores Across Age</th>
    </tr>
    <tr>
      <th colspan="10" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Complete pairwise observations were retained.</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">Child Age</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="9">
        <span class="gt_column_spanner">Self-Correlated Scores by Age</span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">6</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">7</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">8</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">9</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">10</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">11</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">12</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">13</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">14</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">6</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.00</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">7</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">  NA</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.00</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">8</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.49</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">  NA</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.00</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">9</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.31</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.51</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">  NA</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.00</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">10</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.57</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.34</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.56</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.87</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.00</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">11</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.30</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.53</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.27</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.55</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">  NA</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;"> 1.00</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">12</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.09</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.39</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.44</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.57</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.56</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">-0.15</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;"> 1.00</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">13</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.50</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.38</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.82</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.58</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.73</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;"> 0.60</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;"> 0.65</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.00</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">14</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">  NA</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.59</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.44</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.00</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.55</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">   NA</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;"> 0.47</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.84</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.00</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->



## Trajectories

In this section, we demonstrate two different ways to visualize longitudinal trajectories, or the changes in anti-social behavior score over time. Trajectory plots are very useful in conducting exploratory data analyses because they help us understand the functional form of the outcome. In other words, we can generally spot linearity and non-linearity in a plot. It is also important to plot both the aggregated and disaggregated change over time because the trajectory of the whole sample may not be the same as the trajectory for each individual.

### Aggregated Plot

First, we plot the aggregated means overlaid on individual plotted points of anti-social behavior by child's age. The plot is broken up into two frames based on child's assigned sex, and a linear curve is fitted to each trajectory.


```r
dat_final %>%
  dplyr::group_by(., child_age, assigned_sex) %>%
  dplyr::mutate(., mean = mean(anti_score, na.rm = TRUE)) %>%
  dplyr::ungroup(.) %>%
  ggplot2::ggplot(.) +
  ggplot2::aes(y = mean, x = child_age, color = assigned_sex) +
  ggplot2::geom_jitter(
    ggplot2::aes(y = anti_score, x = child_age),
    position = ggplot2::position_jitter(0.2), color = "lightgray"
  ) +
  ggplot2::stat_smooth(
    formula = y ~ x, 
    method = "lm", 
    se = FALSE,
    size = 1, 
    linetype = "dashed", 
    color = "black"
  ) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_line(size = 1.5) +
  ggplot2::xlab("Child's Age") +
  ggplot2::ylab("Antisocial Behavior Score") +
  ggplot2::ggtitle("Sample Mean Trajectories by Sex") +
  ggplot2::scale_color_viridis_d(name = "Sex", option = "C", end = 0.75) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 0, r = 0, b = 10, l = 0)
    ),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)
    ),
    legend.position = "none"
  ) +
  ggplot2::facet_wrap(dplyr::vars(assigned_sex))
```

```
## Warning: Removed 249 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 249 rows containing missing values (geom_point).

## Warning: Removed 249 rows containing missing values (geom_point).
```

```
## Warning: Removed 249 row(s) containing missing values (geom_path).
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_plot-means-1.png" width="672" />

### Individual Plot

Second, we plot the anti-social behavior trajectories of a random sample of 12 children. The plots are color-coded based on the child's assigned sex.


```r
# set seed
set.seed(654)

# randomly select individuals (ids) from dat_final
sample_id <- dat_final %>%
  dplyr::distinct(., person_id) %>%
  dplyr::sample_n(., size = 12) %>%
  unlist(.)

# filter dat_final by sampled ids and save plot
dat_final %>%
  dplyr::filter(., person_id %in% sample_id) %>%
  ggplot2::ggplot(.) +
  ggplot2::aes(
    y = anti_score,
    x = child_age, 
    group = person_id,
    color = assigned_sex
  ) +
  ggplot2::geom_smooth(
    lwd = 0.75, method = "lm", se = FALSE, linetype = "dashed", color = "black"
  ) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_line(lwd = 1) +
  ggplot2::xlab("Child's Age") +
  ggplot2::ylab("Antisocial Behavior Score") +
  ggplot2::ggtitle("Individual Trajectories Plot") +
  ggplot2::scale_color_viridis_d(name = "Sex", option = "C", end = 0.75) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 0, r = 0, b = 10, l = 0)
    ),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)
    ),
    legend.position = "bottom") +
  ggplot2::facet_wrap(dplyr::vars(person_id))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

```
## Warning: Removed 9 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 9 rows containing missing values (geom_point).
```

```
## Warning: Removed 9 row(s) containing missing values (geom_path).
```

```
## geom_path: Each group consists of only one observation. Do you need to adjust
## the group aesthetic?
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_spaghet-indv-1.png" width="672" />

# Model Fitting 

In the next section, we demonstrate the model-building process for mixed-effects models, including basic model comparisons. After fitting each of these models, we demonstrate how to summarize the global fit comparisons using the `gt` table package to help us identify our best "semi-unconditional" (that is, without predictors but including the time effects) model.

Once we have selected a semi-unconditional model, we add predictors to the model. In this analysis, we are interested in whether the trajectory of anti-social behavior is significantly impacted by a child's assigned sex after controlling for reading score, mom's age, home emotional stimulation, and home cognitive stimulation, which have been standardized. Last, we demonstrate how to summarize the model parameter estimates using the `gt` table package.

Modeling Tips:

*  Use "na.action = na.exclude" to allow for easier post-model evaluation when dataset contains missing data.
*  Include "control = list(maxIter = 100, returnObject = TRUE)" to facilitate model convergence.
*  Especially with larger sample sizes, use maximum likelihood estimation (method = "ML") rather than restricted maximum likelihood during model building; switch to restricted maximum likelihood for the final model.
*  The first argument of nlme::lme gives the structure of the fixed effects, the "random = " argument gives the structure of the random effects.
*  The "weights = " argument specifies the structure of the level-1 error variances; the "correlation = " argument specifies the structure of the level-1 error covariances.

## Unconditional 

In this set of code, we use the `nlme::lme` function to find the "best" random effects and variance/covariance structure for modeling our anti-social behavior scores over time.

While there is a growing interest in the R community in writing functions to draw statistical modeling packages into the tidyverse (see [Tidymodels](https://www.tidymodels.org/)), wrappers for the older `nlme` package, authored by José Pinheiro and Douglas Bates for the S and S-PLUS languages originally, have received less attention. In other words, the output of these functions is a little hairy. The helper package [`broom.mixed`](https://cran.r-project.org/web/packages/broom.mixed/index.html) is built for "tidying" the output of statistical packages and has some methods built for `lme` objects (that is, the result of the `nlme::lme` function). We will use the `broom.mixed` package along with the `anova` function from the built-in `stats` package to do the heavy lifting of displaying model results and model comparisons.

(Note: `broom.mixed` also has a function for extracting tidy global model fit statistics -- `broom.mixed::glance` -- but it is just as easy to tidy up the results from the `anova` function.)

### Intraclass Correlation

Before we start modeling, a common approach in mixed effects models is to calculate the ICC, or intra-class correlation coefficient. This coefficient falls between 0 and 1 and tells us the proportion of variance in the intercept that exists at level 2 relative to the total variance in the model.

In a longitudinal analysis, the ICC is sometimes a contentious statistic to calculate. Because longitudinal models include some time effect, different viewpoints exist as to whether to include the time effect in the model used to estimate the ICC. We will follow recommendations by Lesa Hoffman (2015) to estimate the ICC based on the model with no predictors and without the time effect.

*  Hoffman, L. (2015). _Longitudinal analysis: Modeling within-person fluctuation and change._ Routledge.

Following this method, the estimated intercept for this model yields the grand mean of all of the children's mean anti-social behavior scores. In other words, if we calculated an average anti-social score for each child across all of their measurements and then averaged those averages, we would be pretty close to the estimated intercept for the `null_icc` model. In that case, the interpretation of the ICC is the amount of variability in the person-average anti-social scores. A large ICC (closer to 1) would tell us that there is substantial variability in the average anti-social score across children; a smaller ICC (closer to 0) would tell us that children are similar in average anti-social behavior score.


```r
##--MODEL FITTING--##

# unconditional (null) model
null_icc <- dat_final %>%
  nlme::lme(
    anti_score ~ 1, 
    data = .,
    na.action = na.exclude,
    method = "ML",
    random = ~ 1 | person_id,
    control = list(maxIter = 100, returnObject = TRUE)
  )

# calculate the ICC
null_icc %>%
  calc_icc(.)
```

```
## [1] 0.4784052
```

### Unconditional Fits

To select our "best" semi-unconditional model, we proceed as follows:

1.  We start with a fully unconditional, intercept-only model (`null_icc`).
1.  Then, we add a fixed effect for our time variable, child's age (`null_fslp`).
1.  Next, we add a random effect for the slope of the time variable (`null_rslp`).
1.  Retaining the fixed and random effects for time, we then explore heterogeneous error variance structure (`null_hvar`).
1.  Keeping the fixed and random effects for time but removing the heterogeneous error variances, we explore compound symmetric error covariance structure (`null_csym`).

Our model building strategy was largely guided by the information we gained from exploratory data analysis. First, we assumed a linear trajectory for the outcome over time in `null_fslp` based on the plots created in [Trajectories]. Noting the wide variability in slopes and intercepts in the [spaghetti plots][Individual Plot] helped us choose to add a random effect for our time variable to the model in `null_rslp`.

In addition, by calculating the variances and correlations across ages in [Univariate Descriptives] and [Bivariate Descriptives], respectively, we were able to make better decisions about how to model the error variance/covariance structure. Knowing that the variances increased slightly though remained fairly similar across age groups, we decided to try out a heterogeneous error variance structure in `null_hvar` using the `weights = nlme::varIdent()` argument. In addition, the pattern of correlations across ages suggested that a compound symmetric error structure might improve our model fit, which we added to the `null_csym` model using the `correlation = nlme::corCompSymm` argument.


```r
# unconditional model with fixed slope for time var
null_fslp <- dat_final %>%
  nlme::lme(
    anti_score ~ 1 + child_age, 
    data = .,
    na.action = na.exclude,
    method = "ML",
    random = ~ 1 | person_id,
    control = list(maxIter = 100, returnObject = TRUE)
  )

# unconditional model with random slope for time var
null_rslp <- dat_final %>%
  nlme::lme(
    anti_score ~ 1 + child_age, 
    data = .,
    na.action = na.exclude,
    method = "ML",
    random = ~ 1 + child_age | person_id,
    control = list(maxIter = 100, returnObject = TRUE)
  )

# unconditional model with heterogeneous variances
null_hvar <- dat_final %>%
  nlme::lme(
    anti_score ~ 1 + child_age, 
    data = .,
    na.action = na.exclude,
    method = "ML",
    random = ~ 1 + child_age | person_id,
    weights = nlme::varIdent(form = ~ 1 | child_age),
    control = list(maxIter = 100, returnObject = TRUE)
  )

# unconditional model with ar1 error covariance
null_csym <- dat_final %>%
  nlme::lme(
    anti_score ~ 1 + child_age,
    data = .,
    na.action = na.exclude,
    method = "ML",
    random = ~ 1 + child_age | person_id,
    correlation = nlme::corCompSymm(
      value = -.3, form = ~ 1 + child_age | person_id
    ),
    control = list(maxIter = 100, returnObject = TRUE)
  )

# example of the raw results from the nlme::lme package
null_csym %>%
  summary(.)
```

```
## Linear mixed-effects model fit by maximum likelihood
##  Data: . 
##        AIC      BIC    logLik
##   5206.916 5243.314 -2596.458
## 
## Random effects:
##  Formula: ~1 + child_age | person_id
##  Structure: General positive-definite, Log-Cholesky parametrization
##             StdDev    Corr  
## (Intercept) 1.1145000 (Intr)
## child_age   0.1463466 -0.316
## Residual    1.1692363       
## 
## Correlation Structure: Compound symmetry
##  Formula: ~1 + child_age | person_id 
##  Parameter estimate(s):
##        Rho 
## -0.2888797 
## Fixed effects: anti_score ~ 1 + child_age 
##                 Value  Std.Error  DF  t-value p-value
## (Intercept) 1.2067309 0.16699428 941 7.226181       0
## child_age   0.0729982 0.01779365 941 4.102486       0
##  Correlation: 
##           (Intr)
## child_age -0.893
## 
## Standardized Within-Group Residuals:
##        Min         Q1        Med         Q3        Max 
## -3.7925762 -0.5524850 -0.1116229  0.5110598  4.2607661 
## 
## Number of Observations: 1339
## Number of Groups: 397
```

### Global Comparisons Table

Using the `anova` function and `gt` package, we can built a display-quality table of global model fit statistics. As recommended in the ITEMS modules, we use the information criteria to compare non-nested models and models with different random effects structures. The "best" model, and the one we will move forward with when adding covariates in the next section, is the one with the lowest AIC and BIC (in this case, the `null_rslp` model with a random effect for time, homogenous error variances, and independent error covariance structure). All models are estimated with maximum likelihood during the model-building phase. 


```r
# comparing models
gt_global_tab <- anova(null_icc, null_fslp, null_rslp, null_hvar, null_csym) %>%
  tibble::as_tibble(.) %>%
  dplyr::mutate(
    Name = dplyr::case_when(
      Model == 1 ~ "Intercept-Only",
      Model == 2 ~ "Time (Fixed)",
      Model == 3 ~ "Time (Fixed and Random)",
      Model == 4 ~ "Heterogeneous Variances",
      Model == 5 ~ "Compound Symmetry"
    ),
    AIC = round(AIC, 2), 
    BIC = round(BIC, 2)
  ) %>%
  dplyr::select(., Model, Name, df, AIC, BIC) %>%
  gt::gt(.) %>%
  gt::tab_header(
    data = .,
    title = "Global Model Comparisons",
    subtitle = "Semi-unconditional models with fixed and random effects for time."
  ) %>%
  gt::tab_spanner(
    data = .,
    label = "Model Fit", 
    columns = c("df", "AIC", "BIC")
  ) %>%
  gt::tab_options(
    data = .,
    row.striping.include_table_body = FALSE
  ) %>%
  gt::tab_style(
    data = ., 
    style = gt::cell_borders(
      sides = c("top", "bottom"),
      color = "#ffffff",
      weight = gt::px(0),
      style = "solid"
    ),
    locations = gt::cells_body(
      columns = dplyr::everything(),
      rows = dplyr::everything()
    )
  ) %>%
  gt::cols_align(
    data = .,
    align = "center", columns = c("Model", "df", "AIC", "BIC")
  ) %>%
  gt::cols_align(., align = "left", columns = "Name")  %>%
  gt::tab_footnote(
    data = ., 
    footnote = paste(
      "Models 4 & 5 retain the fixed and random effects for time;", 
      "Model 4 does not contain compound symmetric covariance structure;",
      "Model 5 does not contain heterogeneous variances."
    ),
    locations = gt::cells_column_labels(columns = dplyr::vars(Name))) %>%
  gt::tab_footnote(
    data = ., 
    footnote = "df = Model degrees of freedom.",
    locations = gt::cells_column_labels(columns = dplyr::vars(df))
  )

# print plot
gt_global_tab
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#yigrsktgxg .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#yigrsktgxg .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yigrsktgxg .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#yigrsktgxg .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#yigrsktgxg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yigrsktgxg .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yigrsktgxg .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#yigrsktgxg .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#yigrsktgxg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#yigrsktgxg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#yigrsktgxg .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#yigrsktgxg .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#yigrsktgxg .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#yigrsktgxg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#yigrsktgxg .gt_from_md > :first-child {
  margin-top: 0;
}

#yigrsktgxg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#yigrsktgxg .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#yigrsktgxg .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#yigrsktgxg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yigrsktgxg .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#yigrsktgxg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yigrsktgxg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#yigrsktgxg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yigrsktgxg .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yigrsktgxg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#yigrsktgxg .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yigrsktgxg .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#yigrsktgxg .gt_left {
  text-align: left;
}

#yigrsktgxg .gt_center {
  text-align: center;
}

#yigrsktgxg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#yigrsktgxg .gt_font_normal {
  font-weight: normal;
}

#yigrsktgxg .gt_font_bold {
  font-weight: bold;
}

#yigrsktgxg .gt_font_italic {
  font-style: italic;
}

#yigrsktgxg .gt_super {
  font-size: 65%;
}

#yigrsktgxg .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="yigrsktgxg" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="5" class="gt_heading gt_title gt_font_normal" style>Global Model Comparisons</th>
    </tr>
    <tr>
      <th colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Semi-unconditional models with fixed and random effects for time.</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">Model</th>
      <th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">Name<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="3">
        <span class="gt_column_spanner">Model Fit</span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">df<sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">AIC</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">BIC</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1</td>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Intercept-Only</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">3</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">5254.55</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">5270.15</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">2</td>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Time (Fixed)</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">4</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">5239.87</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">5260.66</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">3</td>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Time (Fixed and Random)</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">6</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">5204.92</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">5236.11</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">4</td>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Heterogeneous Variances</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">14</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">5214.20</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">5286.99</td>
    </tr>
    <tr>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">5</td>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Compound Symmetry</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">7</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">5206.92</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">5243.31</td>
    </tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="5">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          Models 4 &amp; 5 retain the fixed and random effects for time; Model 4 does not contain compound symmetric covariance structure; Model 5 does not contain heterogeneous variances.
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>2</em>
          </sup>
           
          df = Model degrees of freedom.
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table></div><!--/html_preserve-->



## Conditional 

In this section of code, we add our predictor of interest, `assigned_sex`, and a slate of standardized covariates to the baseline "best" model identified in the [step above][Global Comparisons Table]: `null_rslp`. If we wanted to conduct further model comparisons for the covariates (e.g., testing the addition of random slopes for the predictors), we could proceed as we did in the section above. Once we have selected our final model, `cond_reml`, we switch the estimation to restricted maximum likelihood (`method = "REML"`).

### Conditional Fits

Let's add predictors to the linear mixed-effects model with random slope for `child_age`. The effects of `read_score`, `mom_age`, `home_emo`, and `home_cog` have been standardized for the purposes of interpretation.


```r
# conditional model
cond_full <- dat_final %>%
  nlme::lme(
    anti_score ~ 1 + child_age + assigned_sex +
      scale(read_score) + 
      scale(mom_age) + 
      scale(home_emo) +
      scale(home_cog),
    data = .,
    na.action = na.exclude,
    method = "ML",
    random = ~ 1 + child_age | person_id,
    control = list(maxIter = 100, returnObject = TRUE)
  )

# final model fit with REML
cond_reml <- dat_final %>%
  nlme::lme(
    anti_score ~ 1 + child_age + assigned_sex +
      scale(read_score) +
      scale(mom_age) +
      scale(home_emo) +
      scale(home_cog),
    data = .,
    na.action = na.exclude,
    method = "REML",
    random = ~ 1 + child_age | person_id,
    control = list(maxIter = 100, returnObject = TRUE)
  )

# example of raw output
cond_reml %>%
  summary(.)
```

```
## Linear mixed-effects model fit by REML
##  Data: . 
##        AIC      BIC    logLik
##   4877.071 4933.642 -2427.535
## 
## Random effects:
##  Formula: ~1 + child_age | person_id
##  Structure: General positive-definite, Log-Cholesky parametrization
##             StdDev    Corr  
## (Intercept) 0.6102073 (Intr)
## child_age   0.1278017 -0.329
## Residual    1.3217583       
## 
## Fixed effects: anti_score ~ 1 + child_age + assigned_sex + scale(read_score) +      scale(mom_age) + scale(home_emo) + scale(home_cog) 
##                        Value Std.Error  DF   t-value p-value
## (Intercept)        0.3045651 0.3178177 873  0.958301  0.3382
## child_age          0.1186889 0.0324837 873  3.653794  0.0003
## assigned_sexmale   0.8790356 0.1386554 392  6.339712  0.0000
## scale(read_score) -0.1453568 0.0863945 873 -1.682477  0.0928
## scale(mom_age)    -0.1049835 0.0719887 392 -1.458332  0.1455
## scale(home_emo)   -0.3439285 0.0731898 392 -4.699129  0.0000
## scale(home_cog)   -0.1250640 0.0725356 392 -1.724174  0.0855
##  Correlation: 
##                   (Intr) chld_g assgn_ scl(r_) scl(m_) scl(hm_m)
## child_age         -0.950                                        
## assigned_sexmale  -0.177 -0.044                                 
## scale(read_score)  0.825 -0.836  0.053                          
## scale(mom_age)    -0.033  0.069 -0.158 -0.090                   
## scale(home_emo)   -0.032  0.039 -0.036 -0.039  -0.148           
## scale(home_cog)   -0.069  0.055  0.077 -0.071  -0.185  -0.270   
## 
## Standardized Within-Group Residuals:
##        Min         Q1        Med         Q3        Max 
## -3.0993649 -0.5663559 -0.1545480  0.4730016  3.9019146 
## 
## Number of Observations: 1272
## Number of Groups: 397
```

### Parameter Estimates Table

Now, we create a table of parameter estimates for the final model (`cond_reml`), grouped by fixed and random effects (note that we have opted to remove p-values from our table and instead included lower and upper bounds of the 95\% confidence interval):


```r
# table results
gt_results_tab <- cond_reml %>%
  broom.mixed::tidy(., conf.int = TRUE) %>%
  dplyr::select(
    -tidyselect::any_of(c("effect", "group", "df", "statistic", "p.value"))
  ) %>%
  dplyr::mutate(., dplyr::across(where(is.numeric), ~round(., 2))) %>%
  dplyr::rename(
    Parameter = term,
    Estimate = estimate,
    SE = std.error,
    CI_low = conf.low,
    CI_high = conf.high) %>%
  dplyr::mutate(
    Parameter = dplyr::case_when(
      Parameter == "child_age" ~ "Child Age",
      Parameter == "assigned_sexmale" ~ "Male",
      Parameter == "scale(read_score)" ~ "Reading Score (std.)",
      Parameter == "scale(mom_age)" ~ "Mother's Age (std.)",
      Parameter == "scale(home_emo)" ~ "Home Emotional Stimulation (std.)",
      Parameter == "scale(home_cog)" ~ "Home Cognitive Stimulation (std.)",
      Parameter == "sd_(Intercept)" ~ "Std. Dev. (Intercept)",
      Parameter == "cor_child_age.(Intercept)" ~ "Corr. (Intercept. with Child Age)",
      Parameter == "sd_child_age" ~ "Std. Dev. (Child Age)",
      Parameter == "sd_Observation" ~ "Std. Dev. (Residual)",
      TRUE ~ Parameter
    )
  ) %>%
  gt::gt(.) %>%
  gt::tab_header(
    data = .,
    title = "Model Results",
    subtitle = "Conditional model with homogenous variances and independent error covariance."
  ) %>%
  gt::tab_spanner(
    data = .,
    label = "Model Estimates", 
    columns = c("Estimate", "SE", "CI_low", "CI_high")
  ) %>%
  gt::tab_options(., row.striping.include_table_body = FALSE) %>%
  gt::tab_style(
    data = .,
    style = gt::cell_borders(
      sides = c("top", "bottom"),
      color = "#ffffff",
      weight = gt::px(0),
      style = "solid"
    ),
    locations = gt::cells_body(
      columns = dplyr::everything(),
      rows = dplyr::everything()
    )
  ) %>%
  gt::cols_label(., CI_low = "CI (2.5%)", CI_high = "CI (97.5%)") %>%
  gt::cols_align(
    data = ., 
    align = "center", 
    columns = c("Estimate", "SE", "CI_low", "CI_high")
  ) %>%
  gt::cols_align(., align = "left", columns = "Parameter") %>%
  gt::tab_row_group(., group = "Random Effects", rows = 8:11) %>%
  gt::tab_row_group(., group = "Fixed Effects", rows = 1:7) %>%
  gt::fmt_missing(., columns = 2:5) %>%
  gt::tab_footnote(data = .,
    footnote = paste(
      "(std.) = Covariate has been standardized;",
      "Std. Dev. = Estimated standard deviation of the random effect;",
      "Corr. = Estimated correlation between level-2 random effects."
    ),
    locations = gt::cells_column_labels(columns = dplyr::vars(Parameter))
  )

# print plot
gt_results_tab
```

```
## Warning in min(rows_matched): no non-missing arguments to min; returning Inf
```

```
## Warning in max(rows_matched): no non-missing arguments to max; returning -Inf
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#opfazzmjrx .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#opfazzmjrx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#opfazzmjrx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#opfazzmjrx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#opfazzmjrx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opfazzmjrx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#opfazzmjrx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#opfazzmjrx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#opfazzmjrx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#opfazzmjrx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#opfazzmjrx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#opfazzmjrx .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#opfazzmjrx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#opfazzmjrx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#opfazzmjrx .gt_from_md > :first-child {
  margin-top: 0;
}

#opfazzmjrx .gt_from_md > :last-child {
  margin-bottom: 0;
}

#opfazzmjrx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#opfazzmjrx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#opfazzmjrx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#opfazzmjrx .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#opfazzmjrx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#opfazzmjrx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#opfazzmjrx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opfazzmjrx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#opfazzmjrx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#opfazzmjrx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#opfazzmjrx .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#opfazzmjrx .gt_left {
  text-align: left;
}

#opfazzmjrx .gt_center {
  text-align: center;
}

#opfazzmjrx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#opfazzmjrx .gt_font_normal {
  font-weight: normal;
}

#opfazzmjrx .gt_font_bold {
  font-weight: bold;
}

#opfazzmjrx .gt_font_italic {
  font-style: italic;
}

#opfazzmjrx .gt_super {
  font-size: 65%;
}

#opfazzmjrx .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="opfazzmjrx" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="5" class="gt_heading gt_title gt_font_normal" style>Model Results</th>
    </tr>
    <tr>
      <th colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Conditional model with homogenous variances and independent error covariance.</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">Parameter<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">
        <span class="gt_column_spanner">Model Estimates</span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Estimate</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">SE</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">CI (2.5%)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">CI (97.5%)</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">Fixed Effects</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">(Intercept)</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.30</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.32</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">-0.32</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.93</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Child Age</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.12</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.03</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.05</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.18</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Male</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.88</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.14</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.61</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.15</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Reading Score (std.)</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">-0.15</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.09</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">-0.31</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.02</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Mother's Age (std.)</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">-0.10</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.07</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">-0.25</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.04</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Home Emotional Stimulation (std.)</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">-0.34</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.07</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">-0.49</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">-0.20</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Home Cognitive Stimulation (std.)</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">-0.13</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.07</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">-0.27</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.02</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">Random Effects</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Std. Dev. (Intercept)</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.61</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.00</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">108.24</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Corr. (Intercept. with Child Age)</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">-0.33</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">-1.00</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.99</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Std. Dev. (Child Age)</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.13</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.04</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">0.44</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">Std. Dev. (Residual)</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">1.32</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
      <td class="gt_row gt_center" style="border-top-width: 0px; border-top-style: solid; border-top-color: #ffffff; border-bottom-width: 0px; border-bottom-style: solid; border-bottom-color: #ffffff;">&mdash;</td>
    </tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="5">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          (std.) = Covariate has been standardized; Std. Dev. = Estimated standard deviation of the random effect; Corr. = Estimated correlation between level-2 random effects.
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table></div><!--/html_preserve-->



# Model Diagnostics 

In this section, we demonstrate a few graphical model diagnostic techniques using the level-1 residuals and fitted values as well as the level-2 random effects from our final conditional model, `cond_reml`.

First, we combine the residuals and random effects estimates with the original dataset. Next, we create "base plots", which include the `ggplot2` elements that we will use in each plot. In this case, we split all of our diagnostic plots by levels of child's assigned sex simply for the purposes of demonstration.

Last, we briefly demonstrate graphical diagnostic plots for the following assumptions:

*  normality of residuals and random effects
*  homoskedasticity (constant variance) of residuals across groups
*  independence of level-1 and level-2 errors (level-2 random effects are not assumed independent from each other)

For more on assumption checking in mixed-effects models, see

*  Pinheiro, J., & Bates, D. (2006). _Mixed-effects models in S and S-PLUS._ Springer Science & Business Media.

(Note: Pinhero and Bates are also the authors of the `nlme` package!)

## Diagnostics Setup

### Merge Data

Using the `nlme::ranef`, `fitted`, and `residuals` functions, we can merge our original dataset (`dat_final`) with the output from our final estimated model, `cond_reml`. Note that we choose to use standardized residuals by requesting `type = "pearson"` from the `residuals` function. Recall that because the `dat_final` dataset contains missing observations for both the outcome (anti-social behavior) and predictors, we used the `na.exclude` option when fitting our models in [Conditional Fits]. If we hadn't done this, then we would get errors when trying to re-merge the residuals back with the original data.


```r
##--Data Prep--##
mod <- cond_reml

model_eval <- mod %>%
  nlme::ranef(., condVar = TRUE) %>%
  tibble::as_tibble(.) %>%
  dplyr::mutate(
    person_id = dat_final %>% dplyr::distinct(., person_id) %>% unlist(.),
    .before = "(Intercept)"
  ) %>%
  dplyr::rename(., resid_int = `(Intercept)`, resid_age = child_age) %>%
  dplyr::right_join(., dat_final, by = "person_id") %>%
  dplyr::mutate(
    resid_id = residuals(mod, type = "pearson"),
    fitted_id = fitted(mod)
  )
```

### Base Plots

To reduce repetition in plotting in the sections below, we create two baseplots for level-1 residual diagnostics and for level-2 random effects diagnostics. These base setups request that the printed plots will all have panels split and colored by child's assigned sex.


```r
## plot setup
g_l1 <- model_eval %>%
  ggplot2::ggplot(.) +
  ggplot2::aes(colour = assigned_sex, fill = assigned_sex) +
  ggplot2::scale_color_viridis_d(name = "Sex", option = "C", end = 0.75) +
  ggplot2::scale_fill_viridis_d(name = "Sex", option = "C", end = 0.75) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::facet_wrap(~assigned_sex)
  

g_l2 <- model_eval %>%
  dplyr::distinct(., person_id, .keep_all = TRUE) %>%
  ggplot2::ggplot(.) +
  ggplot2::aes(colour = assigned_sex, fill = assigned_sex) +
  ggplot2::scale_color_viridis_d(name = "Sex", option = "C", end = 0.75) +
  ggplot2::scale_fill_viridis_d(name = "Sex", option = "C", end = 0.75) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::facet_wrap(~assigned_sex)
```

## Normality Plots

In this code, we demonstrate how to plot histograms and qq-plots of our residuals and random effects to assess the normality assumption. In our histograms, we should see symmetric residuals centered around 0. The qq-plot helps us see where deviations from normality occur when the dots fail to align with the solid diagonal line.


```r
## Normality of Residuals

# level-1 residual histogram
g_l1 +
  ggplot2::aes(x = resid_id) +
  ggplot2::geom_histogram() +
  ggplot2::xlab("Standardized Residual Value") +
  ggplot2::ylab("Frequency") +
  ggplot2::ggtitle("Histogram of Standardized Level-1 Residuals")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 316 rows containing non-finite values (stat_bin).
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_check-normal-1.png" width="672" />

```r
# level-1 qq plot
g_l1 +
  ggplot2::aes(sample = resid_id, colour = assigned_sex) +
  ggplot2::geom_qq() +
  ggplot2::geom_qq_line() +
  ggplot2::xlab("Theoretical Quantiles") +
  ggplot2::ylab("Sample Quantiles") +
  ggplot2::ggtitle("QQ-Plot of Standardized Level-1 Residuals")
```

```
## Warning: Removed 316 rows containing non-finite values (stat_qq).
```

```
## Warning: Removed 316 rows containing non-finite values (stat_qq_line).
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_check-normal-2.png" width="672" />

```r
# level-2 random effect histogram (intercept)
g_l2 +
  ggplot2::aes(x = resid_int) +
  ggplot2::geom_histogram() +
  ggplot2::xlab("Random Effect") +
  ggplot2::ylab("Frequency") +
  ggplot2::ggtitle("Histogram of Level-2 Random Effects (Intercept)")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_check-normal-3.png" width="672" />

```r
# level-2 qq plot (intercept)
g_l2 +
  ggplot2::aes(sample = resid_int) +
  ggplot2::geom_qq() +
  ggplot2::geom_qq_line() +
  ggplot2::xlab("Theoretical Quantiles") +
  ggplot2::ylab("Sample Quantiles") +
  ggplot2::ggtitle("QQ-Plot of Level-2 Random Effects (Intercept)")
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_check-normal-4.png" width="672" />

```r
# level-2 random effect histogram (random time slope)
g_l2 +
  ggplot2::aes(x = resid_age) +
  ggplot2::geom_histogram() +
  ggplot2::xlab("Random Effect") +
  ggplot2::ylab("Frequency") +
  ggplot2::ggtitle("Histogram of Level-2 Random Effects (Time)")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_check-normal-5.png" width="672" />

```r
# level-2 qq plot (random time slope)
g_l2 +
  ggplot2::aes(sample = resid_age) +
  ggplot2::geom_qq() +
  ggplot2::geom_qq_line() +
  ggplot2::xlab("Theoretical Quantiles") +
  ggplot2::ylab("Sample Quantiles") +
  ggplot2::ggtitle("QQ-Plot of Level-2 Random Effects (Time)")
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_check-normal-6.png" width="672" />

## Homoskedasticity Plots

The assumption of homoskedasticity for multilevel models states that the level-1 residuals should be centered around 0 for each group (in our case, child), and that the variance of each child's scores is the same across children. We can assess this both by plotting the residuals against fitted values, looking for points to be randomly distributed across the plot. Using grouped boxplots, we can also check to see whether each person's box has the same size (because our sample size is large, the "boxes" will appear more like lines).


```r
## Homogeneity of Variance Assumption (Level 1)

# level-1 residual x fitted scatterplot
g_l1 +
  ggplot2::aes(x = fitted_id, y = resid_id) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::xlab("Fitted Value") +
  ggplot2::ylab("Standardized Residual") +
  ggplot2::ggtitle("Scatterplot of Fitted Values and Standardized Level-1 Residuals")
```

```
## Warning: Removed 316 rows containing missing values (geom_point).
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_check-homog-1.png" width="672" />

```r
# level-1 group boxplot
g_l1 +
  ggplot2::aes(
    x = as.factor(person_id), 
    y = resid_id, 
    group = as.factor(person_id)
  ) +
  ggplot2::geom_boxplot() +
  ggplot2::theme(
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank()
  ) + 
  ggplot2::xlab("Child ID") +
  ggplot2::ylab("Standardized Residual") +
  ggplot2::ggtitle("Grouped Boxplot of Standardized Level-1 Residuals")
```

```
## Warning: Removed 316 rows containing non-finite values (stat_boxplot).
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_check-homog-2.png" width="672" />

## Independence Plots

Last, we check the assumption of independence across level-1 and level-2 residuals/random effects. This assumption states that the level-1 residuals should not be related to the level-2 random effects. We assess this using scatter plots.

We also plot the level-2 random effects against one another. As shown in our [model results][Assessing Covariate Effects], the correlation between the level-2 intercept random effect and level-2 time random effect is not statistically different from 0, which we can assess visually using this scatter plot.


```r
## Independence of Residuals Assumption

# level-1 residuals & level-2 random effects (intercept)
g_l1 +
  ggplot2::aes(x = resid_id, y = resid_int) +
  ggplot2::geom_point() +
  ggplot2::xlab("Standardized Residual") +
  ggplot2::ylab("Random Effect (Intercept)") +
  ggplot2::ggtitle("Scatterplot of Standardized Level-1 Residuals and Level-2 Random Effects")
```

```
## Warning: Removed 316 rows containing missing values (geom_point).
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_check-indep-1.png" width="672" />

```r
## level-1 residuals & level-2 random effects (random time slope)
g_l1 +
  ggplot2::aes(x = resid_id, y = resid_age) +
  ggplot2::geom_point() +
  ggplot2::xlab("Standardized Residual") +
  ggplot2::ylab("Random Effect (Time)") +
  ggplot2::ggtitle("Scatterplot of Standardized Level-1 Residuals and Level-2 Random Effects")
```

```
## Warning: Removed 316 rows containing missing values (geom_point).
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_check-indep-2.png" width="672" />

```r
## level-2 random effects (intercept & random time slope) - not independent
g_l2 +
  ggplot2::aes(x = resid_age, y = resid_int) +
  ggplot2::geom_point() +
  ggplot2::xlab("Random Effect (Time)") +
  ggplot2::ylab("Random Effect (Intercept)") +
  ggplot2::ggtitle("Scatterplot of Level-2 Random Effects")
```

<img src="C:/Users/tjohnson/Dropbox/zz_Tessa/UMD/NCME_ITEMS_Longitudinal-Growth/ITEMSlme/inst/example/inst/output/Fig_check-indep-3.png" width="672" />


