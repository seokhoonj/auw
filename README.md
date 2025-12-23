# auw

![](man/figures/auw-hex.png){width=30%}

**auw** provides tools for disease cohort analysis and A/E (Actual-to-Expected) ratio simulation, integrating both actuarial and underwriting perspectives. It is designed to support morbidity experience studies, evaluate underwriting bias, and visualize portfolio-level risk outcomes in insurance portfolios.

This package is developed primarily for **actuarial underwriting workflows**, bridging traditional actuarial analysis with practical underwriting use cases.

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/auw)](https://CRAN.R-project.org/package=auw) [![R-CMD-check](https://github.com/seokhoonj/auw/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/seokhoonj/auw/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Installation

``` r
# install dev version
devtools::install_github("seokhoonj/auw")
```

\> **Scope of this README**\
\> While **auw** supports a broader set of A/E and underwriting analytics, this README focuses specifically on **how to compute and interpret Relative Risk (RR)**\
\> using disease cohorts and KCD-based morbidity data.

# KCD Time-Window Relative Risk Toolkit

A compact, class-preserving toolkit for underwriting-oriented incidence and relative risk analysis using KCD-coded episode data.

This module supports a full workflow:

1.  Select IDs by KCD occurrence within underwriting-relative windows
2.  Summarise IDs into incidence-rate (IR) tables
3.  Compute RR / OR with confidence intervals
4.  Visualise incidence rates and relative risks
5.  Export tables and plots to Excel

All functions preserve the input data class (data.frame, tibble, or data.table) and are optimised for large, episode-level cohorts.

------------------------------------------------------------------------

## Installation

``` r
# install.packages("devtools")
devtools::install_github("seokhoonj/auw")
```

------------------------------------------------------------------------

## Quick start

``` r
library(auw)
library(ggshort)
library(instead)

# set_instead_font("Comic Sans MS")
# set_ggshort_font("Comic Sans MS")

cohort <- instead::read_rds("cohort.rds")
icis   <- instead::read_rds("icis.rds")

auw::add_age_band(cohort, age_var = age, interval = 10)

decl  <- list(-60,  0, "E1[0-4]")
excl  <- list(-60,  0, "I2[0-5]|I6[0-9]|G46")
claim <- list(  0, 36, "I2[0-5]|I6[0-9]|G46")

# 1) Build incidence-rate data (ID-level)
ir_data <- auw::summarise_id_with_kcd_ir(
  cohort    = cohort,
  id_var    = id,
  group_var = age_band,
  kcd_var   = kcd,
  from_var  = sdate,
  to_var    = edate,
  uw_date   = "2018-07-01",
  decl      = decl,
  excl      = excl,
  claim     = claim
)

# 2) Relative risk / odds ratio
ir <- summary(ir)
rr <- auw::summarise_rr(ir, decl_vs = c("0", "1"))

# 3) Visualisation
plot(ir)
plot(rr)

# 4) icis mix (target)
mix_data <- auw::summarise_id_with_kcd_mix(
  cohort    = icis,
  id_var    = id,
  group_var = age_band,
  kcd_var   = kcd,
  from_var  = sdate,
  to_var    = edate,
  uw_date   = uw_date,
  decl      = decl,
  excl      = excl
)

mix <- summary(mix_data)

# 5) Export
auw::save_rr_xlsx(
  ir   = ir,
  rr   = rr,
  mix  = mix,
  file = "RR.xlsx",
  overwrite = TRUE
)
```

![](man/figures/ir_plot.png) ![](man/figures/rr_plot.png)

------------------------------------------------------------------------

## Core concepts

### Episode-level cohort

Input data are expected at episode level (multiple rows per ID), with:

-   an ID column
-   a KCD code column
-   a service date interval (from, to) per row

All window logic is evaluated against these intervals.

### Underwriting-relative windows

Time windows are defined in months relative to uw_date:

-   start : months before uw_date (negative)
-   end : months after uw_date (positive)

An episode overlaps a window if:

```         
to >= window_start  &  from < window_end
```

------------------------------------------------------------------------

## Functions

### 1. subset_cohort_by_id_with_kcd()

Subset a cohort by ID, selecting all rows for IDs that have any KCD-matching episode overlapping a given underwriting window.

-   KCD matching uses regular expressions

-   Prefix a pattern with "!" to exclude matching IDs

-   Returned data include all rows for selected IDs

``` r
library(auw)

auw::subset_cohort_by_id_with_kcd(
  cohort   = cohort, 
  id_var   = id, 
  kcd_var  = kcd, 
  from_var = sdate, 
  to_var   = edate, 
  uw_date  = as.Date("2017-08-01"), 
  start = -60, end = 36, "M51"
)

auw::subset_cohort_by_id_with_kcd(
  cohort   = cohort, 
  id_var   = id, 
  kcd_var  = kcd, 
  from_var = sdate, 
  to_var   = edate, 
  uw_date  = as.Date("2017-08-01"), 
  start = -24, end = 12, "M79|S33", "!^C"
)
```

------------------------------------------------------------------------

### 2. summarise_id_with_kcd()

Create an ID-level KCD flag table from an episode-level cohort by evaluating one or more KCD time windows defined relative to an underwriting date.

This function produces a compact, ID-level dataset with binary (0/1) indicators for each specified KCD window and attaches contingency summaries for downstream incidence-rate and relative risk workflows.

What it does

1.  For each specified KCD window, computes a time interval around the underwriting date `uw_date`. Window bounds are specified in months (negative = before, positive = after) and overlap is evaluated as:

    ```         
    to >= window_start  &  from < window_end
    ```

2.  For each ID, checks whether any episode overlaps the window AND matches the given KCD pattern. The result is a binary flag (0/1).

3.  Collapses episode-level data to one row per ID (+ optional grouping variables), producing one indicator column per KCD window.

4.  Attaches contingency summaries as attributes, including:

    -   raw : lowest-level contingency table
    -   summary : first-level summary (alias of summary.1)
    -   summary.N : deeper summaries by nested groupings

Return value

-   Returns an ID-level table with one row per ID (+ group).
-   Each KCD window produces a binary indicator column.
-   The base class of the input cohort is preserved (data.frame / tibble / data.table).

``` r
library(auw)

data <- summarise_id_with_kcd(
  cohort       = cohort, 
  id_var       = id,
  group_var    = age_band, 
  kcd_var      = kcd, 
  from_var     = sdate, 
  to_var       = edate, 
  uw_date      = as.Date("2017-08-01"), 
  hypertension = list(-60, 0, "I10"),
  cv_event     = list( 0, 36, "I2[0-5]|I6[0-9]|G46")
)

summary(data)
attr(data, "raw")
```

Notes

-   Named arguments in `...` control the output column names; unnamed windows default to a pattern-based name.
-   KCD patterns support regular expressions.
-   This function is a low-level building block used by:
    -   summarise_id_with_kcd_ir()
    -   summarise_id_with_kcd_mix()

------------------------------------------------------------------------

### 2.1 summarise_id_with_kcd_ir()

Build an underwriting-style incidence-rate (IR) dataset from an episode-level cohort by flagging each ID for declaration, exclusion, and claim windows defined relative to an underwriting date.

This function returns an ID-level table (one row per ID plus optional grouping) with binary indicators and attaches a pre-aggregated IR summary for downstream relative risk analysis, plotting, and export.

What it does

1.  For each ID, checks whether any episode overlaps each KCD window around the underwriting date `uw_date`. Window bounds are specified in months (negative = before, positive = after) and overlap is evaluated as:

    ```         
    to >= window_start  &  from < window_end
    ```

2.  Produces binary flags (0/1) at the ID (+ group) level:

    -   decl : declaration window (exposure definition)
    -   excl : exclusion window (pre-underwriting disqualifiers)
    -   claim : claim window (outcome definition)

3.  Creates and attaches an "ir" summary table as an attribute, accessible via `summary()`.

4.  Returns the main result with prepended class "ir.data" (base class of the input cohort is preserved).

``` r
library(auw)

ir_data <- auw::summarise_id_with_kcd_ir(
  cohort    = cohort,
  id_var    = id,
  group_var = age_band,
  kcd_var   = kcd,
  from_var  = sdate,
  to_var    = edate,
  uw_date   = as.Date("2017-08-01"),
  decl      = list(-60,  0, "I10"),
  excl      = list(-60,  0, "I2[0-5]|I6[0-9]|G46"),
  claim     = list(  0, 36, "I2[0-5]|I6[0-9]|G46")
)

class(ir_data)     # includes "ir.data"
summary(ir_data)   # returns the attached "ir" summary table
```

### 2.2 summarise_id_with_kcd_mix()

Build a target population mix from an episode-level cohort by selecting IDs that satisfy underwriting-style declaration and exclusion windows defined relative to an underwriting date.

This function is designed to produce group-level composition weights (mix) for downstream weighting, calibration, reporting, and Excel export workflows.

What it does

1.  For each ID, checks whether any episode overlaps each KCD window around the underwriting date `uw_date`. Window bounds are specified in months (negative = before, positive = after) and overlap is evaluated as:

    ```         
    to >= window_start  &  from < window_end
    ```

2.  Defines the target population (IDs) using:

    -   decl : declaration window (must match within the window)
    -   excl : exclusion window (must NOT match within the window)

    In other words, keep IDs that are declared (decl == 1) and not excluded (excl == 0), then compute the composition of that target set.

3.  Collapses to one row per ID (+ group) and summarises by `group_var` to produce a mix table with:

    -   n : number of IDs in each group
    -   wt : weight per group, computed as n / sum(n)

4.  Returns the main result with prepended class "mix.data" (base class of the input cohort is preserved). A group-level mix summary is accessible via `summary()`.

``` r
# 1) Build mix.data (target)
mix_data <- auw::summarise_id_with_kcd_mix(
  cohort    = icis,
  id_var    = id,
  group_var = age_band,
  kcd_var   = kcd,
  from_var  = sdate,
  to_var    = edate,
  uw_date   = uw_date,
  decl      = decl,
  excl      = excl
)

# 2) Extract the group-level mix table (n and wt)
mix <- summary(mix_data)

class(mix_data)   # includes "mix.data"
head(mix)         # typically: group_var, n, wt
```

------------------------------------------------------------------------

### 3. summarise_rr()

Compute relative risk (RR) and odds ratio (OR) from ir / ir.data objects.

``` r
rr <- auw::summarise_rr(ir_data, decl_vs = c("0","1"))
head(rr)
attr(rr, "cmatrix")
```

------------------------------------------------------------------------

### 4. plot_ir()

Draw an incidence rate bar plot from an ir object.

``` r
plot_ir(summary(ir_data), palette = "base", theme = "view")
plot(summary(ir_data), palette = "base", theme = "view")
```

------------------------------------------------------------------------

### 5. plot_rr()

Visualise relative risks from an rr object.

``` r
plot_rr(rr, logscale = FALSE, theme = "view")
plot(rr, logscale = FALSE, theme = "view")
```

------------------------------------------------------------------------

### 6. save_rr_xlsx()

Export a complete RR report to Excel, including tables and plots.

``` r
auw::save_rr_xlsx(
  ir   = summary(ir_data),
  rr   = rr,
  mix  = mix,
  file = "RR.xlsx",
  overwrite = TRUE
)
```

------------------------------------------------------------------------

## Output classes

-   ir.data – ID-level incidence table with attached summaries
-   ir – incidence-rate summary used for plotting and RR calculation
-   rr – relative risk / odds ratio results

------------------------------------------------------------------------

## Design philosophy

-   Class-preserving: works with data.table
-   Underwriting-centric: window logic mirrors real underwriting rules
-   Composable: each step is independently inspectable and reusable
-   Scalable: optimised for large, episode-level claims data
