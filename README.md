# auw

<img src="man/figures/auw-hex.png" width="200"/>

<!-- Pandoc/PDF/Word용 -->
<!--![](man/figures/auw-hex.png){ width=200px }>

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/auw)](https://CRAN.R-project.org/package=auw) [![R-CMD-check](https://github.com/seokhoonj/auw/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/seokhoonj/auw/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Installation

``` r
# install dev version
devtools::install_github("seokhoonj/auw")
```

## Examples

``` r
library(auw)
library(ggshort)
library(instead)

# set_instead_font("Comic Sans MS")
# set_ggshort_font("Comic Sans MS")

cohort <- instead::read_rds("cohort.rds")
icis   <- instead::read_rds("icis.rds")

decl  <- list(-60,  0, "E1[0-4]")
excl  <- list(-60,  0, "I2[0-5]|I6[0-9]|G46")
claim <- list(  0, 36, "I2[0-5]|I6[0-9]|G46")
                
auw::add_age_band(cohort, age_var = age, interval = 10)

data <- auw::summarise_id_with_kcd_ir(
  cohort = cohort,
  id_var = id,
  group_var = .(age_band),
  kcd_var = kcd,
  from_var = sdate,
  to_var = edate,
  uw_date = as.Date("2017-08-01"),
  decl  = decl,
  excl  = excl,
  claim = claim
)

ir <- summary(data)
rr <- auw::summarise_rr(ir)

icis_mix_data <- auw::summarise_id_with_kcd_ir(
  cohort    = icis,
  id_var    = id,
  group_var = .(age_band),
  kcd_var   = kcd,
  from_var  = sdate,
  to_var    = edate,
  uw_date   = uw_date,
  decl      = decl,
  excl      = excl
)

icis_mix <- summary(icis_mix_data)

# auw::save_rr_xlsx(ir, rr, icis_mix, file = "RR.xlsx", overwrite = TRUE)

plot(ir)
plot(rr)
```

![](man/figures/ir_plot.png) ![](man/figures/rr_plot.png)
