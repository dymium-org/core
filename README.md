
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Build
Status](https://travis-ci.org/dymium-org/dymiumCore.svg?branch=master)](https://travis-ci.org/dymium-org/dymiumCore)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Codecov test
coverage](https://codecov.io/gh/dymium-org/dymiumCore/branch/master/graph/badge.svg)](https://codecov.io/gh/dymium-org/dymiumCore?branch=master)
<!-- badges: end -->

# dymiumCore: <img src="man/figures/logo.png" align="right" alt="" width="120" />

**dymiumCore** is an R package which provides a toolbox for developing a
microsimulation model that is modular and pipable. While the core focus
of the package is for modelling urban systems, dymium can be easily
extended to apply in other contexts as well.

## Installation

The dymiumCore package has not been released on CRAN, but you can
install from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("dymium-org/dymium")
```

## Tutorials

For tutorials please see the articles at <https://main.dymium.org>.

## Available modules

Please visit
[dymium-org/dymiumModules](https://github.com/dymium-org/dymiumModules)
to see available modules.

## Structure

The main building blocks of microsimulation are agents, environment and
models.

## Currently supported models

Currently, dymiumCore we only support model objects fitted using the
`caret` package.

| Package |     Class |                                     Model types |         status |
| ------: | --------: | ----------------------------------------------: | -------------: |
|   caret |   `train` |            classification and regression models |      supported |
|  mlogit |  `mlogit` |                        multinomial logit models | in-development |
|    mlr3 | `Learner` | classification, survival, and regression models |        planned |

**Note that**: data.frames and named lists can be used in the model
argument of a Transition object.

  - If a `data.frame` is provided, it must contain a key column or keys
    columns and must have a column named ‘prob’ that must be a numeric
    type with values within \[0,1\] which represent probability values.
    Recently, models of type data.table have gained a new support where
    multiple choices can be simulated using a data.table object as a
    model. The data.table object should contains matching variables and
    two extra columns which are `probs`, a list column of numeric
    vectors, and `choices`, also a list column but contains character
    vectors.

  - A named `list` can be used to represent choices, where names of the
    list are choices and values are their associated probabilities.
