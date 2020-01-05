
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN
status](https://www.r-pkg.org/badges/version/dymiumCore)](https://CRAN.R-project.org/package=dymiumCore)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/dymiumCore)](https://CRAN.R-project.org/package=dymiumCore)
[![Build
Status](https://travis-ci.org/dymium-org/dymiumCore.svg?branch=master)](https://travis-ci.org/dymium-org/dymiumCore)
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
remotes::install_github("dymium-org/dymiumCore")
```

## Tutorials

For tutorials please see the articles at <https://main.dymium.org>.

## Available modules

Please visit
[dymium-org/dymiumModules](https://github.com/dymium-org/dymiumModules)
to see the available modules.

## Overview of the building blocks

An implementation of a microsimulation model usually consists of these
four components which are entities, rules, transition, and markets.

### Entities

Entities can be persons, firms, buildings, zones, transport network etc.
They can be conviniently defined as a class, based on the concept of
‘objects’ in object-oriented programming. Each entity is known by its
fields and methods, what it represents and what it can do. A person may
contains fields such as age, gender, marital status. A person give
birth, leave parental home, etc. While a household may have household
size, household id, number of vehicles owned as its fields. A household
can relocate, have new members and etc.

### Transition

In a microsimulation model, entities are given rules for them to follow
under different conditions. A rule can simply be an ifelse statement
such as:

    if age is greater than 16:
        can_marry 
    else:
        cant_marry

or probabilistic such as a rate-based model or a classification model
(binary logit model, multinomial logit model, hazard-based model, random
forest, and artificial neural network) that takes attributes of the
entities as the input variables.

dymium provides a class called `TransitionClassification` and
`TransitionRegression` which take in a rule and entities then simulate
the outcomes of the entities given the provided rule. For probabilistic
rules, Monte Carlo simulation will be performed based on the
probabilistic values from the rules.

Currently, dymiumCore we only support model objects fitted using the
`caret` package, a named list, and a data.frame or data.table for Monte
Carlo simulation.

| Package |     Class |                                     Model types |         status |
| ------: | --------: | ----------------------------------------------: | -------------: |
|   caret |   `train` |            classification and regression models |      supported |
|  mlogit |  `mlogit` |                        multinomial logit models | in-development |
|    mlr3 | `Learner` | classification, survival, and regression models |        planned |

**Note that**: rate-based models can be used for simulation as well. To
use them, you first need to import them as a data.frame and a named
list.

  - If a `data.frame` or `data.table` is provided, it must contain a key
    column or keys columns and must have a column named ‘prob’ that must
    be a numeric type with values within \[0,1\] which represent
    probability values. Recently, models of type data.table have gained
    a new support where multiple choices can be simulated using a
    data.table object as a model. The data.table object should contains
    matching variables and two extra columns which are `probs`, a list
    column that contains numeric vectors, and `choices`, also a list
    column but contains character vectors.

<!-- end list -->

``` r
library(data.table) # use install.packages("data.table") to install

(rate_based_model <-
  data.table(
  sex = c("male", "female"),
  prob = c(0.3, 0.2)
))
#>       sex prob
#> 1:   male  0.3
#> 2: female  0.2

(choice_model <-
  data.table(
    sex = c('male', 'female'),
    probs = list(c(0.3,0.7), c(0.4,0.6)),
    choices = list(c('can drive', 'cannot drive'), c('can drive', 'cannot drive'))
))
#>       sex   probs                choices
#> 1:   male 0.3,0.7 can drive,cannot drive
#> 2: female 0.4,0.6 can drive,cannot drive
```

  - A named `list` can be used to represent choices, where the names of
    the list are choices and their values are their associated
    probabilities.

<!-- end list -->

``` r
(binary_list_model <- list(yes = 0.05, no = 0.95))
#> $yes
#> [1] 0.05
#> 
#> $no
#> [1] 0.95
(marriage_model <- list(married = 0.05, not_married = 0.95))
#> $married
#> [1] 0.05
#> 
#> $not_married
#> [1] 0.95
(employment_model <- list(employed = 0.4, unemployed = 0.2, not_in_labour_force = 0.3))
#> $employed
#> [1] 0.4
#> 
#> $unemployed
#> [1] 0.2
#> 
#> $not_in_labour_force
#> [1] 0.3
```

In a binary choice model, it is recommended that the outcome variable
should be coded as “yes” and “no” as many of the modules available at
(dymium-org/dymiumModules)\[<https://github.com/dymium-org/dymiumModules>\]
use with this convention.

### Market

In microsimulation, a market is where entities are directly interact
with one another. A market can be an abstraction of a real-estate
bidding market, a mate matching market, a labour market, etc. The entire
market are separated into two sides (i.e. proposers and proprosees,
buyers and sellers, labours and firms). Interactions maybe one-sided or
two-sided depends on the matching mechanism that is used. When it is
one-sided only agents from one side of the market make the decision
about what they get based on their preference. When it is the matching
problem is two-sided one then both sides of the market evaluate evaluate
options available to them and interact in a way that mimic bidding.

Note that, the number of alternatives available to each agent is
customisable to realistically represent a matching situation that is
being simulated. For example, in a housing search situation it properly
would be inappropiate to assume that all relocating households have
perfect knowledge about all the available dwelling units that are on the
market at any moment in time. Hence, the modeller might want to limit
the number of alternatives available in the choiceset of each household
or the number of zones that the households look for their options.

Currently, there are two implementations of market matching algorithms
which are

  - Stochastic matching (`MatchingMarketStochastic`) and
  - Optimal matching (`MatchingMarketOptimal`)

In `MatchingMarketStochastic`, only one-sided matching problems can be
solved. All agents that are seeking a match are randomly ordered into a
virtual queue. The first agent in the queue gets to select an
alternative among all the alternatives that available to it. While in
`MatchingMarketOptimal`, both one-sided and two-sided problems can be
solved. All agents are aware of all the available alternatives
(e.g. houses, partners, etc.) in the market. This generally a very
strong assumption. Hence, to mitigate such strong assumption the whole
market can be further segmented into sub markets where agents that are
more alike or geographically near one others are stratified into the
same sub market and only aware of each others and not those that are
their unlikely matches.

### Logging

dymium uses the `lgr` package for logging internally in the base code.
By default, only log messages with `warn` level or higher will be
displayed in the R console. The users are also encourage to use the same
logger object that is used internally to create additional logging
messages. The same logger may be used for logging of simulation results.
To log a simulation result, use `lg$info()` and use `SIM_OUTPUT` as
value in the msg argument.

``` r
dymiumCore:::lg$info("SIM_OUTPUT", desc = "number_of_individuals", value = 1000)
```

Note that, the first argument of `lg$info()` is `msg` and you do not
have to specify that (i.e. `lg$info(msg = "SIM_OUTPUT"`)) otherwise you
will get the following error.

``` r
[2020-01-06 07:28:40.881] An error occurred during logging: Error in .subset2(public_bind_env, "initialize")(...): formal argument "msg" matched by multiple actual arguments
```

To access the log data of your simulation results use the following
command.

``` r
dymiumCore:::lg$appenders$buff$appenders$sim_output$data
#>    level           timestamp     logger caller        msg                  desc
#> 1    400 2020-01-06 07:29:49 dymiumCore   eval SIM_OUTPUT number_of_individuals
#> 2    400 2020-01-06 07:30:09 dymiumCore   eval SIM_OUTPUT number_of_individuals
#> 3    400 2020-01-06 07:30:46 dymiumCore   eval SIM_OUTPUT number_of_individuals
#> 4    400 2020-01-06 08:27:45 dymiumCore   eval SIM_OUTPUT number_of_individuals
#> 5    400 2020-01-06 08:28:08 dymiumCore   eval SIM_OUTPUT number_of_individuals
#> 6    400 2020-01-06 08:29:25 dymiumCore   eval SIM_OUTPUT number_of_individuals
#> 7    400 2020-01-06 08:30:32 dymiumCore   eval SIM_OUTPUT number_of_individuals
#> 8    400 2020-01-06 08:33:09 dymiumCore   eval SIM_OUTPUT number_of_individuals
#> 9    400 2020-01-06 08:39:04 dymiumCore   eval SIM_OUTPUT number_of_individuals
#> 10   400 2020-01-06 08:42:17 dymiumCore   eval SIM_OUTPUT number_of_individuals
#> 11   400 2020-01-06 08:43:33 dymiumCore   eval SIM_OUTPUT number_of_individuals
#> 12   400 2020-01-06 08:44:29 dymiumCore   eval SIM_OUTPUT number_of_individuals
#>    value
#> 1   1000
#> 2   1000
#> 3   1000
#> 4   1000
#> 5   1000
#> 6   1000
#> 7   1000
#> 8   1000
#> 9   1000
#> 10  1000
#> 11  1000
#> 12  1000
```
