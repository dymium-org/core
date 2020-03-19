# dymiumCore (development version)

## BUG FIXES

1. Fix `Target`'s constructor method which failed to convert data.frame to data.table when storing the target data.

# dymiumCore 0.1.3

## NEW FEATURES

1. Add a `plot_relationship` method to `Household`. This uses `visNetwork` for plotting (added to Suggests). See #48 for its implementation detail.
2. `inspect` now has a verbose option.
3. `Transition` no longer removes the `NA` reponses when target is used.
4. Add a `replace` method to `World` which basically `remove` and `add` in one call.
5. Move `$subset_ids()` from `Agent`  to `Entity`.
6. `download_module()` and `set_active_scenario()` now have a `.basedir` argument which sets the base directory where their files will be created at. By default this is the root folder of the currently active R project (if you are using RStudio) which is determined by `here::here()`.
7. Rename `use_scenario` to `create_scenario` and `active_scenario` to `get_active_scenario`.
8. `TransitionClassification`'s target argument now accepts a dynamic target, see issue [#52] https://github.com/dymium-org/dymiumCore/issues/52. 
9. Add a `Target` R6 class which acts as a wrapper for different types of target and make them work consistently in the `Transition` classes.

## BUG FIXES

- Fixed World's remove method in commit c0df66d.
- Removed the `preprocess` method in `Transition`. Whether the mutate step or the filter step should be run first is now determine by the `mutate_fisrt` public field which has `FALSE` as default.
- `create_scenario` did not pass `.basedir` to `set_active_scenario` so when creating a new scenario that isn't within a RStudio project will raise an incorrect directory error.

## DEPRECATIONS

- Unexported `Firm` as it has not been fully implemented and tested yet.
- Removed deprecated Population's methods: `get_sum_hhsize` and `count_all`.

# dymiumCore 0.1.2

- `TransitionClassification` now treats `probs` as probability weights and not exact probability values that should add up to 1. 

# dymiumCore 0.1.1

- Removed the `target` argument from `TransitionRegression` as alignment is only applicable for classficaition model.

# dymiumCore 0.1.0

This version contains all the basic building blocks for dynamic microsimulation.
