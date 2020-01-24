# dymiumCore (development version)

## NEW FEATURES

1. Added a `plot_relationship` method to `Household`. This uses `visNetwork` for plotting (added to Suggests). See #48 for its implementation detail.
2. `inspect` now has a verbose option.
3. `Transition` no longer removes the `NA` reponses when target is used.
4. Added a `replace` method to `World` which basically `remove` and `add` in one call.
5. Moved `$subset_ids()` from `Agent`  to `Entity`.
6. `download_module()` and `set_active_scenario()` now have a `.basedir` argument which sets the base directory where their files will be created at. By default this is the root folder of the currently active R project (if you are using RStudio) which determines by `here::here()`.
7. Renamed `use_scenario` to `create_scenario` and `active_scenario` to `get_active_scenario`. 

## BUG FIXES

- Fixed World's remove method in commit c0df66d.
- Removed the `preprocess` method in `Transition`. Whether the mutate step or the filter step should be run first is now determine by the `mutate_fisrt` public field which has `FALSE` as default.

## DEPRECATIONS

- Unexported `Firm` as it has not been fully implemented and tested yet.
- Removed deprecated Population's methods: `get_sum_hhsize` and `count_all`.

# dymiumCore 0.1.2

- `TransitionClassification` now treats `probs` as probability weights and not exact probability values that should add up to 1. 

# dymiumCore 0.1.1

- Removed the `target` argument from `TransitionRegression` as alignment is only applicable for classficaition model.

# dymiumCore 0.1.0

This version contains all the basic building blocks for dynamic microsimulation.
