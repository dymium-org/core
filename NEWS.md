# Development

## NEW FEATURES

1. Add `Population$household_type` method for classifying household types of household agents. There are four types: 'non_family_hh', 'couple_hh', 'couple_hh_with_children' and 'lone_parent_hh'. See its documentation for detail.

# dymiumCore 0.1.4

## NEW FEATURES

1. Add `add(.data, ...)` method to Entity. This allows new entities to be added to the attribute database of existing entities. Note that, `add_data(.data)` is for adding new databases and `add(.data, ...)` is for adding new records to **the attribute database** (`Entity$database$attrs`) which are not the same.
2. Add `check_subset2` which basically the same check as `checkmate::check_subset` but it returns a short error message. The error message it returns only include those missing elements in `x` from `choices`. See `checkmate::check_subset` for more details.
3. `DataBackendDataTable` again on option to use key(s) for row indexing. 
4. Add `Entity$get_data2(ids)` which uses key(s) for subsetting of ids. This suppose to be a faster implementation of `get_data()` which is likely to supersede the original implementation in the next version.
5. `add_population(ind_data, hh_data)` now also assigns new ids to all the records of new entities of `ind_data` and `hh_data` to make sure no duplications of ids exist. 
6. `DataBackend` has new active fields which are `data` amd `removed_data` these functions return a copy of the data and not a reference to the data (only applicable in `DataBackendDataTable` and `DataBackendSpatialFeature`). 
7. `extract_data` returns all the data objects in the DataBackend objects that each Entity possess as a named list of data.table. This is useful for saving simulation data for furthur analysis. It works on World too! 
8. `World` saves session info on its creation instead of just the R version it was created on. 
9. Add `dymium.simulation_scale` global option that can be set with `World$set_scale(x)` and access with `World$scale` or getOption("dymium.simulation_scale"). This simulation scale will be used by all `Target` objects created when they are called by their `get` method. 

## BUG FIXES

1. Ignore checking of attributes in all `data.table::all.equal(...)` calls.
2. Fix `Target`'s constructor method which failed to convert data.frame to data.table when storing the target data.

## DEPRECATIONS

1. Remove `Entity$initialise_data()`, the attribute data of Entity must be provided in its constructor method. This change affected many of the testthat tests. 
2. Remove `household_formation()` to encourage more explicit approaches (e.g. use `Population$leave_household()` and `Population$join_household()`). 

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
