# dymiumCore 0.1.9

- introduced `ModelCustom` a model class that let users specify its parameters and `predict` function. We also add `ModelMultinomialLogit`, `ModelBinaryChoice` and `ModelLinear` which are implementations of `ModelCustom`.
- add `makeModel` to create a light weight Model object that can be used in the transition functions and classes. This basically creates an appropriate Model class from the given model object. 
- add helper functions `which_min_n` and `which_max_n`.
- renamed `unnest_datatable` to `unnest_dt` which can take names of the list columns as a character vector. 

# dymiumCore 0.1.8

- Added `sim()` for compiling and executing a microsimulation pipeline.
- Added `targets` as an active field. 
- Fixed `get_transition` didn't use the preprocessing function when the `model` argument is of `Model` class.
- `get_transition` now removes the dot prefix of variables in entity data. Those variables are considered as derived variables which will not be checked against when new entity data are geting added.
- Added `dsample` for sampling from a discrete distribution, an extension of `base:sample`.
- Added `remove_entity` for removing entities from an [Entity] object. 
- `Entity$get_ids` can get the ids of removed entities using newly added the `include_removed` argument.
- `Entity` gains a `primary_id` field.
- `Entity$add` did not check for potential duplicate ids in the removed data.
- `World$set_time` returns self. The `$start_iter` method may be removed in the near future as it is not quite intuitive. 
- Fixed the unexpected behaviour when a non `Target` object is passed into target arg of `Transition`. Prior to this fix, the object that got passed into the target argument was used to create a Target object. Hence, this created an expected behaviour, when the Target's get method was called it returned a scaled down value of the target. This may not be the desire default effect by users. 

# dymiumCore 0.1.7

- `transition()` gains `values` field, it expects a named vector where its values will be used to replace the outcomes of the model and update the variable in `attr`. See the example section to learnsmore.
- `transition()` now supports a named `list` model, e.g: list(yes = 0.1, no = 0.9).
- Introduced `add_log()`, `get_log()`, `mutate_entity()`, and `add_entity()`. These functions are supposed to make constructing a microsimulation pipeline simpler. See their documentation for details.
- Made `$colnames()` into an active field for `DataBackendDataFrame`.
- Added `omit_derived_varnames()` for internal use.
- `Individual` now has a `hid_col` active field which returns its household id column name.
- `Entity$add()` now gives `newdata` new unique ids by default.
- Deprecated `assign_reference()`, `Container$get()` should be used instead.

## Minor changes

- `Generic$log()` now uses `NA_character_` as the default in its `tag` argument.

# dymiumCore 0.1.6

## NEW FEATURES
- Added `transition()`, this is a function for simulating state transition inside a microsimulation pipeline. It also comes with `get_transition()` which returns state transition outcomes. See its documentation, `?dymiumCore::transition`, for more detail. 
- [Model] gains `preprocessing_fn` field, this is for storing a preprocessing function which will be used to evaluate the entity data in [Transition] prior to simulating the transition. A situation where this is useful could be when you want to limit the use of a [Model] object to the specific group of agents (e.g: age between `x` and `y`) that was used to estimate the model.
- [Trans] supports [Model]'s `preprocessing_fn()`.
- Minor fix to error msg in [TransitionClassification].
- [add_models] gains a as_r6model for returning models as [Model] objects.
- Classification models fitted using the `mlr` package is now supported by `transition`.
- Added `simulate_choice`, a function for simulating choice selection from the predicted probability obtained from a model object, currently only supports most of the classification models fitted with `caret` and `mlr`, and `glm` of the binomial family.

## INTERNAL CHANGES
- Renamed `Transition` to `Trans` to make way for `transtion()`.

# dymiumCore 0.1.5

## NEW FEATURES

1. Added `Population$household_type` method for classifying household types of household agents. There are four types: 'non_family_hh', 'couple_hh', 'couple_hh_with_children' and 'lone_parent_hh'. See its documentation for detail.
2. Added a `model` active field to `Model`, this returns the original model object stored inside the object.
3. `Model` gained a S3 summary method.

## FIXES

1. `Household$update_hhsize()` now returns NULL invinsibly instead of its attribute data.
2. `World` now accepts `Model` objects in `add`.

# dymiumCore 0.1.4

## NEW FEATURES

1. Added `add(.data, ...)` method to Entity. This allows new entities to be added to the attribute database of existing entities. Note that, `add_data(.data)` is for adding new databases and `add(.data, ...)` is for adding new records to **the attribute database** (`Entity$database$attrs`) which are not the same.
2. Added `check_subset2` which basically the same check as `checkmate::check_subset` but it returns a short error message. The error message it returns only include those missing elements in `x` from `choices`. See `checkmate::check_subset` for more details.
3. `DataBackendDataTable` again on option to use key(s) for row indexing. 
4. Added `Entity$get_data2(ids)` which uses key(s) for subsetting of ids. This suppose to be a faster implementation of `get_data()` which is likely to supersede the original implementation in the next version.
5. `add_population(ind_data, hh_data)` now also assigns new ids to all the records of new entities of `ind_data` and `hh_data` to make sure no duplications of ids exist. 
6. `DataBackend` has new active fields which are `data` amd `removed_data` these functions return a copy of the data and not a reference to the data (only applicable in `DataBackendDataTable` and `DataBackendSpatialFeature`). 
7. `extract_data` returns all the data objects in the DataBackend objects that each Entity possess as a named list of data.table. This is useful for saving simulation data for furthur analysis. It works on World too! 
8. `World` saves session info on its creation instead of just the R version it was created on. 
9. Added `dymium.simulation_scale` global option that can be set with `World$set_scale(x)` and access with `World$scale` or getOption("dymium.simulation_scale"). This simulation scale will be used by all `Target` objects created when they are called by their `get` method. 

## BUG FIXES

1. Ignore checking of attributes in all `data.table::all.equal(...)` calls.
2. Fixed `Target`'s constructor method which failed to convert data.frame to data.table when storing the target data.

## DEPRECATIONS

1. Removed `Entity$initialise_data()`, the attribute data of Entity must be provided in its constructor method. This change affected many of the testthat tests. 
2. Removed `household_formation()` to encourage more explicit approaches (e.g. use `Population$leave_household()` and `Population$join_household()`). 

# dymiumCore 0.1.3

## NEW FEATURES

1. Added a `plot_relationship` method to `Household`. This uses `visNetwork` for plotting (added to Suggests). See #48 for its implementation detail.
2. `inspect` now has a verbose option.
3. `Transition` no longer removes the `NA` reponses when target is used.
4. Added a `replace` method to `World` which basically `remove` and `add` in one call.
5. Moved `$subset_ids()` from `Agent`  to `Entity`.
6. `download_module()` and `set_active_scenario()` now have a `.basedir` argument which sets the base directory where their files will be created at. By default this is the root folder of the currently active R project (if you are using RStudio) which is determined by `here::here()`.
7. Renamed `use_scenario` to `create_scenario` and `active_scenario` to `get_active_scenario`.
8. `TransitionClassification`'s target argument now accepts a dynamic target, see issue [#52] https://github.com/dymium-org/dymiumCore/issues/52. 
9. Added a `Target` R6 class which acts as a wrapper for different types of target and make them work consistently in the `Transition` classes.

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
