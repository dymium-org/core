# dymiumCore (development version)

- Removed deprecated Population's methods: `get_sum_hhsize` and `count_all`.
- Fixed World's remove method in commit c0df66d.
- Added `plot_relationship` method to `Household`. This uses `visNetwork` for plotting (added to Suggests). See #48 for its implementation detail.
- `inspect` now has a verbose option.
- `Transition` no longer removes the `NA` reponses when target is used.
- Added a `replace` method to `World` which basically `remove` and `add` in one call.

# dymiumCore 0.1.2

- `TransitionClassification` now treats `probs` as probability weights and not exact probability values that should add up to 1. 

# dymiumCore 0.1.1

- Removed the `target` argument from `TransitionRegression` as alignment is only applicable for classficaition model.

# dymiumCore 0.1.0

This version contains all the basic building blocks for dynamic microsimulation.
