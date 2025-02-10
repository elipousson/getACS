# getACS (development)

* Add `{tibble}` to Imports and add development version of `{tigris}` to Remotes (at least until [this PR](https://github.com/walkerke/tigris/pull/173) is merged).
* Add `join_acs_percent_parent()` function.
* Export `join_acs_denominator()` function.
* Add `assign_acs_reliability()` function (2024-06-09).
* Add `na_zero` argument to return `NA` as the MOE when all MOE values are 0 and collapsing groups with `collapse_acs_variables()`.
* Clean-up documentation by filling in missing arguments (2024-09-17).
* Add `load_decennial_vars()` `label_decennial_data()` functions (2024-09-17).
* Add `collapse` argument to `labs_acs_survey()` (2025-02-10).

# getACS 0.1.1.9003

* Set 2022 to the default year (instead of 2021) following [release of 2018-2022 5-year ACS data](https://www.census.gov/programs-surveys/acs/news/data-releases/2022/release-schedule.html) in December 2023.
* Add `collapse_acs_variables()` helper function.
* Add `gt_acs_compare()` function.
* Add `load_acs_vars()` function as wrapper for `tidycensus::load_variables()`.
* Add `jam_values` reference data.
* Add `get_decennial_ts()` function.
* Add experimental functions `geom_acs_errorbarh()` and `geom_acs_errorbarv()`
* Add `extensive` parameter to `use_area_xwalk()` to support calculation of weighted mean estimates.
* Fix errors due to invalid geometry in `make_block_xwalk()` and `make_area_xwalk()` functions.
* Improve handling of named `variables` parameter by `get_acs_tables()`

# getACS 0.1.1

* Add working versions of the `make_block_xwalk()`, `make_area_xwalk()`, and `use_area_xwalk()` functions.
* Add `tigerweb_geo_index` and `race_iteration` reference data.
* Add vignettes.

# getACS 0.1.0

* Initial release. Package name subject to change.
