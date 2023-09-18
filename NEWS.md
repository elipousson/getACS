# getACS (development version)

* Add `collapse_acs_variables()` helper function.
* Add `gt_acs_compare()` function.
* Add `load_acs_vars()` function as wrapper for `tidycensus::load_variables()`.
* Add `jam_values` reference data.
* Add `extensive` parameter to `use_area_xwalk()` to support calculation of weighted mean estimates.
* Fix errors due to invalid geometry in `make_block_xwalk()` and `make_area_xwalk()` functions.
* Add experimental functions `geom_acs_errorbarh()` and `geom_acs_errorbarv()`

# getACS 0.1.1

* Add working versions of the `make_block_xwalk()`, `make_area_xwalk()`, and `use_area_xwalk()` functions.
* Add `tigerweb_geo_index` and `race_iteration` reference data.
* Add vignettes.

# getACS 0.1.0

* Initial release. Package name subject to change.
