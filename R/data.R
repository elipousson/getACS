#' U.S. Census Bureau ArcGIS Services Index
#'
#' Index created with [esri2sf::esriIndex()] listing all services located at
#' <https://tigerweb.geo.census.gov/arcgis/rest/services>. Access ArcGIS
#' services using the arcgislayers <https://github.com/R-ArcGIS/arcgislayers> or
#' esri2sf package <https://github.com/elipousson/esri2sf>. Last updated 2025-02-10.
#'
#' @format A data frame with 7750 rows and 15 variables:
#' \describe{
#'   \item{`name`}{Name}
#'   \item{`type`}{Service/layer type}
#'   \item{`url`}{Folder/service/layer URL}
#'   \item{`urlType`}{URL type}
#'   \item{`folderPath`}{Index type}
#'   \item{`serviceName`}{Service name}
#'   \item{`serviceType`}{Service type}
#'   \item{`id`}{integer Layer ID number}
#'   \item{`parentLayerId`}{integer Parent layer ID number}
#'   \item{`defaultVisibility`}{logical Layer default visibility}
#'   \item{`subLayerIds`}{list Sublayer ID numbers}
#'   \item{`minScale`}{double Minimum scale}
#'   \item{`maxScale`}{integer Maximum scale}
#'   \item{`geometryType`}{Geometry type}
#'   \item{`supportsDynamicLegends`}{logical Supports dynamic legends}
#' }
#' @details <https://tigerweb.geo.census.gov/arcgis/rest/services>
"tigerweb_geo_index"


#' Race or Latino Origin Table Codes
#'
#' For selected tables, an alphabetic suffix follows to indicate that a table is
#' repeated for the nine major race and Hispanic or Latino groups.
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{`code`}{Code}
#'   \item{`group`}{Race or Ethnic group}
#'   \item{`label`}{Short label}
#' }
#' @details <https://www.census.gov/programs-surveys/acs/data/data-tables/table-ids-explained.html>
"race_iteration"

#' ACS Jam Values for Medians
#'
#' Reference table of ACS "jam values" for medians from "Table 5.2. Jam Values
#' for Medians," *[Understanding and Using American Community Survey Data: What
#' All Data Users Need to
#' Know](https://www.census.gov/programs-surveys/acs/library/handbooks/general.html)*
#' (2020). `type` and `units` values are added. `year` is included to account
#' for the possibility of alternate jam values for earlier or later years but
#' annual variation in values has not been checked.
#'
#' @format A data frame with 20 rows and 6 variables:
#' \describe{
#'   \item{`value`}{Estimate value}
#'   \item{`meaning`}{Meaning of estimate value}
#'   \item{`use`}{Subjects/tables where jam value is used}
#'   \item{`type`}{Type (minimum or maximum jam value)}
#'   \item{`units`}{Units. Note year is for a specific year, years is for duration.}
#'   \item{`year`}{Year applicable}
#' }
#' @details <https://docs.google.com/spreadsheets/d/1YX3NBDkkoDXHs88KDfPS_QoS9-1j_C_q8UAyjPznfzA/edit?usp=sharing>
"jam_values"

#' U.S. States Reference Data
#'
#' A reference table of state names, abbreviations, regions, and divisions.
#'
#' @format A data frame with 56 rows and 7 variables:
#' \describe{
#'   \item{`state`}{State name}
#'   \item{`state_abb`}{State USPS abbreviation}
#'   \item{`STATE_GEOID`}{State GeoID}
#'   \item{`division`}{Census Division name}
#'   \item{`DIVISION_GEOID`}{Census Division GeoID}
#'   \item{`region`}{Census Region name}
#'   \item{`REGION_GEOID`}{Census Region GeoID}
#'}
"usa_states"
