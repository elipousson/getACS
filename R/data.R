#' U.S. Census Bureau ArcGIS Services Index
#'
#' Index created with [esri2sf::esriIndex()] listing all services located at
#' <https://tigerweb.geo.census.gov/arcgis/rest/services>. Access ArcGIS
#' services using the esri2sf package <https://github.com/elipousson/esri2sf> or
#' arcpullr <https://github.com/pfrater/arcpullr/>.
#'
#' @format A data frame with 7081 rows and 15 variables:
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
