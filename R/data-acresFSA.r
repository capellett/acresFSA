#' acresFSA
#'
#' Acreage data from 2009-2023 by County, irrigation, and other factors.
#'
#' @format A data frame with nine variables:
#' \describe{
#' \item{\code{Year}}{Year, 2009-2023}
#' \item{\code{State}, \code{County}}{US State and county names, all caps.}
#' \item{\code{Type}}{Cultivation type: Failed, Not Planted, Planted, Prevented, or Volunteer}
#' \item{\code{Irrigation Practice}}{Irrigated with typical means, other type of irrigation, and total (irrigated and non-irrigated)}
#' \item{\code{Crop}}{Name of crop}
#' \item{\code{Variety}}{Name of crop variety}
#' \item{\code{Intended Use}}{Intended use of the crop}
#' \item{\code{Acres}}{Amount of acres}
#' }
#'
#' For further details, see \url{https://www.fsa.usda.gov/news-room/efoia/electronic-reading-room/frequently-requested-information/crop-acreage-data/index}
#'
"acresFSA"
