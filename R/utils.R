## Utility functions
## Never use library() in a package.

## Use requireNamespace(x, quietly = TRUE)
## inside a package if you want a specific action
## (e.g. throw an error) depending on whether or
## not a suggested package is installed.

## Unless there is a good reason otherwise,
## you should always list packages in Imports not Depends.


## not.null(x)
#' @title Not null
#' @description Test whether an R object is not \code{NULL}.
#' @param x an R object to be tested.
#' @return \code{TRUE} if object is not entirely \code{NULL}, otherwise \code{FALSE}.
#' @examples
#' not.null(0) ## TRUE
#' not.null(NA_character_) ## TRUE
#' not.null(NULL) ## FALSE
#' not.null(c(NULL, 0)) ## TRUE
#' not.null(c(a=NULL, b=NULL)) ## FALSE
not.null <- function(x) !is.null(x)

## x %nin% y
#' @title Not in
#' @description Binary operator which returns a logical vector indicating
#'     if there is \emph{not} a match for \code{x} in \code{y}.
#' @param x,y vectors or \code{NULL}.
#'     \code{\link[base]{LongVectors}} not supported for \code{y}.
#' @return A logical vector of the same length as \code{x}.
#' @examples
#' c(1, 2, 3,'a','b','c') %!in% letters
#' ## TRUE TRUE TRUE FALSE FALSE FALSE
'%nin%' <- function(x,y) is.na(match(x,y))

## flast(x)
#' @title Fast last
#' @param x an R object.
#' @return Returns the last element of a vector, factor, or any other R object
#'     which has a method for the \code{length} function.
#' @examples
#' flast(1:5) ## 5
#' flast(letters) ## "z"
flast <- function(x) x[length(x)]

## is.length0(x)
#' @title Test the length of an R object
#' @param x an R object.
#' @return A length 1 logical vector.
#' @examples
#' is.length0() ## TRUE
#' is.length0(NULL) ## TRUE
#' is.length0("") ## FALSE
#' is.length0(NA) ## FALSE
#' is.length1("") ## TRUE
#' is.length1(NA) ## TRUE
#' is.length1(c(NULL, NA)) ## TRUE
#' is.length1(NULL) ## FALSE
#' is.length1(1:2) ## FALSE
is.length0 <- function(x=NULL) length(x)==0

## is.length(0)
#' @rdname is.length0
is.length1 <- function(x=NULL) length(x)==1

## to_aoi(x)
#' @title Convert to Area of Interest
#' @description Convert an R object to an Area of Interest.
#' @param x An R object. Currently only atomic character vectors are accepted.
#' @return An R object of class \code{aoi}
#' @examples
#' to_aoi('South Carolina')
#' to_aoi('North Carolina')
to_aoi <- function(x) {
  aoi <- switch(
    class(x),
    character={x},
    {stop(class(x), ' cannot be converted to aoi')} )
  class(aoi) <- c(class(aoi), 'aoi', recursive=TRUE)
  return(aoi)}

## tabular(df)
#' @title Create a table in Roxygen comments
#' @description Print the output of this function to produce
#'     a table in documentation produced by Roxygen.
#' @param df A \code{data.frame} object to print in the documentation.
#' @return A table of sorts.
# #' @example # print(tabular(mtcars[1:5, 1:5]))
tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format, ...)
  contents <- do.call("paste",
    c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))

  paste("\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
    contents, "\n}\n", sep = "")
}
# #' \\tabular{rrr}{\n }


# library(devtools)
# library(roxygen2)
# library(testthat)
# library(knitr)
# has_devel()

# Some useful keyboard shortcuts for package authoring:
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rstudioapi"))
# rstudioapi::isAvailable("0.99.149")
# devtools::install_github("r-lib/devtools")
# devtools::use_testthat() ## deprecated
# usethis::use_testthat()

#### Within roxygen tags, use .Rd syntax to format text.
# #' \code{\link{function}}: function in this package.
# #' \code{\link[MASS]{abbey}}: function in another package.
# #' \link[=dest]{name}: link to dest, but show name.
# #' \code{\link[MASS:abbey]{name}}: link to function in another package, but show name.
# #' \linkS4class{abc}: link to an S4 class.
# #' \url{http://rstudio.com}: a url.
# #' \href{http://rstudio.com}{Rstudio}:, a url with custom link text.
# #' \email{hadley@@rstudio.com} (note the doubled @): an email address.

#### Example named list
# #' \describe{
# #'   \item{One}{First item}
# #'   \item{Two}{Second item}
# #' }

# devtools::session_info()

## ------ a difference between Imports and Suggests in the DESCRIPTION file
# # You need the suggested package for this function
# my_fun <- function(a, b) {
#   if (!requireNamespace("pkg", quietly = TRUE)) {
#     stop("Package \"pkg\" needed for this function to work. Please install it.",
#       call. = FALSE) } }
# # There's a fallback method if the package isn't available
# my_fun <- function(a, b) {
#   if (requireNamespace("pkg", quietly = TRUE)) {
#     pkg::f()
#   } else { g() } }

# Add roxygen comments to your .R files.
# Run devtools::document() (or press Ctrl/Cmd + Shift + D in RStudio)
#     to convert roxygen comments to .Rd files.
#     (devtools::document() calls roxygen2::roxygenise() to do the hard work.)
# Preview documentation with ?.
