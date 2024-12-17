
## this could go in scutils if it is generally useful.
## A function to track object.size, nrow, & ncol
## Add runtime to log, time each step.
## (color by col type? stripe width by col size?)
#' @title Track a dataset
#' @description I use this to keep track of the changes to a dataset from multiple operations.
#' @param df the data frame to track
#' @param step_name What would you like to call the operation you are tracking?
#' @param track_log What would you like to name the object that logs the information
#' @param step_inc How many steps are you tracking (default is 1).
#' @return \code{TRUE} if object is not entirely \code{NULL}, otherwise \code{FALSE}.
track_dataset <- function(df, step_name=NA_character_,
                          track_log='data_manipulation_log', step_inc=1 ) {
  track_log_entry <- list(step_name=step_name, step_inc=step_inc,
                          nrow=nrow(df), ncol=ncol(df), object.size=object.size(df)[1])
  if (exists(track_log)) {
    track_log_entry$.data <- get(track_log)
    track_log1 <- do.call(add_row, track_log_entry)
  } else {
    track_log1 <- as.data.frame(track_log_entry, stringsAsFactors=FALSE)
  }
  assign(track_log, track_log1, globalenv())
  print(step_name)
  invisible(df)
}
