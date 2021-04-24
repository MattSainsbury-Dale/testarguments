#' bind together two testargs objects
#' @param object1 object of class \code{testargs}
#' @param object2 object of class \code{testargs}
#' @export
#' @import plyr
bind <- function(object1, object2) {
  if (!is(object1, "testargs") || !is(object2, "testargs"))
    stop("object1 and object2 should be of class testargs")

  new_df <- plyr::rbind.fill(object1@diagnostics_df, object2@diagnostics_df)
  new_arg_names <- union(object1@arg_names, object2@arg_names)
  new_diagnostic_names <- union(object1@diagnostic_names, object2@diagnostic_names)

  return(new("testargs",
             diagnostics_df = new_df,
             arg_names = new_arg_names,
             diagnostic_names = new_diagnostic_names
  ))
}

