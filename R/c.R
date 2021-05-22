#' Bind together testarg objects
#' @param x object of class \code{testargs}
#' @param ... objects of class \code{testargs} to be combined with \code{x}
#' @export
setMethod("c", signature="testargs", function(x, ...) {

  object_list <- list(...)

  if (!all(sapply(object_list, function(x) is(x, "testargs"))))
    stop("all objects to be combined should be of class testargs")

  object_list <- c(list(x), object_list)

  new_df <- rbind.fill(lapply(object_list, function(x) x@diagnostics_df))

  new_arg_names  <- x@arg_names
  new_diag_names <- x@diagnostic_names
  for (i in 2:length(object_list)) {
    new_arg_names  <- union(new_arg_names,  object_list[[i]]@arg_names)
    new_diag_names <- union(new_diag_names, object_list[[i]]@diagnostic_names)
  }

  return(new("testargs",
             diagnostics_df = new_df,
             arg_names = new_arg_names,
             diagnostic_names = new_diag_names
  ))
})
