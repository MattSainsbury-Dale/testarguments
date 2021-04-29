#' Find the optimal arguments fir each diagnostic
#'
#' @param object an object of class testargs
#' @param optimality_criterion a function (or list of functions) that defines the optimality criterion for each diagnostic. Should return a single index
#' @export
optimal_arguments <- function(object, optimality_criterion = which.min) {

  if(!is(object, "testargs"))
    stop("object should be of class testargs")

  if (is.list(optimality_criterion)) {

    if(length(optimality_criterion) != length(object@diagnostic_names))
      stop("If using a list of optimality criteria, please provide one function for each diagnostic (accessed with object@diagnostic_names).")

    if(is.null(names(optimality_criterion))) {
      ## Name the elements of the list according to the diagnostic names
      warning("optimality_criterion is an unnamed list: Assuming that the order of optimality_criterion is the same as that of object@diagnostic_names")
      names(optimality_criterion) <- object@diagnostic_names
    } else {
      if (!all(names(optimality_criterion) %in% object@diagnostic_names))
        stop("optimality_criterion is a named list: It should have the same names as object@diagnostic_names")
    }

    optimal_idx <- sapply(object@diagnostic_names,
                          function(i) {
                            x <- object@diagnostics_df[, i, drop = T]
                            optimality_criterion[[i]](x)
                          })
  } else {
    optimal_idx <- sapply(object@diagnostics_df[, object@diagnostic_names, drop = F],
                          function(x) optimality_criterion(x))
  }

  out <- cbind(which_diagnostic_optimal = object@diagnostic_names,
               object@diagnostics_df[optimal_idx, ])
  rownames(out) <- NULL

  return(out)
}
