#' testargs
#'
#' @slot diagnostics_df a data.frame containing the diagnostics for each combination of the supplied arguments
#' @slot arg_names the argument names
#' @slot diagnostic_names the diagnostic names
#' @export
setClass("testargs",
         slots = c("diagnostics_df" = "data.frame", "arg_names" = "character",
                   "diagnostic_names" = "character"))

