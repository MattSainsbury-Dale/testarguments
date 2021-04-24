## Reshape the data so we have a single variable "diagnostic",
## which will be RMSPE, COV90, IS90, CRPS, Time. The rest of the variables
## are the arguments which we are trying to optimise over.
#' @import reshape2
.long_diagnostic_df <- function(df, arg_names) {
  return(reshape2::melt(df, id = arg_names, variable.name = "Diagnostic"))
}



#' Visualise diagnostics across the tested arguments.
#'
#' The idea is to make a faceted plot, where:
#' \itemize{
#'  \item{The columns of the facet correspond to the diagnostic (e.g., RMSPE, CRPS, Time, etc.)}
#'  \item{The y-axis is the value of the given diagnostic}
#'  \item{The x-axis is the value of the first argument we are optimising}
#'  \item{The colour scale and grouping is the value of the second argument (if present)}
#'  \item{If a third argument is present, \code{facet_grid} is used, whereby columns correspond levels of the third argument, and rows correspond to diagnostics. Note that \code{facet_grid} forces a given row to share a common y-scale, so the plot would be misleading if diagnostics were kept as columns (particularly if each diagnostic is at a different scale)}
#'  \item{The shape of the points correspond to the fourth argument (if present)}
#' }
#' @param object an object of class \code{testargs} from a call to \code{test_arguments()}
#' @param focused_args the arguments we wish to plot. If \code{NULL} (default), all arguments are plotted
#' @param average_out_non_focused_args logical indicating whether we should average over the non-focused arguments
#' @return a \code{ggplot} object
#' @seealso \code{\link{test_arguments}}
#' @export
#' @examples
#' ## See the example in ?test_diagnostics for this functions intended use
#' @import ggplot2
#' @import magrittr
plot_diagnostics <- function(object, focused_args = NULL,
                             average_out_non_focused_args = TRUE) {

  if (!is(testargs_object, "testargs"))
    stop("object should be of class 'testargs'")

  if(is.null(focused_args)) {
    focused_args <- object@arg_names
  } else {
    if (!all(focused_args %in% object@arg_names))
      stop("Some focused_args are not in the original argument names")
  }

  long_df <- .long_diagnostic_df(object@diagnostics_df, object@arg_names)

  if (!all(object@arg_names %in% focused_args) && average_out_non_focused_args) {

    ## Need to check that all values of averaged out variables are present for
    ## all values of the focused arguments.
    ## First, create all combinations possible with the values of the arguments supplied
    all_combinations <- lapply(object@diagnostics_df[, object@arg_names], unique) %>% expand.grid()

    ## Now see if all combinations occur in object@diagnostics_df
    ## simple check:
    if (nrow(all_combinations) != nrow(object@diagnostics_df))
      warning("Not all combinations of the arguments have been tested - this may result in misleading visualisations.")

    long_df <- paste(". ~ ", "Diagnostic + ", paste(focused_args, collapse = " + ")) %>%
      as.formula() %>%
      aggregate(long_df, mean)
  }

  if (length(focused_args) > 4)
    stop("Too many arguments for me to visualise!")

  ## Basic plot
  g <- ggplot(long_df, aes(y = value))

  ## If we have a mixture of numeric and character/factor arguments, it would be
  ## best to use the numeric argument for the x-axis. The following sorts the
  ## arguments based on the fact that character < factor < numeric in terms of
  ## alphabetical order.
  focused_args <- names(sort(sapply(long_df[, focused_args, drop  = F], class), decreasing = T))

  ## Add the aesthetics
  if (length(focused_args) >= 1) {
    g <- g + aes_string(x = focused_args[1])
  }

  if (length(focused_args) >= 2) {
    ## Make colour aesthetic factor for nice output
    long_df[, focused_args[2]]  <- factor(long_df[, focused_args[2]], ordered = TRUE)
    g <- g %+% long_df
    g <- g + aes_string(colour = focused_args[2], group = focused_args[2])
  }

  if (length(focused_args) >= 3) {
    long_df[, focused_args[3]] <- factor(
      long_df[, focused_args[3]], ordered = TRUE,
      labels = paste(focused_args[3], sort(unique(long_df[, focused_args[3]])), sep = " = ")
    )
    g <- g %+% long_df
  }

  if (length(focused_args) == 4) {
    ## Shape aesthetic cannot be numeric
    long_df[, focused_args[4]]  <- factor(long_df[, focused_args[4]])
    g <- g %+% long_df
    g <- g + aes_string(shape = focused_args[4])
  }

  ## Add the layers
  g <-  g + geom_point() + theme_bw() + labs(y = "")

  if(is.numeric(long_df[, focused_args[1]])) {
    g <- g + geom_line() + scale_x_continuous(breaks = unique(long_df[, focused_args[1]]))
  }

  ## Add the facet
  if (length(focused_args) %in% c(1, 2)) {
    g <- g + facet_wrap(. ~Diagnostic, scales = "free", nrow = 1)
  } else if (length(focused_args) %in% c(3, 4)) {
    g <- g + facet_grid(as.formula(paste("Diagnostic", "~", focused_args[3])), scales="free")
  }

  return(g)
}
