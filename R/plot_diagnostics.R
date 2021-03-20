## Reshape the data so we have a single variable "diagnostic",
## which will be RMSPE, COV90, IS90, CRPS, Time. The rest of the variables
## are the arguments which we are trying to optimise over.
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
#'  \item{The rows of the facet correspond to the third argument (if present)}
#'  \item{The shape of the points correspond to the fourth argument (if present)}
#' }
#' @param diagnostic_df a \code{data.frame} from a call to \code{test_arguments}
#' @param arg_names the columns which correspond to argument names in \code{diagnostic_df}
#' @param focused_args the arguments we wish to plot. If \code{NULL} (default), all arguments are plotted
#' @param average_out_non_focused_args logical indicating whether we should average over the non-focused arguments
#' @return a \code{ggplot} object
#' @seealso \code{\link{test_arguments}}
#' @export
#' @examples
#' ## See the example in ?test_diagnostics for this functions intended use
#' @import ggplot2
#' @import magrittr
plot_diagnostics <- function(diagnostics_df, arg_names, focused_args = NULL,
                             average_out_non_focused_args = TRUE) {

  # if(missing(arg_names))
  #   arg_names <- names(long_df)[-which(names(long_df) %in% c("Diagnostic", "value"))]

  long_df <- .long_diagnostic_df(diagnostics_df, arg_names)

  if(is.null(focused_args))
    focused_args <- arg_names

  if (average_out_non_focused_args)
    long_df <- paste(". ~ ", "Diagnostic + ", paste(focused_args, collapse = " + ")) %>%
      as.formula() %>%
      aggregate(long_df, mean)

  if (length(focused_args) > 4)
    stop("Too many arguments for me to visualise!")

  g <- ggplot(long_df, aes(y = value))

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
  g <-  g + geom_point() + geom_line() + theme_bw() + labs(y = "")

  if(is.numeric(long_df[, focused_args[1]])) {
    g <- g + scale_x_continuous(breaks = unique(long_df[, focused_args[1]]))
  }

  ## Add the facet
  if (length(focused_args) %in% c(1, 2)) {
    g <- g + facet_wrap(. ~Diagnostic, scales = "free", nrow = 1)
  } else if (length(focused_args) %in% c(3, 4)) {
    g <- g + facet_grid(as.formula(paste("Diagnostic", "~", focused_args[3])), scales="free")
  }

  return(g)
}
