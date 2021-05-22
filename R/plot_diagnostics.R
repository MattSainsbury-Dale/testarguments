#' Visualise diagnostics across the tested arguments.
#'
#' @param object an object of class \code{testargs} from a call to \code{test_arguments()}
#' @param focused_args the arguments we wish to plot. If \code{NULL} (default), all arguments are plotted
#' @param average_out_non_focused_args logical indicating whether we should average over the non-focused arguments
#' @param plot_order integer vector giving the order in which we are to assign arguments to the various aesthetics. If \code{plot_order = NULL} (default), the arguments are assigned based on their type (in the order 'numeric', 'integer', 'factor', 'character', 'logical'); otherwise, if \code{focused_args = NULL}, \code{plot_order} should be the same length as \code{object@arg_names}, and if \code{focused_args} is not \code{NULL}, \code{plot_order} should be the same length as \code{focused_args}
#' @return a facetted \code{ggplot} object, where:
#' \itemize{
#'  \item{the columns of the facet correspond to the diagnostic (e.g., RMSPE, CRPS, Time, etc.)}
#'  \item{the y-axis corresponds to the value of the diagnostic scores}
#'  \item{the x-axis corresponds to the values of the first argument}
#'  \item{the colour scale and grouping correspond to the second argument (if present)}
#'  \item{if a third argument is present, \code{facet_grid()} is used, whereby columns correspond to levels of the third argument, and rows correspond to diagnostics. Note that \code{facet_grid} forces a given row to share a common y-scale, so the plot would be misleading if diagnostics were kept as columns}
#'  \item{the shape of the points correspond to values of the fourth argument (if present)}
#' }
#' @seealso \code{\link{test_arguments}}
#' @export
#' @examples
#' ## See the example in ?test_diagnostics for this functions intended use
#' @export
plot_diagnostics <- function(object, focused_args = NULL,
                   average_out_non_focused_args = TRUE,
                   plot_order = NULL) {

  if (!is(object, "testargs"))
    stop("object should be of class 'testargs'")

  if(is.null(focused_args)) {
    focused_args <- object@arg_names
    if (!is.null(plot_order) && (length(plot_order) != length(object@arg_names))) {
      stop("plot_order should have the same length as object@arg_names")
    }
  } else {
    if (!all(focused_args %in% object@arg_names))
      stop("Some focused_args are not in the original argument names")

    if (!is.null(plot_order) && (length(plot_order) != length(focused_args))) {
      stop("plot_order should have the same length as focused_args")
    }
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
  g <- ggplot(long_df, aes_string(y = "value"))

  ## Define the plotting order of the focused arguments
  if (is.null(plot_order)) {
    ## If we have a mixture of numeric and character/factor arguments, it would be
    ## best to use the numeric argument for the x-axis.
    ## Our strategy is to sort the data frame columns in terms of data type, and
    ## and then use the sorted order for focused_args.
    my_order <- c('numeric', 'integer', 'factor', 'character', 'logical')
    long_df <- long_df %>% order_cols(my_order)
    idx <- which(names(long_df) %in% focused_args)
    focused_args <- names(long_df)[idx]
  } else {
    tmp <- c()
    for (i in object@arg_names[plot_order]) {
      if (i %in% focused_args)
        tmp <- c(tmp, i)
    }
    focused_args <- tmp
  }

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
    ## We also want to separate the lines based on this fourth argument
    ## (To prevent the "zig-zagging" that can occur otherwise).
    ## To do so, we need to make the grouping by the combination of the
    ## second argument and the fourth argument
    long_df$focused_args_2_and_4 <- paste(
      long_df[[focused_args[2]]],
      long_df[[focused_args[4]]]
      )
    g <- g %+% long_df
    g <- g + aes_string(group = "focused_args_2_and_4")
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

## Order columns of a data frame based on data type.
## Credit to: https://stackoverflow.com/a/50936293
globalVariables(c("."))
order_cols <- function(df, col_order){
 df %>%
    select(sapply(., class) %>% .[order(match(., col_order))] %>% names)
}

## Reshape the data so we have a single variable "diagnostic"; The rest of the
## variables are the arguments that we are trying to optimise over.
.long_diagnostic_df <- function(df, arg_names) {
  return(reshape2::melt(df, id = arg_names, variable.name = "Diagnostic"))
}


