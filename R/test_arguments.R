#' Test (multiple) arguments of a prediction algorithm
#'
#' Test the performance of a prediction algorithm over a range of argument
#' values. Multiple arguments can be tested simultaneously.
#'
#' \code{fun} should have formal arguments \code{df_train} and \code{df_test},
#' which are data used to train the model and test out-of-sample predictive
#' performance, respectively, as well as any arguments which are to be tested.
#' Each argument should require simple values only (a single number, string, etc.).
#' The value of \code{fun} should be a matrix-like object
#' with named columns and the same number of rows as \code{df_test}. The output
#' of \code{fun} is \code{cbind} to \code{df_test}, which is then passed into
#' \code{diagnostic_fun} to compute the diagnostics. Hence, since the number of
#' columns in the returned value of \code{fun} is arbitrary, one can test both
#' predictions and prediction uncertainty (e.g., by including prediction
#' standard errors or predictive interval bounds in the returned value of \code{fun}).
#'
#' @param fun prediction function
#' @param df_train training data
#' @param df_test testing data
#' @param arguments named list of arguments to check
#' @param diagnostic_fun the criteria with which the predictive performance will
#' be assessed
#' @export
#' @return a data.frame whose columns contain the diagnostics and run time, and
#' each row corresponds to a combination of the provided arguments
#' @seealso \code{\link{plot_diagnostics}}
#' @importFrom plyr ldply
# #' @examples
test_arguments <- function(fun, df_train, df_test, diagnostic_fun, arguments) {

  if(!all(names(arguments) %in% names(formals(fun))))
    stop("names of arguments do not match the argument names of fun")

  ## Every combination of the arguments
  ## NB: This assumes that arguments are atomic only
  ## i.e., I don't think we could pass in the BAUs in this fashion; to do that,
  ## I would need a list version of expand.grid(). Cross that bridge if needed.
  ## Should add a check.
  arguments <- expand.grid(arguments)

  ## iterate over the rows of all combinations of arguments
  diagnostics <- ldply(seq_len(nrow(arguments)), function(i) {

    ## Convert current arguments to a list
    current_arguments <- as.list(arguments[i, ])

    ## Fit, predict, and record time
    ## Also need to pass in the training data and validation data
    ## (NB: here we assume that predict_function has arguments df_test and df_train; add a check for this)
    current_arguments <- c(list(df_test = df_test, df_train = df_train),
                           current_arguments)

    time <- system.time({
      pred <- do.call(fun, current_arguments)
    })["elapsed"]

    if (!is.matrix(pred) && !is.data.frame(pred))
      stop("fun should return a matrix or data.frame")

    if (nrow(pred) != nrow(df_test))
      stop("fun should return a matrix or data.frame with the *same number of rows* as df_test")

    if(is.null(names(pred))) {
      ## Some prediction algorithms return a matrix where the dimensions have
      ## names but names() is still null (e.g., lm()).
      if (!is.null(dimnames(pred)[[2]])) {
        names(pred) <- dimnames(pred)[[2]]
      } else {
        stop("fun needs to return a matrix or data.frame with *named* columns")
      }
    }


    ## Incorporate prediction results to df_test
    df_test <- cbind(df_test, pred)

    ## Compute the diagnostics and add the time
    current_diagnostics <- c(diagnostic_fun(df_test), Time = unname(time))

    ## Convert to data.frame so we can add the current arugments (which may
    ## contain strings)
    current_diagnostics <- as.data.frame(as.list(current_diagnostics))

    ## record the current arguments
    current_diagnostics[names(arguments)] <- arguments[i, ]

    return(current_diagnostics)
  })

  return(new("testargs",
      diagnostics_df = diagnostics,
      arg_names = names(arguments),
      diagnostic_names = names(diagnostics)[which(!(names(diagnostics) %in% names(arguments)))],
      plot_order = 1:length(names(arguments))
      ))
}
