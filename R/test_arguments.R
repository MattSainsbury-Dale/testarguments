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
#' of \code{fun} is \code{cbind} to \code{df_test}, which is then into
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
#' @examples
#' ## Demonstrate using the package FRK.
#' ## First, load required packages, and create training and testing data:
#' library("testarguments")
#' library("FRK")
#' library("sp")
#' data("Poisson_simulated")
#' n <- nrow(Poisson_simulated)
#' RNGversion("3.6.0"); set.seed(1)
#' train_id <- sample(1:n, round(n / 2))
#' df_train <- Poisson_simulated[train_id, ]
#' df_test  <- Poisson_simulated[-train_id, ]
#'
#' ## Define a function which uses df_train to predict over df_test.
#' ## In this example, we wish to test values of the arguments link and nres,
#' ## so we also include these as arguments.
#' fun <- function(df_train, df_test, link, nres) {
#'
#'   ## Convert dataframes to Spatial* objects (as required by FRK)
#'   coordinates(df_train) <- ~ x + y
#'   coordinates(df_test) <- ~ x + y
#'
#'   BAUs <- auto_BAUs(manifold = plane(), data = rbind(df_train, df_test))
#'
#'   ## Fit using df_train, predict at df_test locations
#'   S <- FRK(f = Z ~ 1, data = list(df_train), BAUs = BAUs, response = "poisson",
#'            link = link, nres = nres)
#'   pred <- predict(S, newdata = df_test, type = "response")
#'
#'   ## NB: returned object needs to be a matrix or data.frame with named columns
#'   return(pred$newdata@data)
#' }
#'
#' ## Define diagnostic function. Should return a named vector
#' diagnostic_fun <- function(df_test) {
#'   with(df_test,
#'        c(RMSPE = sqrt(mean((p_Z - Z)^2)),
#'          coverage = mean((Z > Z_percentile_5) & (Z < Z_percentile_95))))
#' }
#'
#' ## Compute the user-defined diagnostics over a range of arguments.
#' ## Here, we test the prediction algorithm with 1, 2, or 3 resolutions of
#' ## basis functions, and using the log or square-root link function.
#' diagnostics_df <- test_arguments(fun, df_train, df_test, diagnostic_fun,
#'                                  arguments = list(link = c("log", "square-root"),
#'                                                   nres = 1:3))
#'
#' ## Visualise the performance across all combinations of the supplied arguments:
#' plot_diagnostics(diagnostics_df, c("nres", "link"))
# ggsave(
#   filename = "nres_link.png", device = "png", width = 6, height = 3,
#   path = "~/Dropbox/testarguments/img/"
# )
#'
#' ## If we decide that the link function is not relevant, we can focus on only
#' ## the number of resolutions by specifying focused_args = "nres".
#' plot_diagnostics(diagnostics_df, c("nres", "link"), focused_args = "nres")
# ggsave(
# filename = "nres.png", device = "png", width = 6, height = 3,
# path = "~/Dropbox/testarguments/img/"
# )
test_arguments <- function(fun, df_train, df_test, diagnostic_fun, arguments) {

  if(!all(names(arguments) %in% names(formals(fun))))
    stop("names of arguments do not match the argument names of fun")

  ## Every combination of the arguments
  ## NB: This assumes that arugments are atomic only
  ## i.e., I don't think we could pass in the BAUs in this fashion; to do that,
  ## I would need a list version of expand.grid(). Cross that bridge if needed.
  ## Should add a check.
  arguments <- expand.grid(arguments)

  ## iterate over the rows of all combinations of arguments
  diagnostics <- plyr::ldply(seq_len(nrow(arguments)), function(i) {

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

    if(is.null(names(pred)))
      stop("fun needs to return a matrix or data.frame with *named* columns")

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

  return(diagnostics)
}
