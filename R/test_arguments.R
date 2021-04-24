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
#' ## First, load required packages.
#' library("testarguments")
#' library("FRK")
#' library("sp")
#' library("pROC") # AUC score
#'
#' ## Create training and testing data.
#' n <- 5000                                                  # sample size
#' RNGversion("3.6.0"); set.seed(1)
#' data("MODIS_cloud_df") # MODIS dataframe stored in FRK (FRKTMB branch)
#' train_id <- sample(1:nrow(MODIS_cloud_df), n, replace = FALSE)
#' df_train <- MODIS_cloud_df[train_id, ]                     # training set
#' df_test  <- MODIS_cloud_df[-train_id, ]                    # testing set
#'
## Define a function which uses df_train to predict over df_test.
## In this example, we wish to test values of the arguments link and nres,
## so we also include these as arguments.
#' fun <- function(df_train, df_test, link, nres) {
#'
#' ## Convert dataframes to Spatial* objects (as required by FRK)
#' coordinates(df_train) <- ~ x + y
#' coordinates(df_test) <- ~ x + y
#'
#' ## BAUs (just use a grid over the spatial domain of interest)
#' BAUs    <- SpatialPixelsDataFrame(points = expand.grid(x = 1:225, y = 1:150),
#'                                  data = expand.grid(x = 1:225, y = 1:150))
#'
#' ## Fit using df_train
#' df_train$k_Z <- 1 # size parameter of the binomial distribution
#' S <- FRK(f = z ~ 1, data = list(df_train), BAUs = BAUs, response = "binomial",
#'          link = link, nres = nres)
#'
#' ## Predict using df_test
#' pred <- predict(S, newdata = df_test, type = "response")
#'
#' ## Returned object must be a matrix-like object with named columns
#' return(pred$newdata@data)
#' }
#'
#' ## Define diagnostic function. Should return a named vector
#' diagnostic_fun <- function(df) {
#'   with(df, c(
#'     Brier = mean((z - p_Z)^2),
#'     AUC = as.numeric(pROC::auc(z, p_Z))
#'   ))
#' }
#'
#' ## Compute the user-defined diagnostics over a range of arguments.
#' ## Here, we test the prediction algorithm with 1, 2, or 3 resolutions of
#' ## basis functions, and using the logit or probit link function.
#' testargs_object <- test_arguments(
#'   fun, df_train, df_test, diagnostic_fun,
#'   arguments = list(link = c("logit", "probit"), nres = 1:3)
#' )
#'
#' ## Visualise the performance across all combinations of the supplied arguments:
#' plot_diagnostics(testargs_object)
# ggsave("./img/nres_link.png", device = "png", width = 6, height = 3)
#'
#' ## If we decide that some arguments are not relevant, we can focus on a
#' ## subset using the argument focused_args.
#' plot_diagnostics(diagnostics_df, focused_args = "nres")
# ggsave("./img/nres.png", device = "png", width = 6, height = 3)
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

  return(new("testargs",
      diagnostics_df = diagnostics,
      arg_names = names(arguments),
      diagnostic_names = names(diagnostics)[which(!(names(diagnostics) %in% names(arguments)))]
      ))
}
