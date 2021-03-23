testarguments
=========

An R package for testing and visualising the performance of a prediction algorithm with different combinations of function arguments. These functions are useful for optimising the predictive performance of a model when the model has a number of arguments to specify (e.g., choosing the number of basis functions, the number of resolutions, different priors, link functions, etc.). This is particularly true if one suspects an interaction between argument levels. 
 When in the context of a comparative study, these functions can help to select arguments in a systematic fashion.
		
		
Installation tips
-------

To install the package, simply type the following command in R:

```r
devtools::install_github("MattSainsbury-Dale/testarguments")
```


Example
------------

We demonstrate use **testarguments** by predicting with the package **FRK**. Note that the prediction algorithm tested by **testarguments** is arbitrary and user defined, and we use **FRK** only because I am familiar with it. First, load the required packages and data, and create training and testing data:

```r
library("testarguments")
library("FRK")
library("sp")
data("Poisson_simulated")
n <- nrow(Poisson_simulated)
train_id <- sample(1:n, round(n/2))
df_train <- Poisson_simulated[train_id, ]
df_test  <- Poisson_simulated[-train_id, ]
```

Define a wrapper function which uses `df_train` to predict over `df_test`. This will be passed into `test_arguments()` In this example, we wish to test values of `link` and `nres`, so we also include these as arguments. 

```r
fun <- function(df_train, df_test, link, nres) {
  
  ## Convert dataframes to Spatial* objects (as required by FRK)
  coordinates(df_train) <- ~ x + y
  coordinates(df_test) <- ~ x + y
  
  BAUs <- auto_BAUs(manifold = plane(), data = rbind(df_train, df_test))
  
  ## Fit using df_train, predict at df_test locations
  S <- FRK(f = Z ~ 1, data = list(df_train), BAUs = BAUs, response = "poisson",
           link = link, nres = nres)
  pred <- predict(S, newdata = df_test, type = "response")
  
  ## NB: returned object needs to be a matrix or data.frame with named columns
  return(pred$newdata@data)
}
```

Define diagnostic function. This should return a named vector.

```r
diagnostic_fun <- function(df_test) {
  with(df_test,
       c(RMSPE = sqrt(mean((p_Z - Z)^2)),
         coverage = mean((Z > Z_percentile_5) & (Z < Z_percentile_95))))
}
```

Compute the user-defined diagnostics over a range of arguments using `test_arguments()`. Here, we test the prediction algorithm with 1, 2, or 3 resolutions of basis functions, and using the log or square-root link function.

```r
diagnostics <- test_arguments(fun, df_train, df_test, diagnostic_fun,
                              arguments = list(link = c("log", "square-root"),
                                               nres = 1:3))
```

Visualise the performance across all combinations of the supplied arguments using `plot_diagnostics()`:

```r
plot_diagnostics(diagnostics, c("nres", "link"))
```

![Predictive performance for all combinations of nres and link](/img/nres_link.png?raw=true)


If we decide that the link function is not relevant, we can focus on only the number of resolutions by specifying `focused_args = "nres"`. By default, this averages out the arguments which are not of interest. 

```r
plot_diagnostics(diagnostics, c("nres", "link"), focused_args = "nres")
```

![Focusing on nres: levels of link have been averaged out](/img/nres.png?raw=true)


Using various aesthetics, `plot_diagnostics()` can visualise the performance of all combinations of up to 4 different arguments simultaneously.

