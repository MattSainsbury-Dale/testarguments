testarguments
=========

An R package for testing and visualising the performance of a prediction algorithm with different combinations of function arguments. 
		
		
Installation tips
-------

To install the package, simply type the following command in R:

```r
devtools::install_github("MattSainsbury-Dale/testarguments")
```


Example
------------

We demonstrate how one may use testarguments using the package FRK. First, load required packages, and create training and testing data:

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

Define a function which uses df_train to predict over df_test. In this example, we wish to test values of the arguments link and nres, so we also include these as arguments. 

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

Define diagnostic function. Should return a named vector.

```r
diagnostic_fun <- function(df_test) {
  with(df_test,
       c(RMSPE = sqrt(mean((p_Z - Z)^2)),
         coverage = mean((Z > Z_percentile_5) & (Z < Z_percentile_95))))
}
```

Compute the user-defined diagnostics over a range of arguments. Here, we test the prediction algorithm with 1, 2, or 3 resolutions of basis functions, and using the log or square-root link function.

```r
diagnostics <- test_arguments(fun, df_train, df_test, diagnostic_fun,
                              arguments = list(link = c("log", "square-root"),
                                               nres = 1:3))
diagnostics
```

Visualise the performance across all combinations of the supplied arguments:

```r
plot_diagnostics(diagnostics, c("nres", "link"))
```

![Predictive performance for all combinations of nres and link](https://github.com/[MattSainsbury-Dale]/[testarguments]/img/[master]/nres_link.png?raw=true)

If we decide that the link function is not relevant, we can focus on only the number of resolutions by specifying focused_args = "nres":

```r
plot_diagnostics(diagnostics, c("nres", "link"), focused_args = "nres")
```
![Focusing on nres: levels of link have been averaged out](https://github.com/[MattSainsbury-Dale]/[testarguments]/img/[master]/nres.png?raw=true)



