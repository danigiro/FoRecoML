# Extract the Reconciled Model from a Reconciliation Results

Extract the fitted reconciled model(s) from a reconciliation function's
output (e.g.,
[csrml](https://danigiro.github.io/FoRecoML/reference/csrml.md),
[terml](https://danigiro.github.io/FoRecoML/reference/terml.md) and
[ctrml](https://danigiro.github.io/FoRecoML/reference/ctrml.md)). The
model can be reused for forecast reconciliation in the reconciliation
functions.

## Usage

``` r
extract_reconciled_ml(reco)
```

## Arguments

- reco:

  An object returned by a reconciliation function (e.g., the result of
  [csrml](https://danigiro.github.io/FoRecoML/reference/csrml.md),
  [terml](https://danigiro.github.io/FoRecoML/reference/terml.md) and
  [ctrml](https://danigiro.github.io/FoRecoML/reference/ctrml.md)).

## Value

An `rml_fit` object (S3 class) that extends the reconciled model(s) with
reconciliation metadata. This object lets you pre-train the
reconciliation approach before base forecasts are available: the fitted
result can then be passed to the `fit` argument of
[csrml](https://danigiro.github.io/FoRecoML/reference/csrml.md),
[terml](https://danigiro.github.io/FoRecoML/reference/terml.md) or
[ctrml](https://danigiro.github.io/FoRecoML/reference/ctrml.md) to
reconcile new forecasts without refitting. While the underlying list of
models can be retrieved by extracting the `fit` element, the object is
primarily intended to be used as-is.

## Examples

``` r
# \donttest{
# agg_mat: simple aggregation matrix, A = B + C
agg_mat <- t(c(1,1))
dimnames(agg_mat) <- list("A", c("B", "C"))

# N_hat: dimension for the most aggregated training set
N_hat <- 100

# ts_mean: mean for the Normal draws used to simulate data
ts_mean <- c(20, 10, 10)

# hat: a training (base forecasts) feautures matrix
hat <- matrix(
  rnorm(length(ts_mean)*N_hat, mean = ts_mean),
  N_hat, byrow = TRUE)
colnames(hat) <- unlist(dimnames(agg_mat))

# obs: (observed) values for bottom-level series (B, C)
obs <- matrix(
  rnorm(length(ts_mean[-1])*N_hat, mean = ts_mean[-1]),
  N_hat, byrow = TRUE)
colnames(obs) <- colnames(agg_mat)

# h: base forecast horizon
h <- 2

# base: base forecasts matrix
base <- matrix(
  rnorm(length(ts_mean)*h, mean = ts_mean),
  h, byrow = TRUE)
colnames(base) <- unlist(dimnames(agg_mat))

# reco: reconciled forecasts matrix
reco <- csrml(base = base, hat = hat, obs = obs, agg_mat = agg_mat)

mdl <- extract_reconciled_ml(reco)
mdl
#> <rml_fit: 2 models, cross-sectional framework>
#>  ├─ B: <randomForest>
#>  └─ C: <randomForest>
summary(mdl)
#> ℹ Cross-sectional reconciliation using Machine Learning methods
#> • Machine Learning approach: `randomForest`
#> • Number of cross-sectional series: 3
#> • Number of features: 3
#> • Training sample size: 100
#> 
#> ── Cross-sectional linear combination matrix 
#> 1 x 2 sparse Matrix of class "dgCMatrix"
#>   B C
#> A 1 1
#> 
#> ── Trained models 
#> <rml_fit: 2 models, cross-sectional framework>
#>  ├─ B: <randomForest>
#>  └─ C: <randomForest>
# }
```
