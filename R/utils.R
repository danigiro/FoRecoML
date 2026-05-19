#' Extract the Reconciled Model from a Reconciliation Results
#'
#' @description
#' Extract the fitted reconciled model(s) from a reconciliation
#' function's output (e.g., [csrml], [terml] and [ctrml]).
#' The model can be reused for forecast reconciliation in the
#' reconciliation functions.
#'
#' @param reco An object returned by a reconciliation function
#' (e.g., the result of [csrml], [terml] and [ctrml]).
#'
#' @return A named list with reconciliation information:
#'   \item{\code{sel_mat}}{Features used (e.g., the selected feature
#'   matrix or indices).}
#'   \item{\code{fit}}{List of reconciled models.}
#'   \item{\code{approach}}{The learning approach used (e.g., \code{"xgboost"},
#'   \code{"lightgbm"}, \code{"randomForest"}, \code{"mlr3"}).}
#'
#' @examples
#' # agg_mat: simple aggregation matrix, A = B + C
#' agg_mat <- t(c(1,1))
#' dimnames(agg_mat) <- list("A", c("B", "C"))
#'
#' # N_hat: dimension for the most aggregated training set
#' N_hat <- 100
#'
#' # ts_mean: mean for the Normal draws used to simulate data
#' ts_mean <- c(20, 10, 10)
#'
#' # hat: a training (base forecasts) feautures matrix
#' hat <- matrix(
#'   rnorm(length(ts_mean)*N_hat, mean = ts_mean),
#'   N_hat, byrow = TRUE)
#' colnames(hat) <- unlist(dimnames(agg_mat))
#'
#' # obs: (observed) values for bottom-level series (B, C)
#' obs <- matrix(
#'   rnorm(length(ts_mean[-1])*N_hat, mean = ts_mean[-1]),
#'   N_hat, byrow = TRUE)
#' colnames(obs) <- colnames(agg_mat)
#'
#' # h: base forecast horizon
#' h <- 2
#'
#' # base: base forecasts matrix
#' base <- matrix(
#'   rnorm(length(ts_mean)*h, mean = ts_mean),
#'   h, byrow = TRUE)
#' colnames(base) <- unlist(dimnames(agg_mat))
#'
#' # reco: reconciled forecasts matrix
#' reco <- csrml(base = base, hat = hat, obs = obs, agg_mat = agg_mat)
#'
#' mdl <- extract_reconciled_ml(reco)
#' mdl
#'
#' @export
extract_reconciled_ml <- function(reco) {
  if (inherits(reco, "rml_fit")) {
    cli_inform(
      "Input {.arg reco} is already an {.cls rml_fit}; returning it unchanged."
    )
    return(reco)
  }

  if (!inherits(reco, "foreco")) {
    cli_abort(
      c(
        "Failed to retrieve reconciliation info.",
        "i" = "{.arg reco} is not a {.emph foreco} object"
      )
    )
  }

  info <- tryCatch(
    suppressWarnings(summary(reco)),
    error = function(e) {
      cli_warn(
        "Failed to retrieve reconciliation info: {conditionMessage(e)}",
        call = NULL
      )
      return(NULL)
    }
  )

  if (is.null(info) || is.null(info$fit)) {
    cli_warn("No reconciled model information available.", call = NULL)
    return(invisible(NULL))
  }

  return(info$fit)
}

#' @export
#' @method print rml_fit
print.rml_fit <- function(x, ...) {
  n <- length(x$fit)

  cli::cli_text(
    "<{.strong rml_fit}: {.val {n}} model{?s}, {.emph {x$framework}}>"
  )

  invisible(x)
}

# Rombouts et al. (2025) matrix-form
input2rtw <- function(x, kset) {
  x <- FoReco2matrix(x, kset)
  x <- lapply(1:length(kset), function(i) {
    if (NCOL(x[[i]]) > 1) {
      tmp <- apply(x[[i]], 2, rep, each = kset[i])
      #colnames(tmp) <- paste0(colnames(tmp), "_", kset[i])
    } else {
      tmp <- rep(x[[i]], each = kset[i])
    }
    tmp
  })
  do.call(cbind, rev(x))
}

# Reconcile Using Machine Learning Models Class
#
# This function creates an object of class \code{reconcile_ml} that contains the
# necessary components to perform forecast reconciliation using machine learning
# models.
#
# @param features Character string specifying which features are used for model
#   training.
# @param features_size Optional numeric vector specifying the size of the
#   feature set to be used for model training.
# @param framework Character string specifying the reconciliation framework to
#   be used. Options include "\code{cs}" for cross-sectional, "\code{te}" for
#   temporal, and "\code{ct}" for cross-temporal.
# @param sel_mat Selection matrix/vector to be used to select the features
#   for each variable. It's strickly related to the \code{features} argument.
# @inheritParams ctrml
#
# @returns Returns a fitted object ([reconcile_ml] class) that can be used
#   for reconciliation.
#
# @export
new_rml_fit <- function(
  fit,
  agg_mat = NULL,
  agg_order = NULL,
  tew = NULL,
  sel_mat = NULL,
  approach = NULL,
  framework = NULL,
  features = NULL,
  features_size = NULL,
  block_sampling = NULL
) {
  framework <- match.arg(framework, choices = c("cs", "te", "ct"))
  structure(
    list(
      agg_mat = agg_mat,
      agg_order = agg_order,
      tew = tew,
      fit = fit,
      sel_mat = sel_mat,
      approach = approach,
      framework = framework,
      features = features,
      features_size = features_size,
      block_sampling = block_sampling
    ),
    class = "rml_fit"
  )
}


#' @export
#' @method print summary.rml_fit
print.summary.rml_fit <- function(x, ...) {
  cat("----- Reconciled models -----\n")
  cat("Framework:", x$framework, "\n")
  cat("Features:", x$features, "\n")
  cat("Approach:", x$approach, "\n")
  cat("  Models:", x$n_model, "\n")
}

#' @export
#' @method summary rml_fit
summary.rml_fit <- function(object, ...) {
  out <- list(
    framework = object$framework,
    features = object$features,
    approach = object$approach,
    n_model = length(x$fit)
  )
  class(out) <- "summary.rml_fit"
  return(out)
}

#' @export
#' @method plot rml_fit
plot.rml_fit <- function(x, which = NULL, ...) {
  fits <- x$fit
  if (is.null(which)) {
    which <- seq_along(fits)
  }
  approach <- x$approach

  for (i in which) {
    m <- fits[[i]]
    if (approach == "xgboost") {
      imp <- xgboost::xgb.importance(model = m)
      xgboost::xgb.plot.importance(imp, ...)
    } else if (approach == "lightgbm") {
      imp <- lightgbm::lgb.importance(m)
      lightgbm::lgb.plot.importance(imp, ...)
    } else if (approach == "randomForest") {
      randomForest::varImpPlot(m, ...)
    } else if (approach == "mlr3") {
      lrn <- if (inherits(m, "AutoTuner")) m$learner else m
      if ("importance" %in% lrn$properties) {
        imp <- sort(lrn$importance(), decreasing = TRUE)
        imp <- head(imp)
        barplot(rev(imp), horiz = TRUE, las = 1, xlab = "Importance", ...)
      } else {
        cli::cli_warn(
          "Variable importance is not available for the mlr3 learner
           {.val {lrn$id}}."
        )
      }
    }
    title(main = paste("Model", i))
  }
  invisible(x)
}
