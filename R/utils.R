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
#' @return
#' An `rml_fit` object (S3 class) that extends the reconciled model(s)
#' with reconciliation metadata. This object lets you pre-train the
#' reconciliation approach before base forecasts are available: the
#' fitted result can then be passed to the `fit` argument of [csrml],
#' [terml] or [ctrml] to reconcile new forecasts without refitting.
#' While the underlying list of models can be retrieved by extracting
#' the `fit` element, the object is primarily intended to be used
#' as-is.
#'
#' @examples
#' \donttest{
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
#' summary(mdl)
#' }
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
  te_label <- NULL
  cs_label <- NULL

  if (!is.null(x$agg_mat)) {
    if (!is.null(colnames(x$agg_mat))) {
      cs_label <- colnames(x$agg_mat)
    } else {
      cs_label <- paste0("Bottom series ", 1:length(x))
    }
  }

  print_tree_model(
    x$fit,
    framework = x$framework,
    cs_label = cs_label,
    te_label = x$agg_order
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
  sample_size = NULL,
  block_sampling = NULL
) {
  framework <- match.arg(
    framework,
    choices = c("cross-sectional", "temporal", "cross-temporal")
  )
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
      sample_size = sample_size,
      block_sampling = block_sampling
    ),
    class = "rml_fit"
  )
}


#' @export
#' @method print summary_rml_fit
print.summary_rml_fit <- function(x, ...) {
  #cli_rule(right = "FoReco reconciliation summary")
  frm <- paste0(
    toupper(substr(x$framework, 1, 1)),
    substr(x$framework, 2, nchar(x$framework))
  )
  cli_alert_info(
    "{.strong {frm}} reconciliation using Machine Learning methods"
  )

  method <- c()
  if (!is.null(x$approach)) {
    method <- c(
      method,
      "Machine Learning approach: {.strong {.code {x$approach}}}"
    )
  }

  str_frm <- c()
  if (!is.null(x$cs_n)) {
    str_frm <- c(str_frm, "Number of cross-sectional series: {x$cs_n}")
  }

  if (!is.null(x$te_set)) {
    str_frm <- c(
      str_frm,
      "Temporal orders (k): {x$te_set}"
    )
  }
  str_frm <- c(str_frm, "Number of features: {x$features_size}")
  str_frm <- c(str_frm, "Training sample size: {x$sample_size}")

  cli_ul(method)
  cli_ul(str_frm)
  if (!is.null(x$agg_mat)) {
    cli_h3("Cross-sectional linear combination matrix")
    print(x$agg_mat)
  }

  if (!is.null(x$object)) {
    cli_h3("Trained models")
    if (!is.null(x$agg_mat)) {
      if (!is.null(colnames(x$agg_mat))) {
        cs_label <- colnames(x$agg_mat)
      } else {
        cs_label <- paste0("Bottom series ", 1:length(x))
      }
    }
    print_tree_model(
      x$object,
      framework = x$framework,
      cs_label = cs_label,
      te_label = x$agg_order
    )
  }

  invisible(x)
}

#' @export
#' @method summary rml_fit

summary.rml_fit <- function(object, keep_models = TRUE, ...) {
  out <- list(
    agg_mat = object$agg_mat,
    framework = object$framework,
    features = object$features,
    approach = object$approach,
    features_size = object$features_size,
    sample_size = object$sample_size,
    agg_order = object$agg_order,
    n_model = length(object$fit)
  )
  if (!is.null(object$agg_order)) {
    out$te_set <- tetools(object$agg_order)$set
  }

  if (!is.null(object$agg_mat)) {
    out$cs_n <- sum(dim(object$agg_mat))
  }

  if (keep_models) {
    out$object <- object$fit
  }
  class(out) <- "summary_rml_fit"
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

style_comment <- cli::make_ansi_style(
  grDevices::grey(0.4),
  grey = TRUE,
  colors = 256
)

print_tree_model <- function(
  x,
  framework,
  cs_label = NULL,
  te_label = NULL,
  features = NULL
) {
  n <- length(x)

  cli::cli_text(
    "<{.strong rml_fit}: {.val {n}} model{?s}, {.emph {framework}} framework>"
  )
  if (framework == "cross-sectional") {
    list_label <- cs_label
  } else if (framework == "temporal") {
    if (is.null(te_label)) {
      list_label <- paste0("High-frequency level")
    } else {
      list_label <- paste0("High-frequency level (m = ", te_label, ")")
    }

    if (length(x) > 1) {
      list_label <- paste0(list_label, ", forecast horizon ", 1:length(x))
    }
  } else if (framework == "cross-temporal") {
    list_label <- cs_label
    if (is.null(te_label)) {
      list_label <- paste0(list_label, " at high-frequency level")
    } else {
      list_label <- paste0(
        list_label,
        " at high-frequency level (m = ",
        te_label,
        ")"
      )
    }

    if (length(list_label) < length(x)) {
      fh <- length(x) / length(list_label)
      list_label <- paste0(
        rep(list_label, each = fh),
        rep(paste0(", forecast horizon ", seq_len(fh)), length(list_label))
      )
    }
  }

  check <- rep(" \u251C\u2500", length(x))
  check[length(check)] <- " \u2514\u2500"
  class_objects <- sapply(x, function(m) {
    paste0("<", class(m)[1], ">")
  })
  cat(paste0(
    check,
    " ",
    list_label,
    ": ",
    style_comment(class_objects),
    "\n",
    collapse = ""
  ))
}
