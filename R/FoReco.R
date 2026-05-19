# Trasform the temporal [(h*kt) x 1] vector into a [h x kt] matrix
# See also: hmat2vec()
vec2hmat <- function(vec, h, kset) {
  m <- max(kset)
  i <- rep(rep(1:h, length(kset)), rep(m / kset, each = h))
  matrix(vec[order(i)], nrow = h, byrow = T)
}

# Trasform the [h x kt] matrix into a temporal [(h*kt) x 1] vector
# See also: mat2hmat()
hmat2vec <- function(hmat, h, kset) {
  m <- max(kset)
  i <- rep(1:sum(m / kset), h)
  it <- rep(rep(m / kset, m / kset), h)
  ih <- rep(1:h, each = sum(m / kset))
  out <- as.vector(t(hmat))[order(it, ih, i)]
  names_vec <- namesTE(kset = kset, h = h)
  setNames(out, names_vec)
}

# Build a named vector to specify k and h position
namesTE <- function(kset, h) {
  m <- max(kset)
  seqk <- h * (m / kset)
  paste0("k-", rep(kset, seqk), " h-", Reduce("c", sapply(seqk, seq_len)))
}

# Trasform the cross-temporal [n x (h*kt)] matrix into a [h x (n*kt)] matrix
# See also: hmat2mat()
mat2hmat <- function(mat, h, kset, n) {
  m <- max(kset)
  i <- rep(rep(rep(1:h, length(kset)), rep(m / kset, each = h)), n)
  vec <- as.vector(t(mat))
  matrix(vec[order(i)], nrow = h, byrow = T)
}

# Trasform the [h x (n*kt)] matrix into a cross-temporal [n x (h*kt)] matrix
# See also: mat2hmat()
hmat2mat <- function(hmat, h, kset, n) {
  m <- max(kset)
  i <- rep(1:sum(m / kset), h * n)
  it <- rep(rep(m / kset, m / kset), h * n)
  ih <- rep(1:h, each = n * sum(m / kset))
  out <- matrix(as.vector(t(hmat))[order(it, ih, i)], nrow = n)
  colnames(out) <- namesTE(kset = kset, h = h)
  out
}

# Split cross-temporal matrix in a temporal list
mat2list <- function(mat, kset) {
  m <- max(kset)
  h <- NCOL(mat) / sum(kset)
  kid <- rep(kset, h * m / kset)
  split.data.frame(t(mat), kid)[as.character(kset)]
}

.drop_foreco <- function(x) {
  if (inherits(x, "foreco")) {
    attr(x, "FoReco") <- NULL
    cl <- setdiff(class(x), "foreco")
    if (length(cl) == 0) {
      x <- unclass(x)
    } else {
      class(x) <- cl
    }
  }
  x
}
# Reconciled Forecasts to Matrix/Vector
#
# @description
# This function splits the temporal vectors and the cross-temporal matrices
# in a list according to the temporal aggregation order
#
# @param x An output from any reconciliation function implemented by
# \pkg{FoReco}.
# @inheritParams ctrec
# @param keep_names If \code{FALSE} (\emph{default}), the rownames names of
# the output matrices are removed.
# @param temporal_names A character vector containing the names of the temporal
# aggregation levels.
#
# @returns A list of matrices or vectors distinct by temporal aggregation
# order.
#
# @family Utilities
# @examples
# set.seed(123)
# # (3 x 7) base forecasts matrix (simulated), Z = X + Y and m = 4
# base <- rbind(rnorm(7, rep(c(20, 10, 5), c(1, 2, 4))),
#               rnorm(7, rep(c(10, 5, 2.5), c(1, 2, 4))),
#               rnorm(7, rep(c(10, 5, 2.5), c(1, 2, 4))))
#
# reco <- ctrec(base = base, agg_mat = t(c(1,1)), agg_order = 4, comb = "ols")
# matrix_list <- FoReco2matrix(reco)
#
# # With temporal names
# temporal_names <- c("Annual", "Semi-annual", "Quarterly")
# matrix_list <- FoReco2matrix(reco, temporal_names = temporal_names)
FoReco2matrix <- function(
  x,
  agg_order,
  keep_names = FALSE,
  temporal_names = NULL
) {
  if (!is.null(attr(x, "FoReco"))) {
    fr <- summary(x)
    frame <- fr$framework
    set <- fr$te_set
    h <- fr$forecast_horizon
  } else if (!missing(agg_order)) {
    frame <- ifelse(NCOL(x) == 1, "Temporal", "Cross-temporal")
    set <- tetools(agg_order = agg_order)$set
    h <- ifelse(NCOL(x) == 1, length(x), NCOL(x)) / sum(max(set) / set)
  } else {
    frame <- "cross-sectional"
  }

  if (frame == "cross-sectional") {
    attr(x, "FoReco") <- NULL
    return(list("k-1" = x))
  } else {
    id <- rep(set, h * max(set) / set)

    if (NCOL(x) == 1) {
      out <- split(x, factor(id, set))
      if (!keep_names) {
        out <- lapply(out, unname)
      }
    } else {
      out <- lapply(setNames(set, set), function(k) {
        mat <- t(x[, id == k, drop = FALSE])
        if (!keep_names) {
          rownames(mat) <- NULL
        }
        mat
      })
    }

    names(out) <- paste0("k-", names(out))
    if (!is.null(temporal_names)) {
      if (length(temporal_names) == length(out)) {
        names(out) <- paste0(temporal_names, " (", names(out), ")")
      } else {
        cli_warn(
          "Length of {.arg temporal_names} is different from the number of temporal aggregation levels.",
          call = NULL
        )
      }
    }
    return(out)
  }
}
