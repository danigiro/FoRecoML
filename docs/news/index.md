# Changelog

## FoRecoML 1.1.1

- Reworked the [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) methods for
  `rml_fit` objects, which now return a more informative, structured
  overview of the fitted reconciliation (framework, machine-learning
  approach, problem dimensions, features, training sample size,
  combination matrix and trained models).
- [`csrml()`](https://danigiro.github.io/FoRecoML/reference/csrml.md),
  [`terml()`](https://danigiro.github.io/FoRecoML/reference/terml.md)
  and
  [`ctrml()`](https://danigiro.github.io/FoRecoML/reference/ctrml.md)
  now validate their arguments more strictly and fail with informative
  error messages that state the expected value and report the one
  actually supplied.

## FoRecoML 1.1.0

CRAN release: 2026-06-23

- Every reconciliation function now returns an object of the new S3
  class `foreco`, defined in FoReco. The objects are built through
  FoReco’s exported `new_foreco_class()` constructor and therefore
  integrate seamlessly with FoReco’s
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  `components()` methods.

- Revised the `title` field of every file so that all man page titles
  consistently follow title case.

## FoRecoML 1.0.0

CRAN release: 2026-04-21

- Cross-sectional, temporal and cross-temporal forecast reconciliation
  with machine learning
- Initial CRAN submission.
