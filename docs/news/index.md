# Changelog

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
