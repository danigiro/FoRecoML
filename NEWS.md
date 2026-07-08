# FoRecoML 1.1.1

* Reworked the `print()` and `summary()` methods for `rml_fit` objects, which now return a more informative, structured overview of the fitted reconciliation (framework, machine-learning approach, problem dimensions, features, training sample size, combination matrix and trained models).
* `csrml()`, `terml()` and `ctrml()` now validate their arguments more strictly and fail with informative error messages that state the expected value and report the one actually supplied.

# FoRecoML 1.1.0

* Every reconciliation function now returns an object of the new S3 class `foreco`, defined in FoReco. The objects are built through FoReco's exported `new_foreco_class()` constructor and therefore integrate seamlessly with FoReco's `print()`, `summary()`, `plot()` and `components()` methods.

* Revised the `title` field of every file so that all man page titles consistently follow title case.

# FoRecoML 1.0.0

* Cross-sectional, temporal and cross-temporal forecast reconciliation with machine learning
* Initial CRAN submission.
