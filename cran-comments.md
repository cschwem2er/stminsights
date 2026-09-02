## Re-submission after CRAN archive

This is a re-submission of stminsights 0.4.4 following the archiving of
stminsights 0.4.3 on 2026-02-01. The archiving was caused by an upstream
dependency cascade, not by an error in stminsights itself:

  huge  ->  netgwas  ->  stminsights

`huge` was removed from CRAN at the time, which led to the removal of
`netgwas` ("requires archived package 'huge'"), which in turn led to the
removal of stminsights ("requires archived package 'netgwas'").

Both `huge` (2.0.1, published 2026-08-04) and `netgwas` (1.14.5,
published 2026-05-04) are now back on CRAN. In addition, this version
decouples stminsights from a hard `huge` dependency so that a future
archiving of `huge` (or `netgwas`) can no longer take stminsights down.

## Changes in 0.4.4

* `huge` moved from `Imports` to `Suggests`. stminsights never calls
  `huge::huge()` directly. The only code path that uses `huge` is
  `stm::topicCorr(model, method = 'huge')`, which loads `huge` from
  `stm`'s own `Suggests`. Users who select `method = 'huge'` get the
  same "please install huge" message that `stm` itself gives.
* Added `Depends: R (>= 4.1.0)`, reflecting the use of the pipe `|>`
  and the function shorthand `\(…)` syntax (this clears the NOTE
  previously raised by the CRAN checks).

## Test environments

* local macOS, R 4.4.x (devtools::check)
* local Windows 11, R 4.4.x
* win-builder (devel and release)

## R CMD check results

No ERRORs or WARNINGS.
