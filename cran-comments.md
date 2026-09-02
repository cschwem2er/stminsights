## Re-submission (2nd attempt) of stminsights 0.4.4

This is a resubmission of version 0.4.4, which was previously
archived on 2026-02-01 due to an upstream dependency cascade
(huge -> netgwas -> stminsights). Both `huge` (2.0.1, 2026-08-04)
and `netgwas` (1.14.5, 2026-05-04) are back on CRAN.

The previous 0.4.4 submission (2026-09-02) was rejected by the
auto-checks due to a missing \usage section in get_network.Rd.
That has been fixed in this submission.

## Changes in 0.4.4 (vs. archived 0.4.3)

* `huge` moved from Imports to Suggests – stminsights no longer
  requires a live `huge`/`netgwas` on CRAN to install.
* Added `Depends: R (>= 4.1.0)` to reflect the use of `|>` and
  `\(…)` syntax (clears the previous NOTE).
* Added missing `\usage` section to `get_network.Rd` (fixes
  the Debian NOTE from the previous submission).

## Test environments

* local Windows 11, R 4.4.x (devtools::check)
* win-builder (devel and release)

## R CMD check results

Expected remaining NOTEs (informational only, not fixable by the
maintainer):
  - "Package was archived on CRAN" – standard flag for any
    resubmission of a previously archived package.

No ERRORs, no other NOTEs.
