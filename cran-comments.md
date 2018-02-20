## Test environments

* local Windows 10, R 3.4.1
* win-builder (devel and release)
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.4.2
* Windows Server 2012 R2 x64 (build 9600), R 3.4.2 (on Appveyor)

## R CMD check results
There were no ERRORs, WARNINGS or NOTES.

## Comments from last submission:

'Most of your examples are wrapped in \dontrun{}, hence nothing gets tested. Please unwrap the examples if that is feasible and if they can be executed in < 5 sec for each Rd file or create additionally small toy examples.'

This is challenging as most functions required structural topic model objects, which contain several dense matrices (hence need a lot of RAM) and take a long time to compute. I nevertheless tried to come up with small examples that should run in < 5 seconds on average hardware.

'Please ensure that your functions or examples (save.image('stm_poliblog5k.RData')) do not write by default or in your examples in the user's home filespace. That is not allow by CRAN policies. Please only write/save files if the user has specified a directory.'

I adjusted the problematic example which now writes to a temp directory.
