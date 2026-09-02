# stminsights 0.4.4

## Fixes for CRAN re-submission

* moved `huge` from Imports to Suggests – the package no longer requires
  a live `huge`/`netgwas` on CRAN to install
* added `Depends: R (>= 4.1.0)` to reflect the use of `|>` and `\(…)`
  syntax
* clarified documentation for `get_network(method = 'huge')`

# stminsights 0.4.3

## minor fixes

* adjusted visualization for correlation networks
* removed document correlation tab for shiny app (deprecated functions)

# stminsights 0.4.2

## package dependency updates

# stminsights 0.4.1

## minor improvements and bug fixes

* update dependencies to fix other bugs related to Shiny

# stminsights 0.4.0

## minor improvements and bug fixes

* fixes several bugs introduced by changes in the Shiny package
* improved error messages for correlation graphs in the interactive app
* updated documentation, readme and intro vignette

# stminsights 0.3.2

## new feature

* added plot for pointestimate interaction effects

# stminsights 0.3.1

## New features (implemented by Jonne Guyt)

* number of documents to be sampled from can now be set by user
* a minimum threshold (theta) for topic prevalence can be set by user when displaying relevant documents per topic

## Minor improvements

* visual demarcations to highlight different options/panels

# stminsights 0.3.0

## New features (implemented by Jonne Guyt)

* representative documents are now searchable
* representative document table now contains (optional) information on: row id, STM document ID and theta
* proportions can now be plotted for each individual document using the STM document ID
* documents can now be plotted in a (clickable) 2d scatter plot that shows proportions on two topics simultaneously
* document information displayed upon clicking scatter plot

## Minor improvements

* popup texts moved such that inputs can be selected more easily
* default options for document inspection changed

# stminsights 0.2.3

## Minor improvements

* multiple variables can now be selected for inspecting representative documents of topics

# stminsights 0.2.2

## Update Dependencies

* updated package dependencies to fix installation errors

# stminsights 0.2.1

## Bug Fix

* fixed a bug for storing topic labels in a spreadsheet
* increased the number of documents for inspecting topics


# stminsights 0.2.0

## Minor improvements

* include vignette for an introduction to stminsights
* include url for bug reports
* update readme


# stminsights 0.1.2

## Minor improvements / Bug Fix / CRAN Release

* fixed a bug in get_effects (#5)
* added 'difference' effect estimates in get_effects
* the package is now available on CRAN: https://cran.r-project.org/web/packages/stm/index.html

# stminsights 0.1.1

## Minor improvements

* added option for labeling axes
* added more options for content model term selection

# stminsights 0.1.0

## First release

* This is the first release of the package.
