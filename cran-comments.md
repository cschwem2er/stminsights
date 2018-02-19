## Test environments

* local Windows 10, R 3.4.1
* win-builder (devel and release)
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.4.2
* Windows Server 2012 R2 x64 (build 9600), R 3.4.2 (on Appveyor)

## R CMD check results
There were no ERRORs, WARNINGS or NOTES.

## Comments from last Submission:

'please write package names and software names in single quotes (e.g. 'shiny') in title and description.'

I now quoted shiny in the title and added a single quoted reference to stm in the description.

'Most of your code cannot be checked (especially the GUI part).'

I separated an additional function, `get_network()`, used in the GUI part, such that the code can be tested now. In future releases I will add further tests for other functions used in the gui

'Please ensure that your functions do not write by default or in your examples in the user's home filespace. That is not allow by CRAN policies. Please only write/save files if the user has specified a directory. In your examples you can write to tempdir(). '

I adjusted all plots for the GUI such that files will only be saved if the user specified a directory via a download handler.
