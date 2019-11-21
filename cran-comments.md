## Test environments
* local OS X install, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

The install size is 6.3Mb because the README file contains a couple of images
that display the results of a toy analysis using the package. Given that this
is a visualization tool, I think it is important to keep these images in the
package.

## Reason for submission

The main function now returns a named list. This should make it easier for
users to access the results of the function.
