
# patRoonInst

The `patRoonInst` package is specifically used to automate and simplify installations of [patRoon]. Please refer to the `patRoon` [installation section in the handbook][handbook-inst], and the [reference documentation][ref] for more details.

## Installation

The latest version can be installed from [r-universe] or directly from GitHub:

```r
# installation from r-universe
install.packages("patRoonInst", repos = c('https://rickhelmus.r-universe.dev', 'https://cloud.r-project.org'))

# or alternatively, from GitHub
install.packages("remotes") # run this if the remotes (or devtools) package is not yet installed.
remotes::install_github("rickhelmus/patRoonInst")
```

[patRoon]: https://rickhelmus.github.io/patRoon/
[ref]: https://rickhelmus.github.io/patRoonInst/reference/index.html
[handbook-inst]: https://rickhelmus.github.io/patRoon/handbook_bd/installation.html
[r-universe]: https://rickhelmus.r-universe.dev/builds
