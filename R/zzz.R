#' Manage patRoon installations
#'
#' \Sexpr[results=text,echo=FALSE]{packageDescription("patRoonInst", fields = "Description")}
#'
#' @section Package options:
#'
#'   The following package options (see [options()]) can be set:
#'
#'   \itemize{
#'
#'   \item `patRoonInst.repos.patRoonDeps` The URL to the `patRoonDeps` \R package repository.
#'
#'   \item `patRoonInst.repos.runiverse` The URL to the [r-universe](https://r-universe.dev/) \R package repository.
#'
#'   \item `patRoonInst.path.legacy` The directory where legacy files were installed.
#'   }
#'
#'
"_PACKAGE"

# NOTE: some random functions are imported here to ensure namespaces of dependencies are loaded and can be used when
# .libPaths() is changed during installations.

#' @importFrom stats setNames
#' @importFrom utils available.packages download.file installed.packages menu packageName read.csv
#' @importFrom BiocManager containerRepository
#' @importFrom jsonlite write_json
#' @importFrom remotes add_metadata
NULL

# NOTE: the following functions are mostly the same as patRoon's

defaultPkgOpts <- function(pkgname)
{
    ret <- list(
        repos.patRoonDeps = "https://rickhelmus.github.io/patRoonDeps",
        repos.runiverse = "https://rickhelmus.r-universe.dev",
        path.legacy = "~"
    )
    return(setNames(ret, paste0(pkgname, ".", names(ret))))
}

dumpPkgOpts <- function(printFunc)
{
    for (opt in names(defaultPkgOpts(utils::packageName())))
        printFunc(sprintf("- %s: \"%s\"", opt, getOption(opt)))
}

.onLoad <- function(libname, pkgname)
{
    # initialize any options that are unset
    dopts <- defaultPkgOpts(pkgname)
    missingOpts <- !names(dopts) %in% names(options())
    if (length(missingOpts) > 0)
        options(dopts[missingOpts])
}

.onAttach <- function(libname, pkgname)
{
    packageStartupMessage(sprintf("Welcome to %s %s!", pkgname, utils::packageVersion(pkgname)))
    packageStartupMessage("Configuration:")
    dumpPkgOpts(packageStartupMessage)
}
