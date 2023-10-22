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
#'   }
#'
"_PACKAGE"


# NOTE: the following functions are mostly the same as patRoon's

defaultPkgOpts <- function(pkgname)
{
    ret <- list(
        repos.patRoonDeps = "https://rickhelmus.github.io/patRoonDeps",
        repos.runiverse = "https://rickhelmus.r-universe.dev"
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
