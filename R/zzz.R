# NOTE: this file is mostly the same as patRoon's

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
