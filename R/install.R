#' @include install-PD.R
NULL

#' Managing \pkg{patRoon} installations
#'
#' Functions to install, update and synchronize \pkg{patRoon} and its dependencies from various repositories.
#'
#' Details here:
#'
#' * Explain functions
#' * Explain origins
#'
#' @param origin Where \pkg{patRoon} and its dependencies are installed from. Valid values are: `"patRoonDeps"`,
#'   `"runiverse"`, `"regular"`. If `NULL` then the default on Windows is `"patRoonDeps"` and `"runiverse"` otherwise.
#'   See below for more details.
#' @param pkgs A `character` vector with a subset of packages to process. If `NULL` then all packages are considered.
#' @param ignorePkgs A `character` vector with packages that will not be considered. Can also be `"big"` to exclude
#'   large packages (_e.g._ \pkg{patRoonDeps}). If `NULL` then no packages will be ignored.
#' @param lib.loc The path to the \R library where packages will be installed. Set to `NULL` for the default \R library.
#' @param allDeps Consider _all_ dependencies, including recursive dependencies. This is mainly useful for `sync()`.
#'   Currently only supported for `origin="patRoonDeps"` and when `pkgs`/`ignorePkgs` are both `NULL`.
#' @param ask Set to `TRUE` to ask before proceeding package installations. No effect on non-interactive \R sessions.
#' @param force If `TRUE` then packages will _always_ be installed, even if already present and with the correct
#'   version.
#' @param quiet If `TRUE` the installations are performed more quietly (sets the `quiet` option to
#'   [install.packages()]).
#'
#' @name installing
NULL

doInstall <- function(action, origin, pkgs, ignorePkgs, lib.loc, allDeps, ask, quiet)
{
    if (is.null(origin))
        origin <- if (getOS() == "windows") "patRoonDeps" else "runiverse"

    ac <- checkmate::makeAssertCollection()
    checkmate::assertChoice(action, c("install", "force", "update", "sync"), add = ac)
    checkmate::assertChoice(origin, c("patRoonDeps", "runiverse", "regular"), add = ac)
    checkmate::assertCharacter(pkgs, null.ok = TRUE, min.chars = 1, any.missing = FALSE, min.len = 1, add = ac)
    checkmate::assertCharacter(ignorePkgs, null.ok = TRUE, min.chars = 1, any.missing = FALSE, add = ac)
    if (!is.null(lib.loc) && file.exists(lib.loc))
        checkmate::assertDirectoryExists(lib.loc, access = "w", add = ac)
    checkmate::assertFlag(allDeps, add = ac)
    checkmate::assertFlag(ask, add = ac)
    checkmate::assertFlag(quiet, add = ac)
    checkmate::reportAssertions(ac)

    if (allDeps)
    {
        if (!is.null(pkgs) || !is.null(ignorePkgs))
            stop("Cannot combine allDeps=TRUE with pkgs/ignorePkgs", call. = FALSE)
        if (origin != "patRoonDeps")
            stop("allDeps=TRUE currently only works with origin=\"patRoonDeps\"", call. = FALSE)
    }

    lp <- NULL
    if (!is.null(lib.loc))
    {
        # NOTE: .libPaths() is set here to prevent install.packages() (and derived functions) from looking for
        # dependencies outside the target library. Unfortunately, this won't remove the default library (ie .Library)
        # from the search path, but this library is often static
        # NOTE: withr is not used here since it cannot set include.site=FALSE
        lp <- .libPaths()
        on.exit(.libPaths(lp), add = TRUE)
        .libPaths(lib.loc, include.site = FALSE)
    }

    # prevent asking to build packages from source if newer is available
    # UNDONE: make optional?
    withr::local_options(list(install.packages.compile.from.source = "never"))

    directDeps <- getDirectDeps()

    checkPkgs <- function(p)
    {
        o <- setdiff(p, names(directDeps))
        if (length(o) > 0)
            stop(paste("The following packages are unknown:", paste0(o, collapse = ", ")))
    }

    if (!is.null(pkgs))
    {
        checkPkgs(pkgs)
        directDeps <- directDeps[pkgs]
    }
    if (!is.null(ignorePkgs))
    {
        if (ignorePkgs == "big")
            ignorePkgs <- c("patRoonData", "patRoonExt", "MetaCleanData")
        else
            checkPkgs(ignorePkgs)
        directDeps <- directDeps[!names(directDeps) %in% ignorePkgs]
    }
    if (length(directDeps) == 0)
    {
        printf("Nothing to install, aborting...")
        return(invisible(NULL))
    }

    # set lib.loc here as otherwise installed.packages() includes the default library (see comment above)
    instPackages <- getInstalledPackages(lib.loc)

    backend <- switch(origin,
                      patRoonDeps = installPD$new(),
                      runiverse = installRU$new(),
                      regular = installMain$new())
    pkgVersions <- backend$packageVersions(directDeps)

    # set rownames to simplify things
    rownames(instPackages) <- instPackages$Package; rownames(pkgVersions) <- pkgVersions$Package

    if (allDeps)
    {
        considerPackages <- merge(instPackages, pkgVersions, by = "Package", all.y = TRUE,
                                  suffix = c(".inst", ".avail"), sort = FALSE)
    }
    else
    {
        considerPackages <- data.frame(Package = names(directDeps))
        considerPackages <- merge(considerPackages, instPackages, by = "Package", all.x = TRUE, sort = FALSE)
        considerPackages <- merge(considerPackages, pkgVersions, by = "Package", all.x = TRUE, all.y = allDeps,
                                  suffix = c(".inst", ".avail"), sort = FALSE)
    }

    if (action == "force")
        considerPackages$action <- "force" # just install everything
    else
    {
        shouldInstall <- function(pkgs) is.na(pkgs$Version.inst) & is.na(pkgs$RemoteSha.inst)
        shouldUpdate <- if (action == "update")
            function(pkgs) is.na(pkgs$RemoteSha.avail) & pkgs$Version.inst < pkgs$Version.avail
        else
            function(pkgs) is.na(pkgs$RemoteSha.avail) & pkgs$Version.inst != pkgs$Version.avail
        shouldGHSync <- function(pkgs) !is.na(pkgs$RemoteSha.avail) & (is.na(pkgs$RemoteSha.inst) | pkgs$RemoteSha.inst != pkgs$RemoteSha.avail)

        considerPackages[shouldInstall(considerPackages), "action"] <- "install"
        if (action != "install")
        {
            considerPackages[is.na(considerPackages$action) & shouldUpdate(considerPackages), "action"] <- action # update/sync
            considerPackages[is.na(considerPackages$action) & shouldGHSync(considerPackages), "action"] <- "sync"
        }
    }

    considerPackages <- considerPackages[!is.na(considerPackages$action), ]

    if (nrow(considerPackages) == 0)
    {
        printf("The current installation seems already fine.\n")
        return(invisible(NULL))
    }

    printActions <- function(what, pkgAction)
    {
        whpkgs <- considerPackages[considerPackages$action == pkgAction, "Package"]
        if (length(whpkgs) > 0)
            printf("The following %d packages will be %s: %s\n", length(whpkgs), what, paste0(whpkgs, collapse = ", "))
    }

    printActions("installed (forced)", "force")
    printActions("installed", "install")
    printActions("updated", "update")
    printActions("synchronized", "sync")

    if (!ask || askProceed())
    {
        if (!is.null(lib.loc) && !dir.exists(lib.loc))
        {
            if (!dir.create(lib.loc, recursive = TRUE))
                stop("Could not create directory set to lib.loc", call. = FALSE)
        }

        backend$install(considerPackages, directDeps, quiet)

        # verify if all packages and their deps were installed
        deps <- unique(unlist(tools::package_dependencies(considerPackages$Package, db = backend$availablePackages(),
                                                          recursive = TRUE)))
        instPackages <- getInstalledPackages(lib.loc) # update
        notInstalled <- setdiff(deps, instPackages$Package)

        if (length(notInstalled) > 0)
        {
            stop(sprintf("The following %d packages were not installed: %s", length(notInstalled),
                         paste0(notInstalled, collapse = ", ")),
                 call. = FALSE)
        }
    }

    invisible(NULL)
}

#' @rdname installing
#' @export
install <- function(origin = NULL, pkgs = NULL, ignorePkgs = NULL, lib.loc = NULL, allDeps = FALSE, ask = TRUE,
                    force = FALSE, quiet = TRUE)
{
    doInstall(action = if (force) "force" else "install", origin = origin, pkgs = pkgs, ignorePkgs = ignorePkgs,
              lib.loc = lib.loc, allDeps = allDeps, ask = ask, quiet = quiet)
}

#' @rdname installing
#' @export
update <- function(origin = NULL, pkgs = NULL, ignorePkgs = NULL, lib.loc = NULL, allDeps = FALSE, ask = TRUE,
                   quiet = TRUE)
{
    doInstall(action = "update", origin = origin, pkgs = pkgs, ignorePkgs = ignorePkgs, lib.loc = lib.loc,
              allDeps = allDeps, ask = ask, quiet = quiet)
}

#' @rdname installing
#' @export
sync <- function(origin = NULL, pkgs = NULL, ignorePkgs = NULL, lib.loc = NULL, allDeps = FALSE, ask = TRUE,
                 quiet = TRUE)
{
    doInstall(action = "sync", origin = origin, pkgs = pkgs, ignorePkgs = ignorePkgs, lib.loc = lib.loc,
              allDeps = allDeps, ask = ask, quiet = quiet)
}
