#' @include install-PD.R
NULL

#' Managing \pkg{patRoon} installations
#'
#' Functions to install, update and synchronize \pkg{patRoon} and its dependencies from various repositories.
#'
#' The installation of [patRoon](https://github.com/rickhelmus/patRoon) involves installing several dependencies,
#' including \R packages not available from common repositories (CRAN, BioConductor) and software outside the \R
#' environment. The following three functions automate this process and are used to install and maintain \pkg{patRoon}
#' and its dependencies:
#'
#' * `install()` Installs \pkg{patRoon} and any missing dependencies.
#' * `update()` Like `install()` and updates outdated packages.
#' * `sync()` Like `update()` but may also downgrade packages to fully synchronize package versions with the repository.
#'
#' The \R packages are currently sourced from the following repositories (set by the `origin` argument):
#'
#' * `"patRoonDeps"`: a specialized \CRANpkg{miniCRAN} repository contains binary \R packages with versions that are automatically tested with \pkg{patRoon}. Currently this is _only_ supported for Windows.
#' * `"runiverse"`: an [r-universe](https://rickhelmus.r-universe.dev/builds) repository containing the latest versions of \pkg{patRoon} and its dependencies.
#' * "regular": the last versions are sourced from regular CRAN/BioConductor repositories and GitHub. The latter means that suitable build tools (_e.g._ [Rtools on windows](https://cran.r-project.org/bin/windows/Rtools/)).
#'
#' Note that some large \R packages, currently `patRoonDeps`, `patRoonExt` and `MetaCleanData`, are always sourced
#' directly from GitHub.
#'
#' Synchronization with `sync()` was mainly designed to be used in combination with the `patRoonDeps` repository, since
#' it contains package versions already tested with `patRoon`. Doing a synchronization is most commonly used with
#' \pkg{patRoon} bundle installations or when \pkg{patRoon} is installed in a separate \R library (_i.e._ using the
#' `lib.loc` argument). Furthermore, the `allDeps` argument should be set to `TRUE` to ensure _all_ dependencies are
#' synchronized.
#'
#' Packages that originate from GitHub (even when obtained via `patRoonDeps`/`runiverse`) will _also_ be synchronized
#' with `update()`, since these packages typically involve 'snapshots' with unreliable version information.
#'
#' @param pkgs,ignorePkgs A `character` vector with packages to consider/ignore. Execute `names(getDirectDeps())` to
#'   obtain valid package options. If `NULL` then all packages are considered/none are ignored. The `ignorePkgs`
#'   argument can also include `"big"` to exclude large packages (_e.g._ \pkg{patRoonDeps}), and will override `pkgs` in
#'   case of conflicts.
#' @param origin Where \pkg{patRoon} and its \R dependencies are installed from. Valid values are: `"patRoonDeps"`,
#'   `"runiverse"`, `"regular"`. If `NULL` then the default on Windows is `"patRoonDeps"` and `"runiverse"` otherwise.
#'   See below for more details.
#' @param lib.loc The path to the \R library where packages will be installed. Set to `NULL` for the default \R library.
#' @param allDeps Consider _all_ dependencies when synchronizing, including recursive dependencies. This is currently
#'   only supported for `origin="patRoonDeps"`. Note that handling of recursive dependencies currently are not
#'   influenced by the `pkgs` and `ignorePkgs` arguments.
#' @param ask Set to `TRUE` to ask before proceeding package installations. No effect on non-interactive \R sessions.
#' @param force If `TRUE` then packages will _always_ be installed, even if already present and with the correct
#'   version.
#' @param quiet If `TRUE` the installations are performed more quietly (sets the `quiet` option to [install.packages()].
#'
#' @return All functions return `NULL` invisibly.
#'
#' @name installing
NULL

doInstall <- function(action, pkgs, ignorePkgs, origin, lib.loc, allDeps, ask, quiet)
{
    if (is.null(origin))
        origin <- if (getOS() == "windows") "patRoonDeps" else "runiverse"

    ac <- checkmate::makeAssertCollection()
    checkmate::assertChoice(action, c("install", "force", "update", "sync"), add = ac)
    checkmate::assertCharacter(pkgs, null.ok = TRUE, min.chars = 1, any.missing = FALSE, min.len = 1, add = ac)
    checkmate::assertChoice(origin, c("patRoonDeps", "runiverse", "regular"), add = ac)
    checkmate::assertCharacter(ignorePkgs, null.ok = TRUE, min.chars = 1, any.missing = FALSE, add = ac)
    if (!is.null(lib.loc) && file.exists(lib.loc))
        checkmate::assertDirectoryExists(lib.loc, access = "w", add = ac)
    checkmate::assertFlag(allDeps, add = ac)
    checkmate::assertFlag(ask, add = ac)
    checkmate::assertFlag(quiet, add = ac)
    checkmate::reportAssertions(ac)

    if (allDeps && origin != "patRoonDeps")
        stop("allDeps=TRUE currently only works with origin=\"patRoonDeps\"", call. = FALSE)

    basePackages <- getBasePackages() # NOTE: do before setting .libPaths() below

    lp <- NULL
    if (!is.null(lib.loc))
    {
        # NOTE: .libPaths() is set here to prevent install.packages() (and derived functions) from looking for
        # dependencies outside the target library. Unfortunately, this won't remove the default library (ie .Library)
        # from the search path, but this library is often static

        # NOTE: the directory needs to exist in order to be accepted by .libPaths()
        if (!dir.exists(lib.loc))
        {
            if (!dir.create(lib.loc, recursive = TRUE))
                stop("Could not create directory set to lib.loc", call. = FALSE)
        }

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
        {
            stop(paste("The following packages are unknown:", paste0(o, collapse = ", "),
                       "Valid options are:", paste0(names(directDeps), collapse = ", ")))
        }
    }

    if (!is.null(pkgs))
    {
        checkPkgs(pkgs)
        ip <- setdiff(names(directDeps), pkgs)
        ignorePkgs <- if (is.null(ignorePkgs)) ip else union(ignorePkgs, ip)
    }
    if (!is.null(ignorePkgs))
    {
        if ("big" %in% ignorePkgs)
            ignorePkgs <- union(setdiff(ignorePkgs, "big"), c("patRoonData", "patRoonExt", "MetaCleanData"))
        checkPkgs(ignorePkgs)
    }
    else
        ignorePkgs <- character()

    if (setequal(names(directDeps), ignorePkgs))
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

    pkgVersions <- if (origin == "patRoonDeps")
        backend$packageVersions(directDeps, ignorePkgs, allDeps)
    else
        backend$packageVersions(directDeps, ignorePkgs)

    considerPackages <- merge(instPackages, pkgVersions, by = "Package", all.y = TRUE,
                              suffix = c(".inst", ".avail"), sort = FALSE)

    # sync order so that deps are installed in the right order
    ord <- match(considerPackages$Package, names(directDeps), nomatch = 0)
    considerPackages <- considerPackages[order(ord), ]
    
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
        backend$install(considerPackages, directDeps, quiet)

        # verify if all packages and their deps were installed
        deps <- unique(unlist(tools::package_dependencies(considerPackages$Package, db = backend$availablePackages(),
                                                          recursive = TRUE)))
        deps <- union(deps, considerPackages$Package)
        instPackages <- getInstalledPackages(lib.loc) # update
        notInstalled <- setdiff(deps, instPackages$Package)
        notInstalled <- setdiff(notInstalled, basePackages)

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
install <- function(pkgs = NULL, ignorePkgs = NULL, origin = NULL, lib.loc = NULL, ask = TRUE, force = FALSE,
                    quiet = TRUE)
{
    doInstall(action = if (force) "force" else "install", pkgs = pkgs, ignorePkgs = ignorePkgs, origin = origin,
              lib.loc = lib.loc, allDeps = FALSE, ask = ask, quiet = quiet)
}

#' @rdname installing
#' @export
update <- function(pkgs = NULL, ignorePkgs = NULL, origin = NULL, lib.loc = NULL, ask = TRUE, quiet = TRUE)
{
    doInstall(action = "update", pkgs = pkgs, ignorePkgs = ignorePkgs, origin = origin, lib.loc = lib.loc, 
              allDeps = FALSE, ask = ask, quiet = quiet)
}

#' @rdname installing
#' @export
sync <- function(pkgs = NULL, ignorePkgs = NULL, origin = NULL, lib.loc = NULL, allDeps = FALSE, ask = TRUE,
                 quiet = TRUE)
{
    doInstall(action = "sync", pkgs = pkgs, ignorePkgs = ignorePkgs, origin = origin, lib.loc = lib.loc,
              allDeps = allDeps, ask = ask, quiet = quiet)
}
