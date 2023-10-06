#' @include install-PD.R
NULL

doInstall <- function(action, pkgs, ignorePkgs, origin, libPaths, allDeps)
{
    # UNDONE: clean option to be used with sync
    # UNDONE: check args (checkmate?)
    # UNDONE: doc "big" option for ignorePkgs
    # UNDONE: doc that allDeps doesn't work with pkgs/ignorePkgs (also verify args for this)
    # UNDONE: check compatibility
    # UNDONE: optionally ask before proceeding? (enabled by default)
    # UNDONE: force argument?

    lp <- NULL
    if (!is.null(libPaths))
    {
        # NOTE: .libPaths() is set here to prevent install.packages() (and derived functions) from looking for
        # dependencies outside the target library. Unfortunately, this won't remove the default library (ie .Library)
        # from the search path, but this library is often static
        lp <- .libPaths()
        on.exit(.libPaths(lp), add = TRUE)
        .libPaths(libPaths, include.site = FALSE)
    }

    # UNDONE: cache this?
    printf("Downloading dependency file\n")
    rdpath <- tempfile(fileext = ".R")
    downloadFile(paste0(patRoonRepos("patRoonDeps"), "/utils/Rdeps.R"), rdpath)
    rdenv <- new.env()
    source(rdpath, local = rdenv)
    directDeps <- rdenv$getRDependencies("master", getOS(), withInternal = FALSE, flatten = TRUE)

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
    instPackages <- installed.packages(lib.loc = libPaths, fields = "RemoteSha")[, c("Package", "Version", "RemoteSha")]
    instPackages <- as.data.frame(instPackages)

    backend <- switch(origin,
                      patRoonDeps = installPD$new(),
                      runiverse = installRU$new(),
                      regular = installMain$new())
    availPackages <- backend$availablePackages(directDeps)

    # set rownames to simplify things
    rownames(instPackages) <- instPackages$Package; rownames(availPackages) <- availPackages$Package

    considerPackages <- if (allDeps) # UNDONE: limit to patRoonDeps?
        merge(instPackages, availPackages, by = "Package", all.y = TRUE, suffix = c(".inst", ".avail"))
    else
    {
        considerPackages <- data.frame(Package = names(directDeps))
        considerPackages <- merge(considerPackages, instPackages, by = "Package", all.x = TRUE)
        considerPackages <- merge(considerPackages, availPackages, by = "Package", all.x = TRUE, all.y = allDeps, suffix = c(".inst", ".avail"))
    }

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

    considerPackages <- considerPackages[!is.na(considerPackages$action), ]

    printActions <- function(what, pkgAction)
    {
        whpkgs <- considerPackages[considerPackages$action == pkgAction, "Package"]
        if (length(whpkgs) > 0)
            printf("The following %d packages will be %s: %s\n", length(whpkgs), what, paste0(whpkgs, collapse = ", "))
    }

    printActions("installed", "install")
    printActions("updated", "update")
    printActions("synchronized", "sync")

    backend$install(considerPackages, directDeps)
}

#' @export
install <- function(lib.loc = NULL, pkgs = "all", origin = NULL, reposPD = "https://rickhelmus.github.io/patRoonDeps")
{
}
