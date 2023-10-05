#' @include install-PD.R
NULL

doInstall <- function(action, libPaths, allDeps, pkgs, origin, repos, reposPD = "https://rickhelmus.github.io/patRoonDeps",
                      instDE = FALSE)
{
    # Check compatibility
    # Get Rdeps.R
    # Initialize backend
    # Get installed packages
    # Compare with origin and decide with action which packages should be installed

    lp <- NULL
    if (!is.null(libPaths))
    {
        lp <- .libPaths()
        on.exit(.libPaths(lp), add = TRUE)
        .libPaths(libPaths, include.site = FALSE)
    }

    # UNDONE: cache this?
    printf("Downloading dependency file\n")
    rdpath <- tempfile(fileext = ".R")
    downloadFile(paste0(reposPD, "/utils/Rdeps.R"), rdpath)
    rdenv <- new.env()
    source(rdpath, local = rdenv)
    directDeps <- rdenv$getRDependencies("master", getOS(), withInternal = FALSE, flatten = TRUE)
    if (!instDE)
        directDeps <- directDeps[!names(directDeps) %in% c("patRoonData", "patRoonExt", "MetaCleanData")] # UNDONE: keep MetacleanData?

    instPackages <- installed.packages(fields = "RemoteSha")[, c("Package", "Version", "RemoteSha")]
    instPackages <- as.data.frame(instPackages)

    backend <- switch(origin,
                      patRoonDeps = installPD$new(repos = repos),
                      runiverse = installRU$new(repos = repos))
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
