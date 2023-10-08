purge <- function(libPath = NULL, ask = TRUE)
{
    # UNDONE: do we want this function? For now leave it (unexported)

    ac <- checkmate::makeAssertCollection()
    checkmate::assertString(libPath, null.ok = TRUE, min.chars = 1, add = ac)
    checkmate::assertFlag(ask, add = ac)
    checkmate::reportAssertions(ac)

    repInfoPD <- getPDRepInfo()
    directDeps <- getDirectDeps()
    instPackages <- getInstalledPackages(libPath)
    otherPkgs <- setdiff(instPackages$Package, c(repInfoPD$Package, names(directDeps)))
    if (length(otherPkgs) == 0)
        printf("No packages to purge...\n")
    else
    {
        # UNDONE: handle base packages!!!
        printf("The following packages are not part of patRoon and will be REMOVED: %s\n", paste0(otherPkgs, collapse = ", "))
        if (!ask || askProceed())
            utils::remove.packages(otherPkgs, libPath)
    }
}
