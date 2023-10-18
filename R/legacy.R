#' Manage legacy \pkg{patRoon} installations
#'
#' Toggles or removes installations of [patRoon](https://github.com/rickhelmus/patRoon) that were installed by the
#' legacy installation script (`install_patRoon.R`).
#'
#' Previous versions of \pkg{patRoon} (`<2.3`) could be installed _via_ a (now legacy) installation \R script
#' (`install_patRoon.R`). The functions documented here can (temporarily) undo the changes by this script, so that the
#' [installation functions in this package][installing] can be used to manage \pkg{patRoon} installations.
#'
#' The `toggleLegacy()` function is used to enable or disable legacy installations temporarily.
#' 
#' The `removeLegacy()` function is used to undo legacy installations.
#'
#' @section Background: This section is purely informative, but may be of use when you want to manually manage legacy
#'   installations.
#'   
#'   The legacy installation script typically results in the following changes:
#'   
#'   1. `~/patRoon-install`: a directory with external dependencies (`MetFrag`, `SIRIUS`, ...) and may contain \R packages from the `patRoonDeps` repository depending on user input during the installation.
#'   2. `~/.Rprofile-patRoon.R`: An \R script that should be loaded during \R startup, and initializes the configuration needed to use the files (1).
#'   3. Code in `~/.Rprofile` to actually load (2) on \R startup (only if it exists)
#'   
#'   When `toggleLegacy()` is used to disable legacy installations the function simply renames the initialization script
#'   (2) so it won't be loaded on \R startup. The `removeLegacy()` function removes the files from (1-2) and optionally
#'   the changes from (3).
#'
#' @note The \R session should be restarted to make the changes effective.
#'
#' @return All functions return `NULL` invisibly.
#'
#' @name legacy
NULL

getLegacyDataDir <- function() "~/patRoon-install"
getLegacyInitScript <- function(dis = FALSE) if (!dis) "~/.Rprofile-patRoon.R" else "~/.Rprofile-patRoon.R-disabled"
printLegDone <- function() printf("Done! Please restart R to make the changes effective.\n")

inspectLegacyInstall <- function()
{
    legacyLoaded <- getOption("patRoon.Rprof", default = FALSE)
    dataDirExists <- dir.exists(getLegacyDataDir())
    RprofPatExists <- file.exists(getLegacyInitScript())
    RprofPatDisExists <- file.exists(getLegacyInitScript(dis = TRUE))
    RprofExists <- file.exists("~/.Rprofile")

    printf("Legacy installation currently loaded: %s\n", legacyLoaded)
    printf("Legacy data directory (~/patRoon-install) exists %s\n", dataDirExists)
    printf("Legacy init script (~/.Rprofile-patRoon.R) exists: %s\n", RprofPatExists)
    printf("Legacy init script was disabled: %s\n", RprofPatDisExists)
    printf("User Rprofile file exists: %s\n", RprofExists)

    if (RprofPatExists && RprofPatDisExists)
    {
        printf(paste0("NOTE: the legacy init script seems to be present, while there also seems to be a disabled init script. ",
                      "Did you re-install patRoon with the legacy installation script?\n"))
    }
    if ((!dataDirExists && (RprofPatExists || !RprofPatDisExists)) ||
        (dataDirExists && !RprofPatExists && !RprofPatDisExists))
        printf("NOTE: A legacy installation was detected, but seems partial and not all files could be detected.\n")

    return(list(legacyLoaded = legacyLoaded, dataDirExists = dataDirExists, RprofPatExists = RprofPatExists,
                RprofPatDisExists = RprofPatDisExists, RprofExists = RprofExists))
}

#' @param enable Enables (`enable=TRUE`), disables (`enable=FALSE`) or toggles (`enable=NULL`) a legacy \pkg{patRoon}
#'   installation.
#' @export
#' @rdname legacy
toggleLegacy <- function(enable = NULL)
{
    insp <- inspectLegacyInstall()
    
    if (is.null(enable))
    {
        if (insp$RprofPatExists && insp$RprofPatDisExists)
            stop("Cannot detect if legacy installation should be enabled or disabled, please set the enable argument to TRUE/FALSE", call. = FALSE)
        enable <- insp$RprofPatDisExists
    }
    
    src <- dest < - NULL
    
    if (enable)
    {
        if (!insp$RprofPatDisExists)
            stop("Cannot locate backup init script", call. = FALSE)
        src <- getLegacyInitScript(dis = TRUE); dest <- getLegacyInitScript()
    }
    else
    {
        if (!insp$RprofPatExists)
            stop("Cannot locate init script", call. = FALSE)
        src <- getLegacyInitScript(); dest <- getLegacyInitScript(dis = TRUE)
    }
    
    printf("The init script will be %s by renaming %s to %s.\n", if (enable) "enabled" else "disabled", src, dest)

    if (askProceed() && !file.rename(src, dest))
        stop("Failed to rename file!", call. = FALSE)
    
    printLegDone()
    
    invisible(NULL)
}

#' @param restoreRProfile If `TRUE` then the modifications to the user's `.Rprofile` file will be removed that were
#'   automatically performed by the legacy installation script.
#' @export
#' @rdname legacy
removeLegacy <- function(restoreRProfile = FALSE)
{
    insp <- inspectLegacyInstall()

    rmPaths <- character()
    if (insp$RprofPatExists)
        rmPaths <- getLegacyInitScript()
    if (insp$RprofPatDisExists)
        rmPaths <- c(rmPaths, getLegacyInitScript(dis = TRUE))
    if (insp$dataDirExists)
        rmPaths <- c(rmPaths, getLegacyDataDir())
    
    if (length(rmPaths) == 0 && (!restoreRProfile || !insp$RprofExists))
    {
        printf("Nothing needs to be done ...\n")
        return(invisible(NULL))
    }

    if (restoreRProfile)
    {
        npp <- normalizePath("~/.Rprofile", mustWork = FALSE)
        if (!file.exists(npp))
            printf("File %s file found, no need to restore Rprofile\n", npp)
        else
        {
            nip <- normalizePath(getLegacyInitScript(), mustWork = FALSE, winslash = "/")
            
            # text used by legacy installation script
            rpLines <- c("# Automatically added by install_patRoon script on ", # note partial, comment ends with date
                         sprintf("if (file.exists('%s'))", nip),
                         sprintf("    source('%s')", nip))
            pl <- readLines(npp)
            matches <- unlist(lapply(rpLines, grep, pl, fixed = TRUE))
            
            # make sure that the sequence is found exactly once and in the right order
            if (length(matches) == length(rpLines) && !is.unsorted(matches))
            {
                writeLines(pl[-matches], npp)
                printf("Restored Rprofile\n")
            }
            else
                printf("No need to restore ~/.Rprofile\n")
        }
    }
    
    printf("The legacy installation will be removed by deleting the following files and directories: %s\n",
           paste0(rmPaths, collapse = ", "))
    
    if (askProceed() && !all(unlink(rmPaths, recursive = TRUE)))
        stop("Error while deleting files!", call. = FALSE)
    
    printLegDone()
    
    invisible(NULL)
}
