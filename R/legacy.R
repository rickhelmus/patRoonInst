# old patRoon install script leaves the following around:
# 1 ~/patRoon-install: directory with all R packages from patRoonDeps and external tools (MetFrag, SIRIUS, ...)
# 2 ~/.Rprofile-patRoon.R: R script that sets up options/paths from (1); sets option patRoon.Rprof=TRUE
# 3 Code in ~/.Rpfrofile to source (2) (only if it exists)
#
# For toggling, it is enough to simply rename (2).
# For a full cleanup we remove (1), (2) and remove the code of (3)

getLegacyDataDir <- function() "~/patRoon-install"
getLegacyInitScript <- function(dis = FALSE) if (!dis) "~/.Rprofile-patRoon.R" else "~/.Rprofile-patRoon.R-disabled"

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
    
    invisible(NULL)
}

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
    
    printf("The legacy installed will be removed by deleting the following files and directories: %s\n",
           paste0(rmPaths, collapse = ", "))
    
    if (askProceed() && !all(unlink(rmPaths, recursive = TRUE)))
        stop("Error while deleting files!", call. = FALSE)
}
