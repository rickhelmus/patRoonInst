# old patRoon install script leaves the following around:
# 1 ~/patRoon-install: directory with all R packages from patRoonDeps and external tools (MetFrag, SIRIUS, ...)
# 2 ~/.Rprofile-patRoon.R: R script that sets up options/paths from (1); sets option patRoon.Rprof=TRUE
# 3 Code in ~/.Rpfrofile to source (2) (only if it exists)
#
# For toggling, it is enough to simply rename (2).
# For a full cleanup we remove (1), (2) and remove the code of (3)

inspectLegacyInstall <- function()
{
    legacyLoaded <- getOption("patRoon.Rprof", default = FALSE)
    dataDirExists <- dir.exists("~/patRoon-install")
    RprofPatExists <- file.exists("~/.Rprofile-patRoon.R")
    RprofPatDisExists <- file.exists("~/.Rprofile-patRoon.R-disabled")
    RprofExists <- file.exists("~/.Rprofile-patRoon.R")

    printf("Legacy installation currently loaded: %s\n", legacyLoaded)
    printf("Legacy data directory (~/patRoon-install) exists %s\n", dataDirExists)
    printf("Legacy init script (~/.Rprofile-patRoon.R) exists:", RprofPatExists)
    printf("Legacy init script was disabled: %s\n", RprofPatDisExists)
    printf("User Rprofile file exists: %s\n", RprofExists)

    if (RprofPatExists && RprofPatDisExists)
    {
        printf(paste0("NOTE: the legacy init script seems to be present but also disable, ",
                      "did you re-install patRoon with the legacy installation script?\n"))
    }

    return(list(legacyLoaded = legacyLoaded, dataDirExists = dataDirExists, RprofPatExists = RprofPatExists,
                RprofPatDisExists = RprofPatDisExists, RprofExists = RprofExists))
}

toggleLegacy <- function()
{

}

removeLegacy <- function()
{


}
