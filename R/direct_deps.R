#' Installation data for \pkg{patRoon} and its direct dependencies
#'
#' Returns information for \pkg{patRoon} and its direct \R package dependencies.
#'
#' The information returned by this function may change over time, and is downloaded from the internet.
#'
#' @return A `list` with miscellaneous package information.
#'
#' @export
getDirectDeps <- function()
{
    # UNDONE: cache this?
    printf("Downloading dependency file\n")
    rdpath <- tempfile(fileext = ".R")
    downloadFile(paste0(patRoonRepos("patRoonDeps"), "/utils/Rdeps.R"), rdpath)
    rdenv <- new.env()
    source(rdpath, local = rdenv)

    if (!rdenv$checkRDepsVersion(getMyRDepsVersion()))
    {
        stop("The installed patRoonInst versions appears out of date. Please update the package. ",
             "See the patRoon handbook for more details.", call. = FALSE)
    }

    return(rdenv$getRDependencies("master", getOS(), withInternal = FALSE, flatten = TRUE))
}
