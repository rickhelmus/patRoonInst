#' @include install-repos.R
NULL

installPD <- setRefClass("installPD", contains = "installRepos")

installPD$methods(
    initialize = function(...)
    {
        suppressWarnings({
            if (nrow(available.packages(repos = patRoonRepos("patRoonDeps"), type = "binary")) == 0)
                stop("patRoonDeps is not supported for this Operating System and/or R version", call. = FALSE)
        })
        callSuper(..., reposInfo = getPDRepInfo(), reposName = "patRoonDeps", binaryOnly = TRUE)
    }
)
