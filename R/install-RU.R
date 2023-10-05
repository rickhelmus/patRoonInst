#' @include install-repos.R
NULL

installRU <- setRefClass("installRU", contains = "installRepos")

installRU$methods(
    initialize = function(..., repos = NULL)
    {
        rep = if (is.null(repos)) "https://rickhelmus.r-universe.dev" else repos

        f <- tempfile(fileext = ".json")
        downloadFile(paste0(rep, "/api/packages/"), f)
        packagesJS <- jsonlite::read_json(f)

        # convert to simple data.frame
        riList <- lapply(packagesJS, function(pkg)
        {
            if (pkg[["_type"]] == "failure")
                return(NULL)
            ret <- pkg[c("Package", "Version")]
            ret$RemoteSha <- if (!is.null(pkg[["RemoteSha"]])) pkg$RemoteSha else NA_character_
            return(ret)
        })
        ri <- do.call(rbind.data.frame, riList)

        callSuper(..., repos = rep, reposInfo = ri, reposName = "r-universe")
    }
)
