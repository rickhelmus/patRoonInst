#' @include install-repos.R
NULL

installPD <- setRefClass("installPD", contains = "installRepos")

installPD$methods(
    initialize = function(..., repos = NULL)
    {
        rep <- if (is.null(repos)) "https://rickhelmus.github.io/patRoonDeps" else repos

        f <- tempfile(fileext = ".tsv")
        downloadFile(paste0(rep, "/patRoonDeps.tsv"), f)
        ri <- read.csv(f, sep = "\t", colClasses = "character")
        ri$RemoteSha[!nzchar(ri$RemoteSha)] <- NA_character_ # normalize with installed.packages

        callSuper(..., repos = rep, reposInfo = ri, reposName = "patRoonDeps")
    }
)
