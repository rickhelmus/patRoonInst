#' @include install-repos.R
NULL

installPD <- setRefClass("installPD", contains = "installRepos")

installPD$methods(
    initialize = function(...)
    {
        f <- tempfile(fileext = ".tsv")
        downloadFile(paste0(rep, "/patRoonDeps.tsv"), f)
        ri <- read.csv(f, sep = "\t", colClasses = "character")
        ri$RemoteSha[!nzchar(ri$RemoteSha)] <- NA_character_ # normalize with installed.packages

        callSuper(..., reposInfo = ri, reposName = "patRoonDeps", binaryOnly = TRUE)
    }
)
