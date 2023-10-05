#' @include install-backend.R
NULL

installPD <- setRefClass("installPD", contains = "installBackend",
                         fields = list(repos = "character", reposInfo = "data.frame"))

installPD$methods(
    initialize = function(..., repos = NULL)
    {
        rep <- if (is.null(repos)) "https://rickhelmus.github.io/patRoonDeps" else repos

        f <- tempfile(fileext = ".tsv")
        downloadFile(paste0(rep, "/patRoonDeps.tsv"), f)
        ri <- read.csv(f, sep = "\t", colClasses = "character")
        ri$RemoteSha[!nzchar(ri$RemoteSha)] <- NA_character_ # normalize with installed.packages

        callSuper(..., repos = rep, reposInfo = ri)
    },

    availablePackages = function(directDeps)
    {
        ret <- reposInfo
        otherPackages <- directDeps[!names(directDeps) %in% ret$Package]
        if (length(otherPackages) > 0)
            ret <- rbind(ret, callSuper(otherPackages))

        return(ret)
    },

    install = function(pkgs, ...)
    {
        pkgsInRepos <- pkgs[pkgs$Package %in% reposInfo$Package, ]

        for (pkg in pkgsInRepos$Package)
        {
            installMsg(pkg, "patRoonDeps")
            # utils::install.packages(pkg, repos = self$repos, type = "binary", quiet = TRUE)
        }

        otherPkgs <- pkgs[!pkgs$Package %in% reposInfo$Package, ]
        if (nrow(otherPkgs) > 0)
            callSuper(otherPkgs, ...)
    }
)
