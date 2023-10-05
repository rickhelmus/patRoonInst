#' @include install-main.R
NULL

installRepos <- setRefClass("installRepos", contains = c("installMain", "VIRTUAL"),
                            fields = list(repos = "character", reposInfo = "data.frame", reposName = "character"))

installRepos$methods(
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
            installMsg(pkg, reposName)
            utils::install.packages(pkg, repos = .self$repos, type = "binary", quiet = TRUE)
        }

        otherPkgs <- pkgs[!pkgs$Package %in% reposInfo$Package, ]
        if (nrow(otherPkgs) > 0)
            callSuper(pkgs = otherPkgs, ...)
    }
)
