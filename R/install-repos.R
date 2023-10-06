#' @include install-main.R
NULL

installRepos <- setRefClass("installRepos", contains = c("installMain", "VIRTUAL"),
                            fields = list(reposInfo = "data.frame", reposName = "character", binaryOnly = "logical"))

installRepos$methods(
    initialize = function(binaryOnly = FALSE, ...) callSuper(binaryOnly = binaryOnly, ...),

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

        instArgs <- list(repos = patRoonRepos(reposName), quiet = TRUE)
        if (binaryOnly)
            instArgs <- c(instArgs, type = "binary")

        for (pkg in pkgsInRepos$Package)
        {
            installMsg(pkg, reposName)
            do.call(utils::install.packages, c(list(pkg), instArgs))
        }

        otherPkgs <- pkgs[!pkgs$Package %in% reposInfo$Package, ]
        if (nrow(otherPkgs) > 0)
            callSuper(pkgs = otherPkgs, ...)
    }
)
