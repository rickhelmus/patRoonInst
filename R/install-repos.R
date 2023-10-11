#' @include install-main.R
NULL

installRepos <- setRefClass("installRepos", contains = c("installMain", "VIRTUAL"),
                            fields = list(reposInfo = "data.frame", reposName = "character",
                                          reposIsExclusive = "logical", binaryOnly = "logical"))

installRepos$methods(
    initialize = function(reposIsExclusive, binaryOnly = FALSE, ...)
    {
        callSuper(reposIsExclusive = reposIsExclusive, binaryOnly = binaryOnly, ...)
    },

    packageVersions = function(directDeps)
    {
        ret <- reposInfo
        otherPackages <- directDeps[!names(directDeps) %in% ret$Package]
        if (length(otherPackages) > 0)
            ret <- rbind(ret, callSuper(otherPackages))
        return(ret)
    },

    availablePackages = function()
    {
        myAvail <- if (binaryOnly)
            getAvailablePackages(repos = patRoonRepos(reposName), type = "binary")
        else
            getAvailablePackages(repos = patRoonRepos(reposName))
        if (reposIsExclusive)
            return(myAvail)

        otherAvail <- callSuper()
        otherAvail <- otherAvail[!otherAvail$Package %in% myAvail$Package, ]
        return(rbind(myAvail, otherAvail))
    },

    install = function(pkgs, directDeps, quiet)
    {
        pkgsInRepos <- pkgs[pkgs$Package %in% reposInfo$Package, ]

        instFunc <- if (reposIsExclusive)
            utils::install.packages
        else
            BiocManager::install # use BiocManager, so deps from BioC (and CRAN) can also be installed

        instArgs <- list(quiet = quiet)
        if (reposIsExclusive)
            instArgs$repos <- patRoonRepos(reposName)
        else
        {
            instArgs <- c(instArgs, list(site_repository = patRoonRepos(reposName), update = FALSE, ask = FALSE,
                                         force = TRUE))
        }

        if (binaryOnly)
            instArgs <- c(instArgs, list(type = "binary"))

        for (pkg in pkgsInRepos$Package)
        {
            installMsg(pkg, reposName)
            do.call(instFunc, c(list(pkg), instArgs))
        }

        otherPkgs <- pkgs[!pkgs$Package %in% reposInfo$Package, ]
        if (nrow(otherPkgs) > 0)
            callSuper(pkgs = otherPkgs, directDeps = directDeps, quiet = quiet)
    }
)
