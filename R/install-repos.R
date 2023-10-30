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

    packageVersions = function(directDeps, ignorePkgs, getAll)
    {
        ret <- if (getAll)
            reposInfo
        else
            reposInfo[reposInfo$Package %in% names(directDeps), ]
        ret <- ret[!ret$Package %in% ignorePkgs, ]

        # get deps from other repos
        # NOTE: parent methods also check ignorePkgs
        otherDDeps <- directDeps[!names(directDeps) %in% ret$Package]
        if (length(otherDDeps) > 0)
            ret <- rbind(ret, callSuper(otherDDeps, ignorePkgs))

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
        instArgs <- list(quiet = quiet)
        if (binaryOnly)
            instArgs <- c(instArgs, list(type = "binary"))
        
        pkgsInRepos <- pkgs[pkgs$Package %in% reposInfo$Package, ]
        repos <- patRoonRepos(reposName)
        
        for (pkg in pkgsInRepos$Package)
        {
            installMsg(pkg, reposName)
            if (!reposIsExclusive)
            {
                # first install deps: then we can only use our repo while installing the package, which avoids
                # installing newer versions from other repos
                pd <- remotes::package_deps(pkg, repos = repos)
                pd <- pd[is.na(pd$installed) & pd$package != pkg, ]
                if (nrow(pd) > 0)
                {
                    do.call(utils::install.packages, modifyList(instArgs, list(pkgs = pd$package,
                                                                               #NOTE: add CRAN/BioC repos so all deps can be found
                                                                               repos = c(repos, BiocManager::repositories()))))
                }
            }
            do.call(utils::install.packages, c(list(pkg, repos = repos), instArgs))
        }
        
        otherPkgs <- pkgs[!pkgs$Package %in% reposInfo$Package, ]
        if (nrow(otherPkgs) > 0)
            callSuper(pkgs = otherPkgs, directDeps = directDeps, quiet = quiet)
    }
)
