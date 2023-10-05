#' @include install-main.R
NULL

installRU <- setRefClass("installRU", contains = "installMain",
                         fields = list(repos = "character", reposInfo = "data.frame"))

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

        callSuper(..., repos = rep, reposInfo = ri)
    },

    availablePackages = function(directDeps)
    {
        ret <- reposInfo

        # in case we add other packages to the repos that are not relevant here
        ret <- ret[ret$Package %in% names(directDeps), ]

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
            installMsg(pkg, "r-universe")
            utils::install.packages(pkg, repos = self$repos, quiet = TRUE)
        }

        otherPkgs <- pkgs[!pkgs$Package %in% reposInfo$Package, ]
        if (nrow(otherPkgs) > 0)
            callSuper(pkgs = otherPkgs, ...)
    }
)
