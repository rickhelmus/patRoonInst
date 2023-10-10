#' @import methods
NULL

installMain <- setRefClass("installMain")

installMain$methods(
    availablePackages = function(directDeps)
    {
        avail <- getAvailablePackages(repos = BiocManager::repositories())
        avail <- avail[avail$Package %in% names(directDeps), ]
        avail$RemoteSha <- if (nrow(avail) > 0) NA_character_ else character()

        directDepsGH <- directDeps[sapply(directDeps, "[[", "type") == "gh"]
        if (length(directDepsGH) > 0)
        {
            printf("Getting GitHub checksums for %d packages... ", length(directDepsGH))
            availGH <- Map(names(directDepsGH), directDepsGH, f = function(dep, md)
            {
                upSHA <- remotes::remote_sha(remotes::github_remote(getGHRepos(dep, md), ref = getGHRef(md)))

                # NOTE: we don't bother with getting versions here, as for GH packages we solely look at checksums
                return(data.frame(Package = dep, Version = NA_character_, RemoteSha = upSHA))
            })
            availGH <- do.call(rbind, availGH)
            printf("Done!\n")
            # NOTE: GH packages override those from elsewhere
            avail <- rbind(avail[!avail$Package %in% availGH$Package, ], availGH)
        }

        missing <- setdiff(names(directDeps), avail$Package)
        if (length(missing) > 0)
            warning(sprintf("The following packages are not available: %s", paste0(missing, collapse = ", ")), call. = FALSE)

        return(avail)
    },

    install = function(pkgs, directDeps, quiet)
    {
        for (i in seq_len(nrow(pkgs)))
        {
            pkgn <- pkgs$Package[i]
            if (is.na(pkgs$RemoteSha.avail[i]))
            {
                installMsg(pkgn, "CRAN/BioConductor")
                BiocManager::install(pkgn, update = FALSE, ask = FALSE, force = TRUE, quiet = quiet)
            }
            else
            {
                installMsg(pkgn, "GitHub")
                dd <- directDeps[[pkgn]]
                remotes::install_github(getGHRepos(pkgn, dd), ref = getGHRef(dd), subdir = dd[["subdir"]],
                                        upgrade = "never", force = TRUE, quiet = quiet)
            }
        }
    }
)
