printf <- function(...) cat(sprintf(...), sep = "")

# Check without loading namespace, from: https://hohenfeld.is/posts/check-if-a-package-is-installed-in-r/
isInstalled <- function(pkg, lib.loc = NULL) nzchar(system.file(package = pkg, lib.loc = lib.loc))

downloadFile <- function(url, dest)
{
    # increase timeout for large files, thanks to https://stackoverflow.com/a/68944877
    otimeout <- getOption("timeout")
    options(timeout = max(600, otimeout))
    on.exit(options(timeout = otimeout), add = TRUE)

    if (download.file(url, dest, mode = "wb") != 0)
        stop(sprintf("Failed to download from '%s'", url), call. = FALSE)
}

getOS <- function() switch(Sys.info()[["sysname"]], Windows = "windows", Linux = "linux", Darwin = "osx")

getAvailablePackages <- function(...) as.data.frame(available.packages(...))[, c("Package", "Version")]

getGHRepos <- function(dep, md)
{
    repos <- if (!is.null(md[["repos"]])) md$repos else dep
    return(paste0(md$user, "/", repos))
}

getGHRef <- function(md)
{
    if (!is.null(md[["tag"]]))
        md$tag
    else if (!is.null(md[["commit"]]))
        md$commit
    else if (!is.null(md[["branch"]]))
        md$branch
    else
        "master"
}

installMsg <- function(pkg, type) printf("Installing package %s (%s)...\n", pkg, type)
