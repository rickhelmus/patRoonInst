printf <- function(...) cat(sprintf(...), sep = "")

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

askProceed = function() !interactive() || menu(c("Yes", "No"), title = "Do you want to proceed?") == 1

installMsg <- function(pkg, type) printf("Installing package %s (%s)...\n", pkg, type)

patRoonRepos <- function(which)
{
    opt <- paste0(packageName(), ".repos.", if (which == "r-universe") "runiverse" else which)
    ret <- getOption(opt)
    if (is.null(ret))
        stop(sprintf("Cannot use %s repos: the '%s' option is unset", which, opt), call. = FALSE)
    return(ret)
}
