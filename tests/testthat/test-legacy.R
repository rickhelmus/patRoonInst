withr::defer(options(legScriptStarted = NULL))

getTestLegDir <- function()
{
    ret <- tempfile()
    dir.create(ret, recursive = TRUE)
    withr::local_options(list(patRoonInst.path.legacy = ret)) # so we can use path utility functions below

    dir.create(getLegacyDataDir())
    cat("", file = file.path(getLegacyDataDir(), "somedep"))
    lsc <- normalizePath(getLegacyInitScript(), winslash = "/", mustWork = FALSE)
    cat("options(legScriptStarted = TRUE)\n", file = lsc)

    rpLines <- c(paste("# Automatically added by install_patRoon script on ", date()),
                 sprintf("if (file.exists('%s'))", lsc),
                 sprintf("    source('%s')", lsc))
    writeLines(rpLines, getRProfile())

    return(ret)
}

test_that("Toggling legacy installations work", {
    legDir <- getTestLegDir()
    withr::local_options(list(patRoonInst.path.legacy = legDir))

    toggleLegacy(ask = FALSE) # toggle off
    checkmate::expect_file_exists(getLegacyInitScript(dis = TRUE))
    expect_false(checkmate::test_file_exists(getLegacyInitScript(dis = FALSE)))

    toggleLegacy(ask = FALSE) # toggle on
    checkmate::expect_file_exists(getLegacyInitScript(dis = FALSE))
    expect_false(checkmate::test_file_exists(getLegacyInitScript(dis = TRUE)))

    toggleLegacy(enable = TRUE, ask = FALSE) # on again, nothing should change
    checkmate::expect_file_exists(getLegacyInitScript(dis = FALSE))
    expect_false(checkmate::test_file_exists(getLegacyInitScript(dis = TRUE)))
})

checkRProf <- function()
{
    options(legScriptStarted = NULL)
    source(getRProfile())
    return(getOption("legScriptStarted", FALSE))
}

test_that("Removing legacy installations work", {
    legDir <- getTestLegDir()
    withr::local_options(list(patRoonInst.path.legacy = legDir))

    # make sure things work
    expect_true(checkRProf())

    removeLegacy(restoreRProfile = TRUE, ask = FALSE)
    expect_false(checkmate::test_file_exists(getLegacyDataDir()))
    expect_false(checkmate::test_file_exists(getLegacyInitScript()))
    checkmate::expect_file_exists(getRProfile())
    expect_false(checkRProf())
    expect_equal(file.size(getRProfile()), 0)

    legDir <- getTestLegDir()
    withr::local_options(list(patRoonInst.path.legacy = legDir))
    RprofHash <- tools::md5sum(getRProfile())
    removeLegacy(restoreRProfile = FALSE, ask = FALSE)
    expect_false(checkmate::test_file_exists(getLegacyDataDir()))
    expect_false(checkmate::test_file_exists(getLegacyInitScript()))
    checkmate::expect_file_exists(getRProfile())
    expect_false(checkRProf())
    expect_equal(tools::md5sum(getRProfile()), RprofHash)
})
