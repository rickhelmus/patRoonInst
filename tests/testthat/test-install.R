initTestRLib <- function() tempfile()

test_that("Installation works", {
    RLib <- initTestRLib()
    install(pkgs = "splashR", lib.loc = RLib, ask = FALSE)
    checkmate::expect_directory_exists(file.path(RLib, "splashR"))
})

test_that("Ignoring packages", {
    RLib <- initTestRLib()
    install(pkgs = c("splashR", "patRoonData"), ignorePkgs = "big", lib.loc = RLib, ask = FALSE)
    checkmate::expect_directory_exists(file.path(RLib, "splashR"))
    expect_false(dir.exists(file.path(RLib, "patRoonData")))
})

test_that("Updating packages", {
    RLib <- initTestRLib()
    # dir.create(RLib, recursive = TRUE) # so that remotes doesn't ask to make it
    # easiest is to first install it, so all deps are pulled in correctly too
    install(pkgs = "CAMERA", lib.loc = RLib, ask = FALSE)

    # get old version
    remotes::install_github("sneumann/CAMERA@ac16892", lib = RLib, upgrade = "never")
    oldver <- packageVersion("CAMERA", lib.loc = RLib)

    update(pkgs = "CAMERA", lib.loc = RLib, ask = FALSE)
    expect_true(packageVersion("CAMERA", lib.loc = RLib) > oldver)
})

# UNDONE: somehow also test sync()? It's already used for patRoon tests on GHA though.
