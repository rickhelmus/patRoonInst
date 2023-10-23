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

    # test updating Rgraqphviz: first install to get all deps, then downgrade from GitHub and then update again
    install(pkgs = "BiocStyle", lib.loc = RLib, ask = FALSE)

    # get old version
    remotes::install_github("BioConductor/BiocStyle@d4ee8a4", lib = RLib, upgrade = "never")
    oldver <- packageVersion("BiocStyle", lib.loc = RLib)

    update(pkgs = "BiocStyle", lib.loc = RLib, ask = FALSE)
    expect_true(packageVersion("BiocStyle", lib.loc = RLib) > oldver)
})

# UNDONE: somehow also test sync()? It's already used for patRoon tests on GHA though.
