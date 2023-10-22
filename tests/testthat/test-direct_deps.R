test_that("getDirectDeps() works", {
    dd <- getDirectDeps()
    checkmate::expect_list(dd, min.len = 1, names = "unique", any.missing = FALSE)
    checkmate::expect_names(names(dd), must.include = "patRoon")
    expect_true(all(sapply(dd, function(d) "type" %in% names(d))))
})
