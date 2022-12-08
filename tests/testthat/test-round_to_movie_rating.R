context("round_to_movie_rating")

testthat::test_that("equals", {
  testthat::expect_equal(
    round_to_movie_rating(2.1),
    2
  )

  testthat::expect_equal(
    round_to_movie_rating(2.6),
    2.5
  )
})
