library(MovieDataCleaner)

test_that("Rounding the ratings", {
  expect_equal(round_to_movie_rating(2.1), 2)
  expect_equal(round_to_movie_rating(2.6), 2.5)
})
