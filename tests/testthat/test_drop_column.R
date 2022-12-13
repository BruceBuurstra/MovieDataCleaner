library(MovieDataCleaner)

test_that("Drop columns", {
  df <- data.frame (first  = c(3, NA, 3, 5, NA, 2, 1),
                    second = c(NA, NA, 3, 5, NA, 2, NA),
                    third = c(3, NA, 3, 5, 1, 2, 1)
  )
  expect_equal(drop_columns(0.5, df), df <- data.frame (first  = c(3, NA, 3, 5, NA, 2, 1),
                                                        third = c(3, NA, 3, 5, 1, 2, 1)
  ))
})
