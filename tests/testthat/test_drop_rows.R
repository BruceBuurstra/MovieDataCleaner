library(MovieDataCleaner)

test_that("Drop rows", {
  df <- data.frame (first  = c(3, 5, 2, NA, NA),
                    second = c(3, 5, 1, 1, NA),
                    third = c(1, 1, 2, NA, NA)
  )
  expect_equal(drop_rows(0.45, df), df <- data.frame (first = c(3, 5, 2),
                                                      second = c(3, 5, 1),
                                                      third = c(1, 1, 2)
  ))
})
