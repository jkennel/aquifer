context("theis_u_time")

## TODO: Rename context
## TODO: Add more tests

test_that("multiplication works", {
  expect_equal(theis_u_time(10,1e-5,1e-4,1), (10^2 * 1e-5)/(4*1e-4*1))
  expect_equal(theis_u_time(10,1e-5,1e-4,1:2), c((10^2 * 1e-5)/(4*1e-4*1),(10^2 * 1e-5)/(4*1e-4*2)))
  expect_equal(theis_u_time(10,1e-5,1e-4,0), Inf)
  expect_equal(theis_u_time(0,1e-5,1e-4,0), NaN)
})


