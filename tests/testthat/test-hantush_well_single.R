context("hantush_well_single")

#hantush table values from Veling 2010
test_that("hantush_well_single works", {
  expect_equal(hantush_well_single(0,0,100), NaN)
  expect_equal(hantush_well_single(1e-6,0,100), 13.2383, tolerance = 1e-4)
  expect_equal(hantush_well_single(8.0,0,100), 0.0, tolerance = 1e-4)
  expect_equal(hantush_well_single(0.1,0,100), 1.8229, tolerance = 1e-4)
  expect_equal(hantush_well_single(0.005,(0.02/2)^2,100), 4.7068, tolerance = 1e-4)
  expect_equal(hantush_well_single(0, 1, 100), NaN)
})
