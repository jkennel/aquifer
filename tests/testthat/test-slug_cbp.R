test_that("slug_cpb works", {
  time <- 5.776 * 10^(seq(-3, 2, 1))
  expect_equal(slug_cbp(time = time,
                                   n = 12,
                                   radius_casing = 7.6,
                                   radius_screen = 7.6,
                                   radius = 7.6,
                                   storativity = 1e-2,
                                   transmissivity = 10.0,
                                   head_0 = 1.0),
               c(0.9920, 0.9693, 0.8655, 0.4598, 0.03780, 0.002618),
               tolerance = 5e-5)



})
