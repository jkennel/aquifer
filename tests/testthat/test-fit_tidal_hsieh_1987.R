test_that(" works", {
  storativity <- 1e-07
  transmissivity <- 1e-05
  radius_well <- 0.05
  frequency <- 1.9322736/86400
  hsieh <- tidal_hsieh_1987(frequency, storativity, transmissivity, radius_well)

  res <- fit_tidal_hsieh_1987(frequency = 1.9322736/86400,
                              Mod(hsieh$response),       # m / nstrain
                              Arg(hsieh$response),       # degrees
                              radius_well,
                              radius_casing = radius_well)
  expect_equivalent(res$par, log10(c(storativity, transmissivity)), tolerance = 0.05)
})
