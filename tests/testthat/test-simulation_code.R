context("generate_fake_data")

test_that("default", {
  #default vs user provide param
  simData <- generate_fake_data()
  simData2 <- generate_fake_data(t_min = -40, t_max = 9, t0 = 0, rho = 0.5, sd.omega = 1, coef_line = c(20, 0.05),
  			coef_q = c(1, 0, -1, 0), coef_temp = 0.1, coef_sin = c(0, 0), coef_tx = c(0, 0.25, 5))
  expect_equal(simData[3:7], simData2[3:7])
})


test_that("negative", {
  #negative test for parameters
  expect_error(generate_fake_data(tmin = "minTemp"))
  expect_error(generate_fake_data(tmax = "MaxTemp"))
  expect_error(generate_fake_data(t0 = "Zero"))
  expect_error(generate_fake_data(rho = "A"))
  expect_error(generate_fake_data( sd.omega = "sd"))
  expect_error(generate_fake_data(coef_line = c("mx", "c")))
  expect_error(generate_fake_data(coef_q = c(1, 0, -1, "x")))
  expect_error(generate_fake_data(coef_temp = "winter"))
  expect_error(generate_fake_data(coef_sin = c("+infi", "-infi")))
  expect_error(generate_fake_data(coef_tx = c("+infi", "-infi")))
})
