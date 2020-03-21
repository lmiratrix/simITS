context("make.fake.data")

test_that("default", {
  #default vs user provide param
  simData <- make.fake.data()
  simData2 <- make.fake.data(t.min = -40, t.max = 9, t0 = 0, rho = 0.5, sd.omega = 1, coef.line = c(20, 0.05),
  			coef.q = c(1, 0, -1, 0), coef.temp = 0.1, coef.sin = c(0, 0), coef.tx = c(0, 0.25, 5))
  expect_equal(simData[3:7], simData2[3:7])
})


test_that("negative", {
  #negative test for parameters
  expect_error(make.fake.data(tmin = "minTemp"))
  expect_error(make.fake.data(tmax = "MaxTemp"))
  expect_error(make.fake.data(t0 = "Zero"))
  expect_error(make.fake.data(rho = "A"))
  expect_error(make.fake.data( sd.omega = "sd"))
  expect_error(make.fake.data(coef.line = c("mx", "c")))
  expect_error(make.fake.data(coef.q = c(1, 0, -1, "x")))
  expect_error(make.fake.data(coef.temp = "winter"))
  expect_error(make.fake.data(coef.sin = c("+infi", "-infi")))
  expect_error(make.fake.data(coef.tx = c("+infi", "-infi")))
})
