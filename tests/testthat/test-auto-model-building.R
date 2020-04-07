# Testing the seasonality model building stuff

context("test auto model building")

test_that("The four smoothing calls work", {
  t0 = 0
  dat = generate_fake_data( t_min=-40, t_max=15, t0 = t0)
  nrow( dat )
  head( dat )

  fit.season.model = make_fit_season_model( ~ Q2 + Q3 + Q4 )
  expect_true( !is.null( attr( fit.season.model, "lags" ) ) )

  dat1 = add_lagged_covariates( dat, "Y", covariates = c("Q2","Q3","Q4") )
  head( dat1 )

  dat2 = add_lagged_covariates( dat, "Y", covariates =  fit.season.model )
  head( dat2 )

  expect_equal( dat1, dat2 )

  dat.pre = filter( dat1, month <= t0 )

  M0 = fit.season.model( dat.pre, "Y" )
  expect_equal( length( coef( M0 ) ), 9 )

  M0 = fit.season.model( dat.pre, "Y", lagless = TRUE )
  expect_equal( length( coef( M0 ) ), 5 )

})






test_that( "Making formula with and without lags works", {

  fm = make_fit_season_model( ~ A + B )
  attr( fm, "formula" )
  attr( fm, "lag.formula" )

  fm = make_fit_season_model( ~ A, ~ B )
  attr( fm, "formula" )
  attr( fm, "lag.formula" )
  expect_equal( attr( fm, "formula" ), ~ A + B )
  expect_equal( attr( fm, "lag.formula" ), ~ A + B + lag.outcome + lag.A )

  fm = make_fit_season_model( ~ 1, ~ B )
  attr( fm, "formula" )
  attr( fm, "lag.formula" )
  expect_true( !is.null( attr( fm, "formula" ) ) )
  expect_true( !is.null( attr( fm, "lag.formula" ) ) )
  expect_equal( attr( fm, "lag.formula" ), ~ B + lag.outcome )

  # No dot on left hand side!
  expect_error( fm = make_fit_season_model( . ~ A, ~ B ) )

} )
