
context("test-general-tests")


test_that("The four types of smooth vs summarize calls work", {

  data( "mecklenberg")
  t0 = 0
  preds = process_outcome_model( "pbail", mecklenberg,
                                       t0=t0, R = 10,
                                       summarize = FALSE, smooth=FALSE )
  names( preds )
  expect_true( !all( is.na( preds$Ystar ) ) )
  expect_true( all( is.na( preds$Ysmooth ) ) )
  expect_true( nrow( preds ) == nrow( mecklenberg ) * 10 )

  preds = process_outcome_model( "pbail", mecklenberg,
                                 t0=t0, R = 10,
                                 summarize = TRUE, smooth=FALSE )
  expect_true( !all( is.na( preds$Ystar ) ) )
  expect_true( all( is.na( preds$Ysmooth ) ) )
  expect_true( nrow( preds ) == nrow( mecklenberg ) )


  preds = process_outcome_model( "pbail", mecklenberg,
                                 t0=t0, R = 10,
                                 summarize = FALSE, smooth=TRUE,
                                 post.only = FALSE )
  expect_true( !all( is.na( preds$Ystar ) ) )
  #expect_true( all( !is.na( preds$Ysmooth ) ) )
  expect_true( nrow( preds ) == 10 * nrow( mecklenberg ) )


  preds = process_outcome_model( "pbail", mecklenberg,
                                 t0=t0, R = 10,
                                 summarize = TRUE, smooth=TRUE, post.only = FALSE )
  expect_true( !all( is.na( preds$Ystar ) ) )
  #expect_true( all( !is.na( preds$Ysmooth ) ) )
  expect_true( nrow( preds ) == nrow( mecklenberg ) )

})


test_that("Smoothing with post.only=TRUE gives post smoothed only", {

  data( "mecklenberg")
  t0 = 0

  preds = process_outcome_model( "pbail", mecklenberg,
                                 t0=t0, R = 10,
                                 summarize = FALSE, smooth=TRUE,
                                 post.only = TRUE )
  expect_true( !all( is.na( preds$Ystar ) ) )
  expect_true( all( is.na( preds$Ysmooth[preds$month<=t0] ) ) )
  expect_true( all( !is.na( preds$Ysmooth[preds$month>t0] ) ) )
  expect_true( nrow( preds ) == 10 * nrow( mecklenberg ) )


  preds = process_outcome_model( "pbail", mecklenberg,
                                 t0=t0, R = 10,
                                 summarize = TRUE, smooth=TRUE, post.only = TRUE )
  expect_true( !all( is.na( preds$Ystar ) ) )
  expect_true( all( is.na( preds$Ysmooth[preds$month<=t0] ) ) )
  expect_true( all( !is.na( preds$Ysmooth[preds$month>t0] ) ) )
  expect_true( nrow( preds ) == nrow( mecklenberg ) )

})





