

#' @title Summary function for summarize.simulation.results
#' 
#' @description Given a set of simulation runs, estimate average impact over range of months.
#' @param res Dataframe of a single series (simulated or otherwise)
#' @param outcomename Name of outcome in res
#' @param months Which months to average over, Default: 1:18
#' @param ... Other parameters (ignored)
#' @return Single number (in this case mean of given months)
#' @importFrom utils data
#' @examples 
#' data( mecklenberg )
#' calculate_average_outcome( mecklenberg, "pbail", months=1:24 )
#' @rdname calculate_average_outcome
#' @export
#' @seealso See \code{\link{aggregate_simulation_results}} for how this function would be used.
#' @examples
#' calculate_average_outcome( mecklenberg, "pbail", months = 1:18 )
calculate_average_outcome = function( res, outcomename,
                                      months = 1:54,
                                      ... ) {
  
  mts = dplyr::filter( res, month %in% months )
  mean( mts[[outcomename]] )
}



#' @title Test a passed test statistic on the simulated data
#' @description This method is used to look at summary statistics such as
#'   average impact post-policy, and see how the predictive distribution
#'   compares to the observed.
#'
#' @param orig.data The raw data (dataframe)
#' @param predictions The results from process_outcome_model.
#' @param outcomename Outcome to use.
#' @param summarizer A function to calculate some summary quantity, Default:
#'   calculate_average_outcome
#' @param ... Extra arguments passed to the summarizer function.
#'
#' @return List of length two, with first item being the observed value of the
#'   test statistic and the second being a numeric vector representing the
#'   emperical reference distribution.
#' @examples
#' predictions = process_outcome_model( "pbail", mecklenberg,
#'                                     t0=0, R = 5,
#'                                     summarize = FALSE, smooth=FALSE )
#' sstat = aggregate_simulation_results( orig.data = mecklenberg, outcomename = "pbail",
#'                                      predictions = predictions, months = 1:18 )
#' sstat$t
#' sstat$t.obs
#'
#' @export
aggregate_simulation_results = function( orig.data, predictions,
                                         outcomename,
                                         summarizer = calculate_average_outcome, ... ) {
  summary = predictions %>%
    tidyr::nest( data = c(month, Ybar, Ystar, Ysmooth) ) %>%
    dplyr::mutate( t = purrr::map( data, summarizer, outcomename = "Ystar", ... ) )  %>%
    tidyr::unnest( t )
  
  sum.obs = summarizer( orig.data, outcomename = outcomename, ... )
  
  list( t.obs = sum.obs, t = summary$t )
}

