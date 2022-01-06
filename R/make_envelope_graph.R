


#' Make envelope style graph with associated smoothed trendlines
#'
#' This method builds a ggplot object with the trendline and prediction
#' envelope. It can be customized after the fact by adding more ggplot layers
#' via normal ggplot "+" syntax.
#'
#' @param envelope The result of a `process_outcome_model()` call, i.e.
#'   dataframe with columns of original data, imputed data and, potentially,
#'   smoothed data.
#' @param t0  Last pre-policy timepoint. Will draw vertical line here.
#' @param ylab Y label of plot
#' @param xlab X label of plot
#' @example examples/example_process_outcome_model.R
#' @importFrom rlang .data
#' @export
#' @return Returns (does not yet display) a ggplot plot object containing the
#'   time series along with extrapolation and prediction envelope. This plot can
#'   be augmented and changed via standard ggplot commands.
#' @seealso The \code{ggplot2} package.
#' @example examples/example_process_outcome_model.R
make_envelope_graph = function( envelope, t0, ylab = "Y", xlab="month" ) {
  
  has.smooth = ( "Ysmooth" %in% names(envelope) ) && (any( !is.na( envelope$Ysmooth ) ) )
  
  # get last pre-policy timepoint
  Y.init = dplyr::filter( envelope, month == t0 )$Y
  
  #ft = dplyr::filter( envelope, month == t0+1 )
  #ft$month = ft$month - 0.5
  #ft$Ysmooth = ft$Ysmooth1 = ft$Y = NA
  #envelope = dplyr::bind_rows( envelope, ft )
  t0mo = which( envelope$month == t0 )
  envelope$Ysmooth[t0mo] =  envelope$Ymin[t0mo] =  envelope$Ymax[t0mo] =  envelope$Y[t0mo]
  
  plt = ggplot2::ggplot( envelope, ggplot2::aes( .data$month ) ) +
    ggplot2::geom_line( ggplot2::aes( y= .data$Y ), alpha = 0.6 ) + 
    ggplot2::geom_point( ggplot2::aes( y=.data$Y ) ) +
    ggplot2::geom_vline( xintercept=t0, col="red" ) +
    ggplot2::geom_point( x=t0, y=Y.init, col="red" ) +
    ggplot2::geom_ribbon( ggplot2::aes( ymin=.data$Ymin, ymax=.data$Ymax ), alpha=0.2, fill="green", na.rm=TRUE ) +
    ggplot2::labs( y=ylab, x=xlab )
  
  if ( has.smooth ) {
    plt = plt + 
      ggplot2::geom_line( ggplot2::aes( y=.data$Ysmooth ), alpha=0.7, color="green", na.rm=TRUE ) +
      ggplot2::geom_line( ggplot2::aes( y=.data$Ysmooth1 ), color = "red", na.rm=TRUE )
  } else {
    plt = plt + ggplot2::geom_line( data=dplyr::filter( envelope, .data$month >= t0 ), 
                                    ggplot2::aes( y=.data$Ystar ) )
  }
  
  plt
}






