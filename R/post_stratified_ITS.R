

##
## Post-stratified ITS estimator code
##



#' Calculate proportion of subgroups across time
#'
#' Calculate overall proportion of cases in each group that lie within a given
#' interval of time defined by t_min and t_max.
#'
#' @inheritParams aggregate_data
#' @param t_min The start month to aggregate cases over.
#' @param t_max The final month (default is last month).
#' @return Dataframe of each group along with overall average group weight in
#'   the specified timespan.
#' @example examples/aggregate_data_etc.R
#' @export
calculate_group_weights = function( groupname, dat, t_min, t_max = max( dat$month ), Nname = "N" ) {

  stopifnot( Nname %in% names( dat ) )
  #groupname.q = quo( groupname )
    # select target months to calibrate averages on
    dat = dplyr::filter( dat, month >= t_min, month <= t_max )

    # calculate the total sizes for each group
    sdat = dat %>% dplyr::group_by_at( groupname ) %>%
         dplyr::summarise( N = sum(!!rlang::sym(Nname)) )
    
    sdat = sdat %>% dplyr::ungroup() %>% dplyr::mutate( pi_star = N / sum(N) )

    sdat
}


#' Aggregate grouped data
#'
#' This will take a dataframe with each row being the outcomes, etc., for a
#' given group for a given month and aggregate those groups for each month.
#'
#' @param dat Dataframe with one row for each time point and group that we are
#'   going to post stratify on.  This dataframe should also have an column with
#'   passed name "Nname" indicating the number of cases that make up each given
#'   row. It should have a 'month' column for the time.
#' @param outcomename String name of the outcome variable in dat.
#' @param groupname Name of the column that has the grouping categorical
#'   variable
#' @param Nname Name of variable holding the counts (weight) in each group.
#' @param rich If TRUE, add a bunch of extra columns with proportions of the
#'   month that are each group and so forth.
#' @param is_count If TRUE the data are counts, and should be aggregated by sum
#'   rather than by mean.
#' @param covariates group-invariant covariates to preserve in the augmented
#'   rich dataframe.  These are not used in this method for any calculations.
#'   Pass as list of column names of dat
#' @return Dataframe of aggregated data, one row per month.  If rich=TRUE many
#'   extra columns with further information.
#' @example examples/aggregate_data_etc.R
#' @export
aggregate_data = function( dat, outcomename, groupname, Nname, 
                           is_count=FALSE,
                           rich = TRUE, covariates = NULL ) {

  if ( is.null( covariates ) ) {
    covariates = c()
  } else {
    if ( !all( covariates %in% names( dat ) ) ) {
      stop( "Covariates listed that are not in dataframe" )
    }
  }
  
    if ( is_count ) {
        dd <- dat %>% dplyr::group_by( month ) %>%
            dplyr::summarise( .Y = sum( (!!rlang::sym(outcomename)) ),
                     #  .Y.bar = sum( (!!rlang::sym(outcomename))  ) / sum(N),
                       N = sum( (!!rlang::sym(Nname)) ) )
        dd[ outcomename ] = dd$.Y
       # dd[ paste0( outcomename, ".bar" ) ] = dd$.Y.bar
        dd$.Y = dd$.Y.bar = NULL
    } else {
        dd <- dat %>% dplyr::group_by( month ) %>%
            dplyr::summarise( .Y = sum( (!!rlang::sym(Nname)) * (!!rlang::sym(outcomename)) ) / sum( (!!rlang::sym(Nname)) ),
                       N = sum( (!!rlang::sym(Nname)) ) )
        dd[ outcomename ] = dd$.Y
        dd$.Y = NULL
    }


    if ( rich ) {
        # calculate group sizes
        ddwts = dat %>% dplyr::select( "month", groupname, Nname ) %>%
          dplyr::rename( N = {{Nname}} ) %>%
          dplyr::group_by( month ) %>%
          dplyr::mutate( pi = N / sum( N ) ) %>% 
          dplyr::select( -N ) %>%
          tidyr::pivot_wider( month, 
                              names_from = groupname, values_from = "pi",
                              names_prefix = "pi_" )
        
        # throw in group baselines and covariates as well in wide form
        ddg = dat[ c( "month", groupname, outcomename, covariates ) ]
        ddg = tidyr::spread_( ddg, groupname, outcomename, sep="_" )
        names(ddg) = gsub( groupname, outcomename, names(ddg) )
        stopifnot(nrow(ddg) == nrow( dd ) )  # possibly covariates varied in spread?

        ddg$month = ddwts$month = NULL
        dd = dplyr::bind_cols( dd, ddg, ddwts )
    }

    dd
}


#' Adjust an outcome time series based on the group weights.
#'
#' Reweight the components of a series to match target weights for several
#' categories. This is a good preprocessing step to adjust for time-varying
#' covariates such as changing mix of case types.
#'
#' @param outcomename Name of column that has the outcome to calculated adjusted
#'   values for.
#' @param groupname Name of categorical covariate that determines the groups.
#' @param Nname Name of column in dat that contains total cases (this is the
#'   name of the variable used to generate the weights in pi_star).
#' @param include_aggregate Include aggregated (unadjusted) totals in the output
#'   as well.
#' @param dat  Dataframe of data.  Requires an N column of total cases
#'   represented in each row.
#' @param pi_star The target weights.  Each month will have its groups
#'   re-weighted to match these target weights.
#' @param is_count Indicator of whether outcome is count data or a continuous
#'   measure (this impacts how aggregation is done).
#' @param covariates Covariates to be passed to aggregation (list of string
#'   variable names).
#' @return Dataframe of adjusted data.
#' @example examples/aggregate_data_etc.R
#' @export
adjust_data = function( dat, outcomename, groupname, Nname, pi_star, is_count=FALSE,
                        include_aggregate = FALSE,
                        covariates = NULL ) {

    # add the target subgroup weights to the dataframe
    adat = merge( dat, pi_star[ c( groupname, "pi_star" ) ], by=groupname, all.x = TRUE )

    if ( is_count ) {
        adat[outcomename] = adat[[outcomename]] / adat[[Nname]]
    }

    # calculate adjusted outcomes
    adj.dat = adat %>% dplyr::group_by( month ) %>%
        dplyr::summarise( #.Y = sum( N * ( !!rlang::sym( outcomename ) ) / sum(N) ),
                   .Y.adj = sum( pi_star * !!rlang::sym( outcomename ) ),
                   N = sum(!!rlang::sym(Nname) ) )

    if ( is_count ) {
        adj.dat = dplyr::mutate( adj.dat, #.Y = .Y * N,
                          .Y.adj = .Y.adj * N )
    }

    oname = paste0( outcomename, ".adj" )
    adj.dat[ oname ] = adj.dat$.Y.adj
  #  adj.dat[ outcomename ] = adj.dat$.Y
    adj.dat$.Y.adj = adj.dat$.Y = NULL

    if ( include_aggregate ) {
        sdat = aggregate_data( dat, 
                               outcomename=outcomename, groupname=groupname, Nname=Nname, 
                               is_count=is_count, covariates = covariates )
        adj.dat = merge( adj.dat, sdat, by=c("N","month"), all=TRUE )
    }

    dplyr::arrange( adj.dat, month )
}





####### For simulation studies and illustration #######

#' A fake DGP with time varying categorical covariate for illustrating the code.
#'
#' This code makes synthetic grouped data that can be used to illustrate
#' benefits of post stratification.
#'
#' @param t_min Index of first month
#' @param t_max Index of last month
#' @param t0 last pre-policy timepoint
#' @param method Type of post-stratification structure to generate (three designs of 'complex', 'linear' and 'jersey' were originally concieved of when designing simulation studies with different types of structure).
#' @return Dataframe of fake data, with one row per group per time period.
#' @examples 
#' fdat = generate_fake_grouped_data(t_min=-5,t_max=10, t0 = 0)
#' table( fdat$month )
#' table( fdat$type )
#' @export
generate_fake_grouped_data = function( t_min, t0, t_max, method=c("complex","linear","jersey") ) {
    stopifnot( t_min < t0 )
    stopifnot( t_max > t0 )
    t = t_min:t_max

    method = match.arg(method)

    # number of cases of each type (not impacted by policy)
    # Drug is steadily declining.  violent is slowly increasing.
    N.drug = round( (200-800)*(t - t_min)/(t_max-t_min) + 800 )
    N.violent = round( (300-100)*(t - t_min)/(t_max-t_min) + 100 )
    
    if ( method == "complex" ) {
      # Add a seasonality component
      N.violent = N.violent + 55 * sin( 2 * pi * t / 12)
    }
    
    if ( method == "jersey" ) {
      N.drug = stats::rpois( length( t ), lambda=700 )
      N.violent = stats::rpois( length( t ), lambda=400 )
      N.property = stats::rpois( length( t ), lambda=500 )
      N.drug = pmax( 0.55, pmin( 1, 1 - (t - t0) / 25 ) ) * N.drug
    }
    
    if ( method=="linear" || method == "complex") {

        # impact on proportion of cases with outcome
        prop.base = arm::logit( seq( 0.8, 0.4, length.out=length(t) ) )

        prop.violent = arm::invlogit( prop.base - 1.5 + stats::rnorm( length(t), mean=0, sd=0.05 )
                                      + (t>t0) * pmin( 0.3*(t-t0), 1.5 ) )

        prop.drug =    arm::invlogit( prop.base + stats::rnorm( length(t), mean=0, sd=0.05 )
                                      - (t>t0) * (0.05*(t-t0)) )
    } else {
      # impact on proportion of cases with outcome
      prop.base = arm::logit( seq( 0.5, 0.55, length.out=length(t) ) )
      
      prop.violent = arm::invlogit( prop.base + 1.5 + stats::rnorm( length(t), mean=0, sd=0.02 )
                                    - (t>t0) * (0.01*(t-t0)) )
      
      prop.property = arm::invlogit( prop.base + 1 + stats::rnorm( length(t), mean=0, sd=0.02 )
                                    - (t>t0) * (0.003*(t-t0)) )
      
      prop.drug =    arm::invlogit( prop.base + stats::rnorm( length(t), mean=0, sd=0.02 ) 
                                    - (t>t0) * (0.005*(t-t0)) )
      
    }

    
    ## Scenario 1b: multifacet, complex.
    # if ( FALSE ) {
    #     # number of cases of each type (not impacted by policy)
    #     N.drug = round( 300 - 5 * t + 2 * sin( 2 * pi * t / 12) )
    #     N.violent = 30 + round( 100 - 0.1 * t + 10 * sin( 2 * pi * t / 12) )
    # 
    #     # impact on proportion of cases with outcome
    #     prop.drug = 0.6 - 0.01 * t   # baseline index (will recalculate below)
    #     prop.violent = arm::invlogit( prop.drug/2 + stats::rnorm( length(t), mean=0, sd=0.15 )
    #                                   + (t>t0) * pmin( 0.3*(t-t0), 1.5 ) )
    #     prop.drug = arm::invlogit( -1 + prop.drug - (t>t0)* (0.15*(t-t0)) + stats::rnorm( length(t), mean=0, sd=0.15 ) )
    # }
    # 
    # ## Scenario 2: change in number of drug cases, but no impact on case handling within category
    # ## Nonsensical, I think.
    # if ( FALSE ) {
    #     N.drug = round( 100 - 0.5 * t - (t >= t0) * ( 10 + (t-t0) * 2 ) )
    #     N.violent = round( 100 - 0.1 * t + 10 * sin( 2 * pi * t / 12) )
    # 
    #     prop.drug = 0.6 - 0.01 * t
    #     prop.violent = arm::invlogit( prop.drug + 0.2 + stats::rnorm( length(t), mean=0, sd=0.15 ) )
    #     prop.drug = arm::invlogit( -2 + prop.drug + stats::rnorm( length(t), mean=0, sd=0.15 ) )
    # }


    # bundle our subgroups
    make.frame = function( N, prop, type="unknown" ) {
        Y = round( N * prop )
        data.frame( month = t, type=type, N=N, Y=Y, prop = Y / N, stringsAsFactors = FALSE )
    }

    df = dplyr::bind_rows( make.frame( N.drug, prop.drug, "drug" ),
                    make.frame( N.violent, prop.violent, "violent" ) )
    if ( method =="jersey" ) {
      df = dplyr::bind_rows( df, 
                      make.frame( N.property, prop.property, "property" ) )
    }
    df = mutate( df,
                 M = 1 + (month %% 12),
                 M.ind = as.factor(M),
                 A = sin( 2 * pi * month / 12 ),
                 B = cos( 2 * pi * month / 12 ),
                 Tx = as.numeric(month >= t0) )

    df = dplyr::arrange( df, month )
    df
}





#### Exploring and testing our fake data structure ####

if ( FALSE ) {
    # fake, illustration data -- specifying the range of months
    t_min = -12*6.5
    t0 = 0
    t_max = 18

    dat = generate_fake_grouped_data( t_min, t0, t_max, method = "jersey" )
    head( dat )

    ss = aggregate_data( dat, "prop", "type", rich=TRUE )
    head( ss )
    plot( ss$pi_drug )
    
    sdat = aggregate_data( dat, "prop", "type", is_count=FALSE, rich = FALSE )
    sdat2 = aggregate_data( dat, "Y", "type", is_count=TRUE, rich= FALSE )
    sdat = merge( sdat, sdat2, by=c("month","N") )
    head( sdat )
    sdat$type = "all"

    d2 = dplyr::bind_rows( dat, sdat )
    d2 = tidyr::gather( d2, Y, N, prop, key="variable", value="outcome" )
    ggplot2::ggplot( d2, ggplot2::aes( month, outcome, col=type ) ) +
        ggplot2::facet_wrap( ~ variable , scales = "free_y" ) +
        ggplot2::geom_line() +
        ggplot2::geom_vline( xintercept=t0, col="red" )



    dat %>% dplyr::group_by( type ) %>% dplyr::summarise( N.bar = mean(N),
                                            Y.bar = mean(Y),
                                            prop.bar = mean(prop) )

}


#### Examining aggregation functions ####
if ( FALSE ) {
    head( dat )

    # Calculate how to weight the groups
    pis = calculate_group_weights( "type", dat, t0, max(dat$month) )
    pis



    # looking at rates
    head( dat )
    sdat = aggregate_data( dat, "prop", "type", is_count=FALSE )

    adjdat = adjust_data( dat, "prop", "type", pis )
    head( adjdat )
    adjdat = merge( adjdat, sdat, by=c("N","month"), all=TRUE )
    head( adjdat )

    d1 = gather( adjdat, starts_with( "pi" ), key="group", value="pi" )
    head( d1 )
    ggplot2::ggplot( d1, ggplot2::aes( month, pi, col=group ) ) +
        ggplot2::geom_line() +
        ggplot2::labs( title="Sizes of the groups")

    d2 = tidyr::gather( adjdat, starts_with( "prop" ), key="outcome", value="Y" )
    head( d2 )

    ggplot2::ggplot( d2, ggplot2::aes( d2$month, d2$Y, col=outcome ) ) +
        ggplot2::geom_line()

    # checking calculations
    head( adjdat )


    # Looking at counts
    sdat = aggregate_data( dat, "Y", "type", is_count=TRUE )
    head( sdat )

    adjdat = adjust_data( dat, "Y", "type", pis, is_count = TRUE )
    head( adjdat )
    d2 = tidyr::gather( adjdat, Y.adj, Y, starts_with( "type." ), key="outcome", value="Y" )
    head( d2 )

    ggplot2::ggplot( d2, ggplot2::aes( d2$month, d2$Y, col=outcome ) ) +
        ggplot2::geom_line()

}



#### Illustration of the easy modeling approach  ####



if ( FALSE ) {

    # fake, illustration data -- specifying the range of months
    t_min = -12*6.5
    t0 = 0
    t_max = 18

    dat = generate_fake_grouped_data( t_min, t0, t_max )
    head( dat )

    pis = calculate_group_weights( "type", dat, t0, max(dat$month) )
    pis


    ##
    ## The proportion as outcome
    ##
    adjdat = adjust_data( dat, "prop", "type", pis, include_aggregate=TRUE )
    head( adjdat )

    adjdat = add_lagged_covariates(adjdat, "prop.adj", c("A","B") )
    head( adjdat )

    # Modeling adjusted and not
    envelope.adj = process_outcome_model( "prop.adj", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

    envelope = process_outcome_model( "prop", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

    envelope.drug = process_outcome_model( "prop.drug", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )
    envelope.violent = process_outcome_model( "prop.violent", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

    env = dplyr::bind_rows( raw=envelope, adjusted=envelope.adj, drug=envelope.drug, violent=envelope.violent, .id="model")
    head( env )
    plt <- ggplot2::ggplot( env, ggplot2::aes( month, col=model ) ) +
        ggplot2::geom_line( ggplot2::aes(y= env$Ystar), lty=2 ) +
        ggplot2::geom_line( ggplot2::aes(y=env$Y)) + ggplot2::geom_point( ggplot2::aes( y=env$Y ), size=0.5 ) +
        #geom_line( aes(y=Ysmooth1), lty=2 ) +
        ggplot2::geom_vline( xintercept=t0 )

    #plt

    plt +         facet_wrap( ~model )



    ##
    ## And with Y (counts)
    ##
    adjdat = adjust_data( dat, "Y", "type", pis, include_aggregate=TRUE, is_count = TRUE )
    head( adjdat )

    qplot( Y, Y.adj, data=adjdat )

    adjdat = add_lagged_covariates(adjdat, "Y.adj", c("A","B") )
    head( adjdat )

    # Modeling adjusted and not
    envelope.adj = process_outcome_model( "Y.adj", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

    envelope = process_outcome_model( "Y", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

    envelope.drug = process_outcome_model( "Y.drug", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )
    envelope.violent = process_outcome_model( "Y.violent", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

    env = dplyr::bind_rows( raw=envelope, adjusted=envelope.adj, drug=envelope.drug, violent=envelope.violent, .id="model")
    head( env )
    plt <- ggplot2::ggplot( env, ggplot2::aes( month, col=model ) ) +
        ggplot2::geom_line( aes(y= env$Ystar), lty=2 ) +
        ggplot2::geom_line( aes(y= env$Y)) + ggplot2::geom_point( aes( y=env$Y ), size=0.5 ) +
        # ggplot2::geom_line( aes(y=Ysmooth1), lty=2 ) +
        ggplot2::geom_vline( xintercept=t0 )

    #plt

    plt +         ggplot2::facet_wrap( ~model )


}

