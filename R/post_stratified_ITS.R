

##
## Post-stratified ITS estimator code
##
## This builds on the seasonality code.




#' Calculate overall proportion of cases in each group that lie within a given
#' interval of time defined by t.min and t.max.
#'
#' @param groupname Name of the column that has the grouping categorical
#'   variable
#' @param t.min The start month to aggregate cases over.
#' @param t.max The final month (default is last month).
#' @param dat Dataframe with one row for each time point and group that we are
#'   going to post stratify on.  This dataframe should also have an 'N' column
#'   indicating the number of cases that make up each given row. It should have
#'   a 'month' column for the time.
#' @param N Name of variable holding the counts (weight) in each group.
#' @export
calculate.group.weights = function( groupname, dat, t.min, t.max = max( dat$month ), N = "N" ) {

  stopifnot( N %in% names( dat ) )
  
    # select target months to calibrate averages on
    dat = filter( dat, month >= t.min, month <= t.max )

    # calculate the total sizes for each group
    sdat = dat %>% group_by_( groupname ) %>%
         summarise( N = sum(!!rlang::sym(N)) )
    sdat = sdat %>% ungroup() %>% mutate( pi.star = N / sum(N) )

    sdat
}


#' Aggregate grouped data
#'
#' This will take a dataframe with each row being the outcomes, etc., for a
#' given group for a given month and aggregate those groups for each month.
#'
#' @param dat Dataframe of data
#' @param outcomename String name of the outcome variable in dat.
#' @param groupname Name of grouping variable to post-stratify on. 
#' @param rich If TRUE, add a bunch of extra columns with proportions of the
#'   month that are each group and so forth.
#' @param is.count If TRUE the data are counts, and should be aggregated by sum
#'   rather than by mean.
#' @param covariates group-invariant covariates to preserve in the augmented
#'   rich dataframe.  These are not used in this method for any calculations.
#'   Pass as list of column names of dat
#' @export
aggregate_data = function( dat, outcomename, groupname, is.count=FALSE,
                           rich = TRUE, covariates =NULL ) {

    if ( is.count ) {
        dd <- dat %>% group_by( month ) %>%
            summarise( .Y = sum( (!!rlang::sym(outcomename)) ),
                     #  .Y.bar = sum( (!!rlang::sym(outcomename))  ) / sum(N),
                       N = sum(N) )
        dd[ outcomename ] = dd$.Y
       # dd[ paste0( outcomename, ".bar" ) ] = dd$.Y.bar
        dd$.Y = dd$.Y.bar = NULL
    } else {
        dd <- dat %>% group_by( month ) %>%
            summarise( .Y = sum( N * (!!rlang::sym(outcomename)) ) / sum(N),
                       N = sum(N) )
        dd[ outcomename ] = dd$.Y
        dd$.Y = NULL
    }


    if ( rich ) {
        # calculate group sizes
        ddwts = dat %>% dplyr::select_( "month", groupname, "N" ) %>%
            group_by( month ) %>%
            mutate( pi = N / sum(N ) ) %>%
            dplyr::select( -N ) %>%
            spread_( groupname, "pi", sep=".")
        names(ddwts) = gsub( groupname, "pi", names(ddwts) )

        if ( is.null( covariates ) ) {
          covariates = c()
        } else {
          if ( !all( covariates %in% names( dat ) ) ) {
            stop( "Covariates listed that are not in dataframe" )
          }
        }
        
        # throw in group baselines and covariates as well in wide form
        ddg = dat[ c( "month", groupname, outcomename, covariates ) ]
        ddg = spread_( ddg, groupname, outcomename, sep="." )
        names(ddg) = gsub( groupname, outcomename, names(ddg) )
        stopifnot(nrow(ddg) == nrow( dd ) )  # possibly covariates varied in spread?

        dd = bind_cols( dd, ddg, ddwts )
        dd$month1 = dd$month2 = NULL
    }

    dd
}


#' Adjust an outcome based on the group weights.
#'
#' @param outcomename Name of column that has the outcome to calculated adjusted
#'   values for.
#' @param groupname Name of categorical covariate that determines the groups.
#' @param include.aggregate Include aggregated (unadjusted) totals in the output
#'   as well.
#' @param dat  Dataframe of data.  Requires an N column of total cases
#'   represented in each row.
#' @param pi.star The target weights.  Each month will have its groups
#'   re-weighted to match these target weights.
#' @param is.count Indicator of whether outcome is count data or a continuous
#'   measure (this impacts how aggregation is done).
#' @param covariates Covariates to be passed to aggregation (list of string
#'   variable names).
#' @export
adjust.data = function( dat, outcomename, groupname, pi.star, is.count=FALSE,
                        include.aggregate = FALSE,
                        covariates = NULL ) {

    # add the target subgroup weights to the dataframe
    adat = merge( dat, pi.star[ c( groupname, "pi.star" ) ], by=groupname, all.x = TRUE )

    if ( is.count ) {
        adat[outcomename] = adat[[outcomename]] / adat[["N"]]
    }

    # calculate adjusted outcomes
    adj.dat = adat %>% group_by( month ) %>%
        summarise( #.Y = sum( N * ( !!rlang::sym( outcomename ) ) / sum(N) ),
                   .Y.adj = sum( pi.star * !!rlang::sym( outcomename ) ),
                   N = sum( N ) )

    if ( is.count ) {
        adj.dat = mutate( adj.dat, #.Y = .Y * N,
                          .Y.adj = .Y.adj * N )
    }

    oname = paste0( outcomename, ".adj" )
    adj.dat[ oname ] = adj.dat$.Y.adj
  #  adj.dat[ outcomename ] = adj.dat$.Y
    adj.dat$.Y.adj = adj.dat$.Y = NULL

    if ( include.aggregate ) {
        sdat = aggregate_data( dat, outcomename, groupname, is.count, covariates = covariates )
        adj.dat = merge( adj.dat, sdat, by=c("N","month"), all=TRUE )
    }

    arrange( adj.dat, month )
}





####### For simulation studies and illustration #######

#' A fake DGP for illustrating the code.
#'
#' This code makes synthetic grouped data that can be used to illustrate
#' benefits of post stratification.
#'
#' @param t.min Index of first month
#' @param t.max Index of last month
#' @param t0 last pre-policy timepoint
#' @param method Type of post-stratification structure to generate.
#' @export
make.fake.group.data = function( t.min, t0, t.max, method=c("complex","linear","jersey") ) {
    stopifnot( t.min < t0 )
    stopifnot( t.max > t0 )
    t = t.min:t.max

    method = match.arg(method)

    # number of cases of each type (not impacted by policy)
    # Drug is steadily declining.  violent is slowly increasing.
    N.drug = round( (200-800)*(t - t.min)/(t.max-t.min) + 800 )
    N.violent = round( (300-100)*(t - t.min)/(t.max-t.min) + 100 )
    
    if ( method == "complex" ) {
      # Add a seasonality component
      N.violent = N.violent + 55 * sin( 2 * pi * t / 12)
    }
    
    if ( method == "jersey" ) {
      N.drug = rpois( length( t ), lambda=700 )
      N.violent = rpois( length( t ), lambda=400 )
      N.property = rpois( length( t ), lambda=500 )
      N.drug = pmax( 0.55, pmin( 1, 1 - (t - t0) / 25 ) ) * N.drug
    }
    
    if ( method=="linear" || method == "complex") {

        # impact on proportion of cases with outcome
        prop.base = arm::logit( seq( 0.8, 0.4, length.out=length(t) ) )

        prop.violent = arm::invlogit( prop.base - 1.5 + rnorm( length(t), mean=0, sd=0.05 )
                                      + (t>t0) * pmin( 0.3*(t-t0), 1.5 ) )

        prop.drug =    arm::invlogit( prop.base + rnorm( length(t), mean=0, sd=0.05 )
                                      - (t>t0) * (0.05*(t-t0)) )
    } else {
      # impact on proportion of cases with outcome
      prop.base = arm::logit( seq( 0.5, 0.55, length.out=length(t) ) )
      
      prop.violent = arm::invlogit( prop.base + 1.5 + rnorm( length(t), mean=0, sd=0.02 )
                                    - (t>t0) * (0.01*(t-t0)) )
      
      prop.property = arm::invlogit( prop.base + 1 + rnorm( length(t), mean=0, sd=0.02 )
                                    - (t>t0) * (0.003*(t-t0)) )
      
      prop.drug =    arm::invlogit( prop.base + rnorm( length(t), mean=0, sd=0.02 ) 
                                    - (t>t0) * (0.005*(t-t0)) )
      
    }

    
    ## Scenario 1b: multifacet, complex.
    if ( FALSE ) {
        # number of cases of each type (not impacted by policy)
        N.drug = round( 300 - 5 * t + 2 * sin( 2 * pi * t / 12) )
        N.violent = 30 + round( 100 - 0.1 * t + 10 * sin( 2 * pi * t / 12) )

        # impact on proportion of cases with outcome
        prop.drug = 0.6 - 0.01 * t   # baseline index (will recalculate below)
        prop.violent = arm::invlogit( prop.drug/2 + rnorm( length(t), mean=0, sd=0.15 )
                                      + (t>t0) * pmin( 0.3*(t-t0), 1.5 ) )
        prop.drug = arm::invlogit( -1 + prop.drug - (t>t0)* (0.15*(t-t0)) + rnorm( length(t), mean=0, sd=0.15 ) )
    }

    ## Scenario 2: change in number of drug cases, but no impact on case handling within category
    ## Nonsensical, I think.
    if ( FALSE ) {
        N.drug = round( 100 - 0.5 * t - (t >= t0) * ( 10 + (t-t0) * 2 ) )
        N.violent = round( 100 - 0.1 * t + 10 * sin( 2 * pi * t / 12) )

        prop.drug = 0.6 - 0.01 * t
        prop.violent = arm::invlogit( prop.drug + 0.2 + rnorm( length(t), mean=0, sd=0.15 ) )
        prop.drug = arm::invlogit( -2 + prop.drug + rnorm( length(t), mean=0, sd=0.15 ) )
    }


    # bundle our subgroups
    make.frame = function( N, prop, type="unknown" ) {
        Y = round( N * prop )
        data.frame( month = t, type=type, N=N, Y=Y, prop = Y / N, stringsAsFactors = FALSE )
    }

    df = bind_rows( make.frame( N.drug, prop.drug, "drug" ),
                    make.frame( N.violent, prop.violent, "violent" ) )
    if ( method =="jersey" ) {
      df = bind_rows( df, 
                      make.frame( N.property, prop.property, "property" ) )
    }
    df = mutate( df,
                 M = 1 + (month %% 12),
                 M.ind = as.factor(M),
                 A = sin( 2 * pi * month / 12 ),
                 B = cos( 2 * pi * month / 12 ),
                 Tx = as.numeric(month >= t0) )

    df = arrange( df, month )
    df
}





#### Exploring and testing our fake data structure ####

if ( FALSE ) {
    # fake, illustration data -- specifying the range of months
    t.min = -12*6.5
    t0 = 0
    t.max = 18

    dat = make.fake.group.data( t.min, t0, t.max, method = "jersey" )
    head( dat )

    ss = aggregate_data( dat, "prop", "type", rich=TRUE )
    head( ss )
    plot( ss$pi.drug )
    
    sdat = aggregate_data( dat, "prop", "type", is.count=FALSE, rich = FALSE )
    sdat2 = aggregate_data( dat, "Y", "type", is.count=TRUE, rich= FALSE )
    sdat = merge( sdat, sdat2, by=c("month","N") )
    head( sdat )
    sdat$type = "all"

    d2 = bind_rows( dat, sdat )
    d2 = gather( d2, Y, N, prop, key="variable", value="outcome" )
    ggplot( d2, aes( month, outcome, col=type ) ) +
        facet_wrap( ~ variable , scales = "free_y" ) +
        geom_line() +
        geom_vline( xintercept=t0, col="red" )



    dat %>% group_by( type ) %>% summarise( N.bar = mean(N),
                                            Y.bar = mean(Y),
                                            prop.bar = mean(prop) )

}


#### Examining aggregation functions ####
if ( FALSE ) {
    head( dat )

    # Calculate how to weight the groups
    pis = calculate.group.weights( "type", dat, t0, max(dat$month) )
    pis



    # looking at rates
    head( dat )
    sdat = aggregate_data( dat, "prop", "type", is.count=FALSE )

    adjdat = adjust.data( dat, "prop", "type", pis )
    head( adjdat )
    adjdat = merge( adjdat, sdat, by=c("N","month"), all=TRUE )
    head( adjdat )

    d1 = gather( adjdat, starts_with( "pi" ), key="group", value="pi" )
    head( d1 )
    ggplot( d1, aes( month, pi, col=group ) ) +
        geom_line() +
        labs( title="Sizes of the groups")

    d2 = gather( adjdat, starts_with( "prop" ), key="outcome", value="Y" )
    head( d2 )

    ggplot( d2, aes( month, Y, col=outcome ) ) +
        geom_line()

    # checking calculations
    head( adjdat )


    # Looking at counts
    sdat = aggregate_data( dat, "Y", "type", is.count=TRUE )
    head( sdat )

    adjdat = adjust.data( dat, "Y", "type", pis, is.count = TRUE )
    head( adjdat )
    d2 = gather( adjdat, Y.adj, Y, starts_with( "type." ), key="outcome", value="Y" )
    head( d2 )

    ggplot( d2, aes( month, Y, col=outcome ) ) +
        geom_line()

}



#### Illustration of the easy modeling approach  ####



if ( FALSE ) {

    # fake, illustration data -- specifying the range of months
    t.min = -12*6.5
    t0 = 0
    t.max = 18

    dat = make.fake.group.data( t.min, t0, t.max )
    head( dat )

    pis = calculate.group.weights( "type", dat, t0, max(dat$month) )
    pis


    ##
    ## The proportion as outcome
    ##
    adjdat = adjust.data( dat, "prop", "type", pis, include.aggregate=TRUE )
    head( adjdat )

    adjdat = add.lagged.covariates(adjdat, "prop.adj", c("A","B") )
    head( adjdat )

    # Modeling adjusted and not
    envelope.adj = process.outcome.model( "prop.adj", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

    envelope = process.outcome.model( "prop", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

    envelope.drug = process.outcome.model( "prop.drug", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )
    envelope.violent = process.outcome.model( "prop.violent", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

    env = bind_rows( raw=envelope, adjusted=envelope.adj, drug=envelope.drug, violent=envelope.violent, .id="model")
    head( env )
    plt <- ggplot( env, aes( month, col=model ) ) +
        geom_line( aes(y=Ystar), lty=2 ) +
        geom_line( aes(y=Y)) + geom_point( aes( y=Y ), size=0.5 ) +
        #geom_line( aes(y=Ysmooth1), lty=2 ) +
        geom_vline( xintercept=t0 )

    #plt

    plt +         facet_wrap( ~model )



    ##
    ## And with Y (counts)
    ##
    adjdat = adjust.data( dat, "Y", "type", pis, include.aggregate=TRUE, is.count = TRUE )
    head( adjdat )

    qplot( Y, Y.adj, data=adjdat )

    adjdat = add.lagged.covariates(adjdat, "Y.adj", c("A","B") )
    head( adjdat )

    # Modeling adjusted and not
    envelope.adj = process.outcome.model( "Y.adj", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

    envelope = process.outcome.model( "Y", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

    envelope.drug = process.outcome.model( "Y.drug", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )
    envelope.violent = process.outcome.model( "Y.violent", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

    env = bind_rows( raw=envelope, adjusted=envelope.adj, drug=envelope.drug, violent=envelope.violent, .id="model")
    head( env )
    plt <- ggplot( env, aes( month, col=model ) ) +
        geom_line( aes(y=Ystar), lty=2 ) +
        geom_line( aes(y=Y)) + geom_point( aes( y=Y ), size=0.5 ) +
        #geom_line( aes(y=Ysmooth1), lty=2 ) +
        geom_vline( xintercept=t0 )

    #plt

    plt +         facet_wrap( ~model )


}

