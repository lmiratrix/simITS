# library(stats)

#' @title Mecklenberg PSA Reform Data
#' @description Monthly aggregate outcomes of various measures of interest from Mecklenberg. See MDRC Report.
#' @format A data frame with 54 rows and 10 variables:
#' \describe{
#'   \item{\code{month}}{integer Month, with 0 being month of policy implementation.}
#'   \item{\code{karr}}{integer Total count of arrests.}
#'   \item{\code{pbail}}{double Proportion of cases in a given month assigned bail (or outright detention).}
#'   \item{\code{pptrel}}{double Proportion of cases assigned to pretrial supervised release.}
#'   \item{\code{pror}}{double Proportion of cases released on own recognizance.}
#'   \item{\code{pb4c}}{double Proportion of cases assigned to money bail (alternate coding from pbail, above).}
#'   \item{\code{avg_days_initial}}{double Average number of days spent detained before release due to bail, case resolution, etc.}
#'   \item{\code{avg_t2d}}{double Average time to case resolution (in days).}
#'   \item{\code{pstint7}}{double Proportion detained longer than 7 days.}
#'   \item{\code{pstint30}}{double Proportion detained longer than 30 days.}
#'}
"mecklenberg"



#' @title New Jersey PSA Reform aggregate data
#' @description Montly aggregate counts of arrests of different types in New Jersey.
#' @format A data frame with 106 rows and 11 variables:
#' \describe{
#'   \item{\code{month}}{integer Index of month.}
#'   \item{\code{sin.m}}{double cos of month number}
#'   \item{\code{cos.m}}{double sin of month number}
#'   \item{\code{M12}}{integer Month number}
#'   \item{\code{Q1}}{integer Indicator of 1st quarter.}
#'   \item{\code{Q2}}{integer Indicator of 2nd quarter.}
#'   \item{\code{Q3}}{integer Indicator of 3rd quarter.}
#'   \item{\code{Q4}}{integer Indicator of 4th quarter.}
#'   \item{\code{n.warrant}}{double Number of warrant arrests}
#'   \item{\code{n.summons}}{double Number of summons arrests}
#'   \item{\code{n}}{double Total number of arrests}
#'   \item{\code{temperature}}{double Average temperature in New Jersey that month.}
#'}
"newjersey"



#' @title Mecklenberg data by subgroup of charge type
#' @description Mecklenberg data that gives proportion of different charge categories of cases given bail (by month).
#' @format A data frame with 144 rows and 5 variables:
#' \describe{
#'   \item{\code{month}}{integer Month, with 0 being month of policy implementation.}
#'   \item{\code{n.cases}}{integer Number of cases of that subgroup for that month}
#'   \item{\code{n.bail}}{interger Total number of cases given bail for that subgroup for that month}
#'   \item{\code{pbail}}{double Proportion of new cases in given subgroup in that month assigned bail} 
#'   \item{\code{category}}{character Category of group (charge type).}
#'}
"meck_subgroup"
