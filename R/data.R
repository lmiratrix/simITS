

#' @title Mechlenberg PSA Reform Data
#' @description Monthly aggregate outcomes of various measures of interest from Mechlenberg. See MDRC Report.
#' @format A data frame with 54 rows and 10 variables:
#' \describe{
#'   \item{\code{month_t}}{integer Month, with 0 being month of policy implementation.}
#'   \item{\code{karr}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pbail}}{double Proportion of cases in a given month assigned bail (or outright detention).}
#'   \item{\code{pptrel}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pror}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pb4c}}{double COLUMN_DESCRIPTION}
#'   \item{\code{avg_days_initial}}{double COLUMN_DESCRIPTION}
#'   \item{\code{avg_t2d}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pstint7}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pstint30}}{double COLUMN_DESCRIPTION}
#'}
"mecklenberg"



#' @title New Jersey PSA Reform aggregate data
#' @description Montly aggregate counts of arrests of different types in New Jersey.
#' @format A data frame with 106 rows and 11 variables:
#' \describe{
#'   \item{\code{tsin1}}{double COLUMN_DESCRIPTION}
#'   \item{\code{tcos1}}{double COLUMN_DESCRIPTION}
#'   \item{\code{M12}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Q1}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Q2}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Q3}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Q4}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{compwarr.cs}}{double Number of warrant arrests}
#'   \item{\code{compsumm.cs}}{double Number of summons arrests}
#'   \item{\code{comptot.cs}}{double Total number of arrests}
#'   \item{\code{temperature}}{double Average temperature in New Jersey that month.}
#'}
"newjersey"



#' @title Mecklenberg subgroup data
#' @description Mecklenberg data of proportion of different categories of cases given bail (by month).
#' @format A data frame with 144 rows and 5 variables:
#' \describe{
#'   \item{\code{month}}{integer Month, with 0 being month of policy implementation.}
#'   \item{\code{n.cases}}{integer Number of cases total that month}
#'   \item{\code{category}}{character Class of subgroup}
#'   \item{\code{n}}{integer Number of cases in subgroup}
#'   \item{\code{pbail}}{double Proportion of cases in subgroup assigned bail} 
#'}
"meck_subgroup"
