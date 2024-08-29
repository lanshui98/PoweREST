#' An example of power results from PoweREST
#'
#' A subset of power results from PoweREST
#' by running PoweREST(Peri,cond='Condition',
#' replicates=5,spots_num=80,iteration=2)
#'
#' @format ## `result_example`
#' A data frame with ~20,000 rows and 3 columns:
#' \describe{
#'   \item{power}{power values}
#'   \item{avg_logFC}{average log2FC}
#'   \item{avg_PCT}{percentage of spots detecting the gene}
#' }
"result_example"
#' An example of power results with multiple replicates number
#'
#' A subset of power results with multiple
#' replicates number from PoweREST
#'
#' @format ## `power_example`
#' A data frame with 844 rows and 5 columns:
#' \describe{
#'   \item{avg_logFC}{average log2FC}
#'   \item{mean_PCT}{percentage of spots detecting the gene}
#'   \item{sample_size}{number of replicates}
#'   \item{power}{power values}
#'   \item{avg_log2FC_abs}{the absolute value of average log2FC}
#' }
"power_example"
