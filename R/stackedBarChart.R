#' @title Stacked Bar Chart
#' @description This creates a stacked bar chart
#' @export
stackedBarChart <- function(...) {

  bar_args <- rlang::list2(...)
  bar_args$stacked <- T

  do.call('barChart', args = bar_args)

}
