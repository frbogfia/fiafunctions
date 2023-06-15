

# This should make the "opt" environment available globally
## This is where we import the necessary packages (We hopefully can remove this with :: sometime)
#' @import dplyr graphics grDevices lubridate utils zoo 
#' @export
set.opt <- function() {
  opt = list(
              frame.lwd = 1.5,
              line.lwd  = rep(1.7,20),
              label.cex = .75,
              axis.cex  = .65,
              exhibit.title.cex = 1,
              chart.title.line = 0.8,
              chart.title.cex = 0.8,
              foot.cex  = .55,
              legend.cex = .7,
              line.types = rep(1,20),
              legend.inset = .03,
              yaxis.line = 0,
              tick.length = 0.025,
              yaxis.pos= .5,
              colors = c("black","firebrick","SteelBlue", "DarkOliveGreen3", "goldenrod1", "blueviolet","magenta","darkseagreen","darkorange2", "darkturquoise","darksalmon","tomato2", "lightpink4", "forestgreen","peachpuff4"),
              key_adj_x=1,
              key.cex=0.75,
              paneltitleline=1.2,
              paneltitlecex=.85,
              tealbook.months = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."),
              tealbook.quarters = c("Q1","Q2","Q3","Q4"))
  assign("opt", opt, envir = .GlobalEnv)
  return(opt)
}
# We previously imported # utils grDevices graphics stats zoo tis frb fame RODBC

#' @title set.presets
#' @description This should adds the presets to the global environment
#' @export
set.presets <- function() {

  yqm_bars <- custom_preset(dynamicColors = T, dynamicDates = T, series2LabelFreq = 'quarter', series3LabelFreq = 'month',
                            dataFrame2 = rlang::expr(dataFrame), dataFrame3 = rlang::expr(dataFrame), famePath2 = rlang::expr(famePath), famePath3 = rlang::expr(famePath),
                            series2 = rlang::expr(series), series3 = rlang::expr(series), printLastValue = rlang::expr(lastValuePrelim), lastValueFreq = 'day',
                            xTicksBelowAxis = T, xBarTicksSeries1 = F, xBarTicksSeries2 = F, xBarTicksSeries3 = F, xSeriesDivider = F,
                            xMajorTickFreq = 'year')

  year_axis_bars <- custom_preset(dynamicColors = T, xBarTicksSeries1 = F, xBarTicksSeries2 = F, xBarTicksSeries3 = F, xSeriesDivider = F,
                                  xMajorTickFreq = 'year', xMinorTickFreq = NA, xLabelFreq = 'year')

  assign("yqm_bars", yqm_bars, envir = .GlobalEnv)
  assign("year_axis_bars", year_axis_bars, envir = .GlobalEnv)

  return(c(yqm_bars, year_axis_bars))
}



.onLoad <- function(libname, pkgname) {
  #require(grDevices) #Required for setcmpar
  set.opt()
  set.presets()
  utils::globalVariables(c("opt", "yqm_bars", "year_axis_bars"))
}



.onAttach <- function(libname, pkgname) {
  opt = list()
}

#Adding a test Data Frame
testDataFrame <- data.frame(date = as.Date(c("2000-01-01", "2001-01-01", "2002-01-01", "2003-01-01", "2004-01-01", "2005-01-01", "2006-01-01", "2007-01-01", "2008-01-01", "2009-01-01", "2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01")), value = seq(1,20,1), value2=seq(-2, 36, 2), line= seq(8,-11, -1), line2=seq(5,-14, -1))

