#' @title defineXAxisBar
#' @description This defines the X-axis for Bar Charts
#' @export
defineXAxisBar <- function(df_full = plot.data.full,
                           series1Length,
                           series2Length,
                           series3Length,
                           bars,
                           xBarTickAdj,
                           xMajorTickFreq,
                           xMinorTickFreq,
                           xBarTicksSeries1,
                           xBarTicksSeries2,
                           xBarTicksSeries3,
                           xSeriesDivider
                           ) {

  #### define x axis labels and locations for dates ####

  lagg <- function(x) c(NA, x[-length(x)])

  # where does each date interval start? where does it end? use this to
  # inform tick and label placements based on dates
  barPlaceX <- function(df, freq, bars, position) {

    if (freq == 'year') {
      formatt <- format(df$date, "%Y")
    } else if (freq == 'quarter') {
      formatt <- as.yearqtr(df$date)
    } else if (freq == 'month') {
      formatt <- as.yearmon(df$date)
    } else if (freq == 'day') {
      formatt <- unique(df$date)
    }

    periodStart <- table(formatt) %>% as.vector %>% lagg %>% coalesce(1) %>% cumsum
    periodEnd <- table(formatt) %>% as.vector %>% cumsum

    if (position == 0) {
      (colMeans(bars)[periodStart] + colMeans(bars)[periodEnd]) / 2
    } else if (position == .5) {
      c(0, (colMeans(bars)[periodStart][-1] + colMeans(bars)[periodEnd][-length(periodEnd)]) / 2)
    }

  }


  # year
  yearLabels <- seq(min(year(df_full$date)), max(year(df_full$date)), by = 1) 
  yearLabelsLocations <- barPlaceX(df_full, 'year', bars, position = 0)

  # quarter
  quarterLabels <- as.yearqtr(df_full$date) %>% unique %>% stringr::str_remove("[:digit:]{4} ")
  quarterLabelsLocations <- barPlaceX(df_full, 'quarter', bars, position = 0)

  # month
  monthLabels <- as.yearmon(df_full$date) %>% unique %>% stringr::str_remove(" [:digit:]{4}")
  monthLabelsLocations <- barPlaceX(df_full, 'month', bars, position = 0)

  # yearmonth
  yearMonthLabels <- as.yearmon(df_full$date) %>% unique
  yearMonthLabelsLocations <- monthLabelsLocations

  # day
  dayLabels <- format(df_full$date, '%d')
  dayLabelsLocations <- barPlaceX(df_full, 'day', bars, position = 0)

  # major axis ticks
  if (!is.na(xMajorTickFreq)) {
    xMajorTickLocations <- barPlaceX(df_full, xMajorTickFreq, bars, position = 0.5)
  } else {
    xMajorTickLocations <- NULL
  }


  # minor axis ticks
  if (!is.na(xMinorTickFreq)) {
    xMinorTickLocations <- barPlaceX(df_full, xMinorTickFreq, bars, position = 0.5)
  } else {
    xMinorTickLocations <- NULL
  }



  #### define x axis ticks for bars ####

  # decide whether to draw ticks middle of group of bars (0) or in between groups of bars (.5, default)
  if (xBarTickAdj == 0) {

    xBarTicksLocations <- colMeans(bars)

  } else if (xBarTickAdj == 0.5) {

    xBarTicksLocations <- c(0, (colMeans(bars)[-ncol(bars)] + colMeans(bars)[-1]) / 2)

  }

  # toggle on and off for each series
  xBarTicksLocations <- xBarTicksLocations[c(rep(xBarTicksSeries1, series1Length),
                                             rep(xBarTicksSeries2, series2Length),
                                             rep(xBarTicksSeries3, series3Length))]


  #### define x series dividers ####

  # decide whether to draw x series dividers
  xSeriesDividerLocations <- c()

  if (xSeriesDivider) {

    # between first and second series, if present
    if (series2Length > 0) {
      xSeriesDividerLocations <- c(xSeriesDividerLocations, (bars[nrow(bars), series1Length] + bars[1, series1Length + 1]) / 2)
    }

    # between second and third series, if present
    if (series3Length > 0) {
      xSeriesDividerLocations <- c(xSeriesDividerLocations,   (bars[nrow(bars), series1Length + series2Length] + bars[1, series1Length + series2Length + 1]) / 2)
    }

  }


  #### return ####

  return(list(type = 'bar',
    # tick locations
    xMajorTickLocations = xMajorTickLocations, xMinorTickLocations = xMinorTickLocations,
    # labels
    dayLabels = dayLabels, monthLabels = monthLabels, yearMonthLabels = yearMonthLabels,
    quarterLabels = quarterLabels,  yearLabels = yearLabels,
    # label locations
    dayLabelsLocations = dayLabelsLocations, monthLabelsLocations = monthLabelsLocations, yearMonthLabelsLocations = yearMonthLabelsLocations,
    quarterLabelsLocations = quarterLabelsLocations, yearLabelsLocations = yearLabelsLocations,
    # bar tick locations
    xBarTicksLocations = xBarTicksLocations,
    # series divider
    xSeriesDividerLocations = xSeriesDividerLocations
  ))

}

#' @title defineXAxisDate
#' @description This defines the X-axis for charts with a date x-axis.
#' @export
defineXAxisDate <- function(df = plot.data,
                            xMajorTickFreq,
                            xMinorTickFreq,
                            xLabelCenter = T) {

  ### define x axis ticks ###

  # major ticks
  if (!is.na(xMajorTickFreq)) {
    xMajorTickLocations <- seq(floor_date(min(df$date),xMajorTickFreq),
                               floor_date(max(df$date),xMajorTickFreq),
                               by = xMajorTickFreq)
  } else {
    xMajorTickLocations <- NULL
  }


  # minor ticks
  if (!is.na(xMinorTickFreq)) {
    xMinorTickLocations <- seq(floor_date(min(df$date), xMinorTickFreq), floor_date(max(df$date), xMinorTickFreq), by = xMinorTickFreq)
  } else {
    xMinorTickLocations <- NULL
  }

  ### define x axis labels ###

  # if xLabelCenter then adjust the frac of month placement
  frac_placement <- 0.5*xLabelCenter

  # daily placement
  daily.label.placement = unique(df$date)
  daily.labelz <- format(unique(daily.label.placement), "%d")

  # monthly placement
  first.month=as.Date(as.yearmon(min(df$date,na.rm=TRUE)), frac = frac_placement)
  last.month=as.Date(as.yearmon(max(df$date,na.rm=TRUE)), frac = frac_placement)
  month.label.placement = seq(first.month, last.month, by = "month")

  # monthly labels (regular and daily)
  month.labelz = opt$tealbook.months[month(month.label.placement)]
  month.labelz.daily = format.Date(month.label.placement, "%b %Y")

  # quarterly placement
  first.quarter=as.Date(as.yearqtr(min(df$date,na.rm=TRUE)), frac = frac_placement)
  last.quarter=as.Date(as.yearqtr(max(df$date,na.rm=TRUE)), frac = frac_placement)
  quarter.label.placement = seq(first.quarter, last.quarter %m+% months(1), by = "quarter")

  # quarterly labels
  quarter.labelz = opt$tealbook.quarters[quarter(quarter.label.placement)]

  # yearly labels
  year.labelz = seq(min(year(df$date)), max(year(df$date)), by = 1) 

  # yearly placement
  first.year <- as.Date(ISOdate(year(min(df$date, na.rm =T)), 1, 1)) %m+% months(12*frac_placement)
  last.year <- as.Date(ISOdate(year(max(df$date, na.rm =T)), 1, 1)) %m+% months(12*frac_placement)
  year.label.placement = seq(first.year, last.year, by = "year")


  return(list(type = 'date',
              # tick locations
              xMajorTickLocations = xMajorTickLocations, xMinorTickLocations = xMinorTickLocations,
              # labels
              dayLabels = daily.labelz, monthLabels = month.labelz, yearMonthLabels = month.labelz.daily,
              quarterLabels = quarter.labelz,  yearLabels = year.labelz,
              # label locations
              dayLabelsLocations = daily.label.placement, monthLabelsLocations = month.label.placement, yearMonthLabelsLocations = month.label.placement,
              quarterLabelsLocations = quarter.label.placement, yearLabelsLocations = year.label.placement
              ))

}

#' @title defineXAxisNumeric
#' @description This defines the X-axis for charts with a numeric x-axis
#' @export
defineXAxisNumeric <- function(df = plot.data,
                               xMajorTickFreq,
                               xMinorTickFreq,
                               roundDig,
                               seriesX
                               ) {
  start_loc <- min(df[[seriesX]], na.rm =T)
  end_loc <- max(df[[seriesX]], na.rm =T)

  ## if they don't give a major tick frequency then just put 10 major ticks
  if (!is.na(xMajorTickFreq) & xMajorTickFreq == 'year') {
    # Make a guess at the tick frequency
    xMajorTickFreq <- round((end_loc - start_loc)/10,0)

    # if we're talking about smaller numbers we scale the rounding
    if ((end_loc - start_loc)<10){
      xMajorTickFreq <- round((end_loc - start_loc)/10,1)
    }
  }

  ## default for Minor ticks just place them in between any major ticks
  if (!is.na(xMinorTickFreq) & xMinorTickFreq == 'month') {
    # Make a guess at the tick frequency
    xMinorTickFreq <- xMajorTickFreq/2
  }



  ### define x axis ticks ###
  if (!is.na(xMajorTickFreq)) {
    xMajorTickLocations <- round(seq(start_loc,end_loc, by = xMajorTickFreq),roundDig)
  } else {
    xMajorTickLocations <- NULL
  }

  if (!is.na(xMinorTickFreq)) {
    xMinorTickLocations <- round(seq(start_loc,end_loc, by = xMinorTickFreq),roundDig)
  } else {
    xMinorTickLocations <- NULL
  }

  return(list(type = 'numeric',
              # tick locations and labels and label locations (label locations go to major tick locations and labels are major ticks)
              xMajorTickLocations = xMajorTickLocations,
              xMinorTickLocations = xMinorTickLocations
  ))
}

defineXAxisLabel <- function() {}


#' @title drawXAxis
#' @description This draws the X-axis using the output from the defineXAxis functions.
#' @export
drawXAxis <- function(xAxisFontSize = NULL,
                      xLabelFreq,
                      xTicksBelowAxis,
                      xTicksLabels) {

  #### draw x axis date ticks ####

  # Draw major x ticks with no labels
  if (!is.null(xTicksLabels$xMajorTickLocations)) {
    axis(side = 1,
         at = xTicksLabels$xMajorTickLocations,
         tck = (opt$tick.length+.015) * ifelse(xTicksBelowAxis, -1, 1),
         cex.axis = xAxisFontSize,
         # las = 2,
         labels=FALSE,
         lwd.ticks=1.25)
  }

  # Draw minor x ticks with no labels
  if (!is.null(xTicksLabels$xMinorTickLocations)) {
    axis(side = 1, at = xTicksLabels$xMinorTickLocations,
         tck = opt$tick.length * ifelse(xTicksBelowAxis, -1, 1),
         cex.axis = xAxisFontSize,
         # las = 1,
         labels=FALSE)
  }


  #### draw x axis date labels ####

  # note, we add these in reverse order so later dates will not be cut off

  # adjust down if we have ticks below the x axis
  labelLine1 <- -1 + .225 * xTicksBelowAxis
  labelLine2 <- -.25 + .2 * xTicksBelowAxis

  if (xTicksLabels$type == 'numeric') {
    axis(side = 1, at = xTicksLabels$xMajorTickLocations, tick=FALSE, cex.axis = xAxisFontSize, las = 1,labels=xTicksLabels$xMajorTickLocation,line=-1)


  } else if (xLabelFreq == "day"){

    #Add daily labels
    axis(side = 1, at = rev(xTicksLabels$dayLabelsLocations), tick=FALSE, cex.axis = xAxisFontSize,
         las = 1,labels = rev(xTicksLabels$dayLabels),line=labelLine1)
    # Add month+year labels
    axis(side = 1, at = rev(xTicksLabels$yearMonthLabelsLocations), tick=FALSE, cex.axis = xAxisFontSize, las = 1,
         labels = rev(xTicksLabels$yearMonthLabels),line=labelLine2)

  } else if(xLabelFreq == "year"){

    # add year labels
    axis(side = 1, at = rev(xTicksLabels$yearLabelsLocations), tick=FALSE, cex.axis = xAxisFontSize, las = 1,
         labels = rev(xTicksLabels$yearLabels),line=labelLine1)

  } else if(xLabelFreq == "month"){

    #Add year labels
    axis(side = 1, at = rev(xTicksLabels$yearLabelsLocations), tick=FALSE, cex.axis = xAxisFontSize, las = 1,
         labels = rev(xTicksLabels$yearLabels),line=labelLine2)
    #Add month labels
    axis(side = 1, at = rev(xTicksLabels$monthLabelsLocations), tick=FALSE, cex.axis = xAxisFontSize, las = 1,
         labels = rev(xTicksLabels$monthLabels),line=labelLine1)

  } else if(xLabelFreq == "quarter"){

    #Add year labels
    axis(side = 1, at = rev(xTicksLabels$yearLabelsLocations), tick=FALSE, cex.axis = xAxisFontSize, las = 1,
         labels = rev(xTicksLabels$yearLabels),line=labelLine2)
    #Add quarter labels
    axis(side = 1, at = rev(xTicksLabels$quarterLabelsLocations), tick=FALSE, cex.axis = xAxisFontSize, las = 1,
         labels = rev(xTicksLabels$quarterLabels),line=labelLine1)

  }


  #### draw x axis series divider ####

  if (!is.null(xTicksLabels$xSeriesDividerLocations)) {

    axis(side = 1,
         at = xTicksLabels$xSeriesDividerLocations,
         # tck = 2 * (opt$tick.length),
         tck = .1,
         cex.axis = opt$axis.cex,
         labels = F,
         lwd.ticks = 2
    )

  }



  #### draw bar separators (bar charts only) ####

  if (xTicksLabels$type == 'bar') {

    axis(side = 1,
         # this is how we will determine which ticks to turn on
         at = xTicksLabels$xBarTicksLocations,
         tck = opt$tick.length + .01,
         cex.axis = opt$axis.cex,
         las = 2,
         labels = F,
         lwd.ticks = 1)

  }

}


#' @title drawYAxis
#' @description This draws the Y-axis.
#' @export
drawYAxis <- function(df = plot.data,
                      dfLeft = NULL, #plot.data.line in most cases
                      yMin,
                      yMax,
                      yStep,
                      yMinLeft,
                      yMaxLeft,
                      yStepLeft,
                      yAxisFontSize,
                      hadj,
                      axisColorRight,
                      axisColorLeft,
                      roundDig,
                      roundDigLeft,
                      chartParametersRight,
                      chartParametersLeft) {

  # right axis
  par('usr' = chartParametersRight)
  axis(side = 4,
       at = round(seq(yMin, yMax, by = yStep), roundDig),
       tck = opt$tick.length,
       cex.axis = yAxisFontSize,
       las = 2,
       hadj = hadj,
       col = axisColorRight,
       col.axis = axisColorRight,
       col.ticks = axisColorRight)

  # left axis: if no left data, copy formatting from right axis.
  # otherwise use left parameters and scaling
  if (is.null(dfLeft)) {

    par('usr' = chartParametersRight)
    axis(side = 2,
         at = round(seq(yMin,yMax, by = yStep),roundDig),
         tck = opt$tick.length,
         cex.axis = yAxisFontSize,
         las = 2,
         labels = F,
         col = axisColorLeft,
         col.axis = axisColorLeft,
         col.ticks = axisColorLeft)

  } else {

    par('usr' = chartParametersLeft)
    axis(side = 2,
         at = round(seq(yMinLeft,yMaxLeft, by = yStepLeft),roundDigLeft),
         tck = opt$tick.length,
         cex.axis = yAxisFontSize,
         las = 2,
         labels = T,
         col = axisColorLeft,
         col.axis = axisColorLeft,
         col.ticks = axisColorLeft)
  }

}


#' @title placeX
#' @description Get the x coordinate that is locX% across the chart.
#' @export
placeX <- function(locX = .2) {par('usr')[1] + locX * (par('usr')[2] - par('usr')[1])}

#' @title placeY
#' @description Get the y coordinate that is locY% up the chart.
#' @export
placeY <- function(locY = .97) {par('usr')[3] + locY * (par('usr')[4] - par('usr')[3])}

getDynamicDates <- function(df = plot.data, df2 = plot.data2, df3 = plot.data3) {

  # guess periodicity of each data frame
  periodicity1 <- xts::periodicity(df$date)$scale %>% stringr::str_replace('ily', 'yly') %>% stringr::str_remove('ly$')
  periodicity2 <- xts::periodicity(df2$date)$scale %>% stringr::str_replace('ily', 'yly') %>% stringr::str_remove('ly$')
  periodicity3 <- xts::periodicity(df3$date)$scale %>% stringr::str_replace('ily', 'yly') %>% stringr::str_remove('ly$')

  # list of start and end dates of each period
  startDateList1 <- floor_date(df$date, periodicity1)
  endDateList1 <- ceiling_date(df$date, periodicity1) - 1
  startDateList2 <- floor_date(df2$date, periodicity2)
  endDateList2 <- ceiling_date(df2$date, periodicity2) - 1
  startDateList3 <- floor_date(df3$date, periodicity3)
  endDateList3 <- ceiling_date(df3$date, periodicity3) - 1

  # never show temporary values for years and quarters
  if (periodicity1 == 'year') {
    endDateList1 <- endDateList1[endDateList1 < floor_date(Sys.Date(), 'year')]
  }

  if (periodicity2 == 'year') {
    endDateList2 <- endDateList2[endDateList2 < floor_date(Sys.Date(), 'year')]
  }

  if (periodicity1 == 'quarter') {
    endDateList1 <- endDateList1[endDateList1 < floor_date(Sys.Date(), 'quarter')]
  }

  if (periodicity2 == 'quarter') {
    endDateList2 <- endDateList2[endDateList2 < floor_date(Sys.Date(), 'quarter')]
  }

  # pick start and end dates so they line up continuously
  endDate <- max(endDateList1)

  if (!is.null(df2)) {
    startDate2 <- min(startDateList2[startDateList2 > max(endDateList1)], na.rm = T)
    endDate2 <- max(endDateList2[startDateList2 > max(endDateList1)], na.rm = T)
 } else {
    startDate2 <- NULL
    endDate2 <- NULL
  }

  if (!is.null(df3)) {
    startDate3 <- min(startDateList3[startDateList3 > max(endDateList2)], na.rm = T)
    # startDate3 <- min(startDateList3[startDateList3 > endDate2], na.rm = T)
  } else {
    startDate3 <- NULL
  }
  
  # return
  return(list('endDate' = endDate, 'startDate2' = startDate2, 'endDate2' = endDate2, 'startDate3' = startDate3))

}


#### PRESETS ####

#' @title custom_preset
#' @description This allows you to define a custom preset of parameters for your exhibits.
#' @export
custom_preset <- function(...) {
  list(...)
}

