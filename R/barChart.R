#' @title Bar Chart
#' @description This creates a bar chart.
#' @export
barChart <- function(
  stacked = F,
  famePath=NULL,
  famePath2 = famePath,
  famePath3 = famePath,
  famePathLine = famePath,
  famePathLine2 = famePathLine,
  famePathLine3 = famePathLine,
  dataFrame=NULL,
  dataFrame2=NULL,
  dataFrame3=NULL,
  dataFrameLeft = dataFrame,
  dataFrameLeft2 = dataFrame2,
  dataFrameLeft3 = dataFrame3,
  dataFrameLine = dataFrame, #!
  dataFrameLine2 = dataFrame2, #!
  dataFrameLine3 = dataFrame3, #!
  seriesLine2 = NULL,
  seriesLine3 = NULL,
  chartTitle="",
  chartTitleFontSize = opt$paneltitlecex,
  series=NULL,
  series2=NULL,
  series3=NULL,
  yMin=NULL,
  yMax=NULL,
  yStep=NULL,
  seriesScale=1,
  unitsLabel="",
  axisColorRight = "black",
  seriesLine=NULL, #!
  yMinLeft=NULL,
  yMaxLeft=NULL,
  yStepLeft=NULL,
  unitsLabelLeft="",
  labelFontSize = opt$legend.cex,
  seriesScaleLeft=1,
  seriesScaleLine = 1,
  seriesScale2 = 1,
  seriesScale3 = 1,
  seriesScaleLine2 = 1,
  seriesScaleLine3 = 1,
  axisColorLeft = "black",
  xAxisFontSize = opt$axis.cex,
  yAxisFontSize = opt$axis.cex,
  colorBandSeries = NA, # this is the series that is 'banded'
  seriesBandUpper = NA, # band itself
  seriesBandLower = NA, # band itself
  borderBand = F,
  startDate=NULL,
  endDate=NULL,
  startDate2 = NULL,
  endDate2 = NULL,
  startDate3 = NULL,
  endDate3 = NULL,
  xMajorTickFreq=NA,
  xMinorTickFreq=NA,
  xLabelFreq="year",
  extraLimX = 0,
  legend=TRUE,
  legendNames=NULL,
  legendCol=1,
  legendColPlacement=NULL,
  legendLocationX=0.05,
  legendLocationY=0.97,
  legendFontSize=opt$key.cex,
  frequencyLabel="",
  footNote="",
  footNotePlacement=NULL,
  footNoteFontSize = opt$foot.cex,
  lineType= NULL,
  lineWidth= NULL,
  colors=NULL,
  colorsLine = NULL,
  dynamicColors=FALSE,
  lineAtZero=T,
  printLastValue=TRUE,
  lastValueFreq=NA,
  lastValue = NULL,
  recessions=FALSE,
  hadj=opt$yaxis.pos,
  interp=FALSE,
  roundDig=2,
  roundDigLeft=2,
  customShading =FALSE,
  shadeStart="",
  shadeEnd = "",
  palette = NULL,
  scheme = NULL,
  xBarTickAdj = 0.5,
  xBarTicksSeries1 = T,
  xBarTicksSeries2 = T,
  xBarTicksSeries3 = T,
  xTicksBelowAxis = T,
  barSpace = T,
  barBorder = T,
  barBorderColor = 'black',
  barBorderWidth = 1,
  density = NULL,
  angle = 45,
  xSeriesDivider = T,
  series2LabelFreq = NA,
  series3LabelFreq = NA,
  barLabelFontSize = 0.5,
  monthLabelLetters = F,
  lastValuePrelim = F,
  lastValueLocationX = .9,
  lastValueLocationY = .9,
  dynamicDates = F,
  fullYearAsQuarters = T,
  # WHAT ABOUT THIS
  PRESET = list(),
  # DEPRECATED PARAMETERS
  dataType = NULL,
  dataTypeLine = NULL,
  series2MonthLabel = NULL,
  series2QuarterLabel = NULL,
  series3MonthLabel = NULL,
  series3QuarterLabel = NULL,
  tickMarksSeries1 = NULL,
  tickMarksSeries2 = NULL,
  tickMarksSeries3 = NULL,
  printLastMonthLabel = NULL,
  printLastMonthDayLabel = NULL,
  printLastQuarterLabel = NULL,
  lastValueLabelVerticalShift = NULL,
  lastValueLabelHorizontalShift = NULL,
  xLabelStop = NULL,
  xLabelYearUpperTicks = NULL,
  xLabelYearLowerTicks = NULL,
  xLabelShift = NULL # eventually want to reincorporate this
) {

  #### 0. deprecated and preset parameters ####

  if (!is.null(series2MonthLabel)) {
    if (series2MonthLabel) {
      series2LabelFreq <- 'month'
    }
  }

  if (!is.null(series2QuarterLabel)) {
    if (series2QuarterLabel) {
      series2LabelFreq <- 'quarter'
    }
  }

  if (!is.null(series3MonthLabel)) {
    if (series3MonthLabel) {
      series3LabelFreq <- 'month'
    }
  }

  if (!is.null(series3QuarterLabel)) {
    if (series3QuarterLabel) {
      series3LabelFreq <- 'quarter'
    }
  }

  if (!is.null(tickMarksSeries1)) {
    xBarTicksSeries1 <- tickMarksSeries1
  }

  if (!is.null(tickMarksSeries2)) {
    xBarTicksSeries2 <- tickMarksSeries2
  }

  if (!is.null(tickMarksSeries3)) {
    xBarTicksSeries3 <- tickMarksSeries3
  }

  if (!is.null(printLastMonthDayLabel)) {
    if (printLastMonthDayLabel) {
      lastValueFreq <- 'day'
    }
  }

  if (!is.null(printLastMonthLabel)) {
    if (printLastMonthLabel) {
      lastValueFreq <- 'month'
    }
  }

  if (!is.null(printLastQuarterLabel)) {
    if (printLastQuarterLabel) {
      lastValueFreq <- 'quarter'
    }
  }

  if (!is.null(lastValueLabelVerticalShift)) {
    lastValueLocationY <- lastValueLabelVerticalShift
  }

  if (!is.null(lastValueLabelHorizontalShift)) {
    lastValueLocationX <- lastValueLabelHorizontalShift
  }

  deprecated_pars <- (list('dataType', 'dataTypeLine', 'series2MonthLabel', 'series2QuarterLabel', 'series3MonthLabel', 'series3QuarterLabel',
                           'tickMarksSeries1', 'tickMarksSeries2', 'tickMarksSeries3', 'printLastMonthDayLabel',
                           'printLastMonthLabel', 'printLastQuarterLabel', 'lastValueLabelVerticalShift', 'lastValueLabelHorizontalShift',
                           'xLabelYearUpperTicks', 'xLabelYearLowerTicks', 'xLabelStop', 'xLabelShift'))

  if (any(sapply(deprecated_pars, function(x) !is.null(get(x))))) {

    warning(paste("Deprecated parameters provided:", paste0(deprecated_pars[sapply(deprecated_pars, function(x) !is.null(get(x)))], collapse = ', ')),
            '. See fiafunctions version 2 documentation for appropriate replacements.')

  }

  # presets.  These will override the default values of certain variables, unless overridden
  # themselves by variables explicitly specified by the user.
  if (length(PRESET) > 0) {

    passed <- names(as.list(match.call())[-1])

    for (i in 1:length(PRESET)) {
      if (!names(PRESET)[i] %in% passed) {
        assign(names(PRESET)[i], eval(PRESET[[i]]))
      }
    }
  }


  #### 1. create data subsets ####

  plot.data <- getCopy(dataFrame = dataFrame,
                       series = series,
                       famePath = famePath,
                       startDate= startDate,
                       endDate= endDate,
                       seriesScale = seriesScale,
                       interp = F)

  plot.data.line <- getCopy(dataFrame = dataFrameLine,
                            series = seriesLine,
                            famePath = famePathLine,
                            startDate= startDate,
                            endDate= endDate,
                            seriesScale = seriesScaleLine,
                            interp = F)

  plot.data2 <- getCopy(dataFrame = dataFrame2,
                        series = series2,
                        famePath = famePath2,
                        startDate= startDate2,
                        endDate= endDate2,
                        seriesScale = seriesScale2,
                        interp = F)

  plot.data.line2 <- getCopy(dataFrame = dataFrameLine2,
                             series = seriesLine2,
                             famePath = famePathLine2,
                             startDate= startDate2,
                             endDate= endDate2,
                             seriesScale = seriesScaleLine2,
                             interp = F)

  plot.data3 <- getCopy(dataFrame = dataFrame3,
                        series = series3,
                        famePath = famePath3,
                        startDate= startDate3,
                        endDate= endDate3,
                        seriesScale = seriesScale3,
                        interp = F)

  plot.data.line3 <- getCopy(dataFrame = dataFrameLine3,
                             series = seriesLine3,
                             famePath = famePathLine3,
                             startDate= startDate3,
                             endDate= endDate3,
                             seriesScale = seriesScaleLine3,
                             interp = F)
  # dynamic dates
  if (dynamicDates) {
    date_list_dynamic <- getDynamicDates(df = plot.data, df2 = plot.data2, df3 = plot.data3)

    # redo our subsetting of the dates
    plot.data <- plot.data[plot.data$date <= date_list_dynamic$endDate, ]
    plot.data2 <- plot.data2[plot.data2$date >= date_list_dynamic$startDate2 & plot.data2$date <= date_list_dynamic$endDate2, ]
    plot.data3 <- plot.data3[plot.data3$date >= date_list_dynamic$startDate3, ]

    plot.data.line <- plot.data.line[plot.data.line$date <= date_list_dynamic$endDate, ]
    plot.data.line2 <- plot.data.line2[plot.data.line2$date >= date_list_dynamic$startDate2 & plot.data.line2$date <= date_list_dynamic$endDate2, ]
    plot.data.line3 <- plot.data.line3[plot.data.line3$date >= date_list_dynamic$startDate3, ]

  }

  # make sure the line data matches up with the bar data
  if (!is.null(plot.data.line2)) {
    if (length(setdiff(plot.data.line2$date, plot.data2$date)) > 0) {
      stop("Dates in second line series do not match dates in second bar series.")
    } else {
      plot.data.line2 <- merge(data.frame(date = plot.data2[, 'date']), plot.data.line2, by = 'date', all.x = T)
    }
  }

  if (!is.null(plot.data.line3)) {
    if (length(setdiff(plot.data.line3$date, plot.data3$date)) > 0) {
      stop("Dates in third line series do not match dates in third bar series.")
    } else {
      plot.data.line3 <- merge(data.frame(date = plot.data3[, 'date']), plot.data.line3, by = 'date', all.x = T)
    }
  }

  # make sure series are consistent (lengths and names)
  if (length(series2) > 0 & length(series2) != length(series)) {
    stop("series and series2 must be the same length.")
  }

  if (length(series3) > 0 & length(series3) != length(series)) {
    stop("series and series3 must be the same length.")
  }

  if (length(seriesLine2) > 0 & length(seriesLine2) != length(seriesLine)) {
    stop("seriesLine and seriesLine2 must be the same length.")
  }

  if (length(seriesLine3) > 0 & length(seriesLine3) != length(seriesLine)) {
    stop("seriesLine and seriesLine3 must be the same length.")
  }

  if (!is.null(plot.data2)) {
    names(plot.data2) <- names(plot.data)
  }

  if (!is.null(plot.data.line2)) {
    names(plot.data.line2) <- names(plot.data.line)
  }

  if (!is.null(plot.data3)) {
    names(plot.data3) <- names(plot.data)
  }

  if (!is.null(plot.data.line3)) {
    names(plot.data.line3) <- names(plot.data.line)
  }

  # combine into full series
  plot.data.full <- rbind(plot.data, plot.data2, plot.data3)
  plot.data.line.full <- rbind(plot.data.line, plot.data.line2, plot.data.line3)

  # split bar data into positive and negative
  plot.data.positive <- plot.data.full
  plot.data.negative <- plot.data.full
  for (s in series) {
    plot.data.positive[, s] <- pmax(plot.data.positive[, s], 0)
    plot.data.negative[, s] <- pmin(plot.data.negative[, s], 0)
  }

  plot.data.full$stackedBarPositiveHeight <- rowSums(data.frame(plot.data.positive[, series]))
  plot.data.full$stackedBarNegativeHeight <- rowSums(data.frame(plot.data.negative[, series]))

  # time length of each series
  series1Length <- nrow(plot.data)
  series2Length <- ifelse(is.null(plot.data2), 0, nrow(plot.data2))
  series3Length <- ifelse(is.null(plot.data3), 0, nrow(plot.data3))


  #### 2. define chart limits ####

  yLims <- setYLims(df = plot.data.full,
                    dfLeft = plot.data.line.full,
                    series = if (stacked) c('stackedBarPositiveHeight', 'stackedBarNegativeHeight') else series,
                    seriesLeft = seriesLine,
                    yMax = yMax,
                    yMin = yMin,
                    yStep = yStep,
                    yMaxLeft = yMaxLeft,
                    yMinLeft = yMinLeft,
                    yStepLeft = yStepLeft

  ) ## Returns a list with all yLimits and steps
  yMin <- yLims$yMin
  yMax <- yLims$yMax
  yStep <- yLims$yStep
  yMinLeft <- yLims$yMinLeft
  yMaxLeft <- yLims$yMaxLeft
  yStepLeft <- yLims$yStepLeft

  #### 3. pick colors ####

  colors <- pickColors(series = series,
                       seriesLeft = seriesLine,
                       colors = colors,
                       dynamicColors = dynamicColors,
                       palette = palette,
                       scheme = scheme)

  # if line colors are specified, overwrite
  if (!is.null(colorsLine)) {
    colors[seq(from = length(series) + 1, by = 1, length.out = length(colorsLine))] <- colorsLine
  }

  # if insufficient number of colors given, holler
  if ((length(colors) + length(colorsLine)) < (length(series) + length(seriesLine))) {
    stop("An insufficient number of colors was specified.")
  }


  #### 4. type-specific formatting ####

  # line type and width for seriesLine
  lineFormats <- lineFormatting(lineType = lineType,
                                lineWidth = lineWidth,
                                series = seriesLine,
  )
  lineType <- lineFormats$lineType
  lineWidth <- lineFormats$lineWidth


  #### 5. plot data ####

  # plot right-hand data (bars)
  savelwd <- par('lwd')
  par('lwd' = barBorderWidth)

  if (!stacked) { # FOR BAR CHART

    bars <- barplot(t(plot.data.full[,c(series)]),
                    col = colors[1:length(series)],
                    yaxt = 'n',
                    bty = 'u',
                    ylim = c(yMin, yMax),
                    width =c(rep(1,length(plot.data$date)), rep(.6,length(plot.data2$date)),
                             rep(.4,length(plot.data3$date))),
                    xaxt="n",
                    tck = 0,
                    main="",
                    beside=TRUE,
                    axes=FALSE,
                    border = ifelse(barBorder, barBorderColor, F),
                    space = if(barSpace) c(0, 1) else c(0, 0),
                    density = density,
                    angle = angle
    )

  } else { # FOR STACKED BAR CHART

    # plot positive bars
    bars.positive <- barplot(t(plot.data.positive[,c(series)]),
                             col = colors[1:length(series)],
                             yaxt = 'n',
                             bty = 'u',
                             ylim = c(yMin, yMax),
                             width =c(rep(1,length(plot.data$date)), rep(.6,length(plot.data2$date)),
                                      rep(.4,length(plot.data3$date))),
                             xaxt="n",
                             tck = 0,
                             main="",
                             beside=F,
                             axes=FALSE,
                             border = ifelse(barBorder, barBorderColor, F),
                             space = ifelse(barSpace, 0.2, 0),
                             density = density,
                             angle = angle
    )

    # plot negative bars
    par(new = T)
    bars.negative <- barplot(t(plot.data.negative[,c(series)]),
                             col = colors[1:length(series)],
                             yaxt = 'n',
                             bty = 'u',
                             ylim = c(yMin, yMax),
                             width =c(rep(1,length(plot.data$date)), rep(.6,length(plot.data2$date)),
                                      rep(.4,length(plot.data3$date))),
                             xaxt="n",
                             tck = 0,
                             main="",
                             beside=F,
                             axes=FALSE,
                             border = ifelse(barBorder, barBorderColor, F),
                             space = ifelse(barSpace, 0.2, 0),
                             density = density,
                             angle = angle
    )

    # save bar positions as matrix
    bars <- matrix(bars.positive, nrow = 1)

  }

  par('lwd' = savelwd)

  # save chart parameters
  chartParametersRight <- par('usr')
  chartParametersLeft <- c(par('usr')[1:2], yMinLeft, yMaxLeft)

  # plot line data
  if (!is.null(seriesLine)) {
    par('usr' = chartParametersLeft)

    for (i in 1:length(seriesLine)) {
      lines(x = colMeans(bars)[match(plot.data.line.full$date, plot.data.full$date)],
            y = plot.data.line.full[, i + 1],
            col = colors[length(series) + i],
            lwd = lineWidth[i],
            lty = lineType[i])
    }
  }


  #### 6. flourishes ####

  flourish(
           lineAtZero = lineAtZero,
           )


  #### 7. labels ####


  chartLabels(chartType = ifelse(!stacked, 'bar', 'stackedBar'),
              df = plot.data.full,
              chartTitle = chartTitle,
              chartTitleFontSize = chartTitleFontSize,
              series = series,
              seriesLeft = seriesLine,
              legend = legend,
              lineType = lineType,
              lineWidth = lineWidth,
              legendCol = legendCol,
              legendLocationX = legendLocationX,
              legendLocationY = legendLocationY,
              legendFontSize = legendFontSize,
              legendColPlacement = legendColPlacement,
              legendNames = legendNames,
              footNote = footNote,
              footNoteFontSize = footNoteFontSize,
              footNotePlacement = footNotePlacement,
              xLabelFreq = xLabelFreq,
              xTicksBelowAxis = xTicksBelowAxis,
              unitsLabel = unitsLabel,
              colors = colors,
              unitsLabelLeft = unitsLabelLeft,
              frequencyLabel = frequencyLabel,
              axisColorRight = axisColorRight,
              axisColorLeft = axisColorLeft,
              printLastValue = printLastValue,
              lastValueFreq = lastValueFreq,
              lastValue = lastValue,
              lastValueLocationX = lastValueLocationX,
              lastValueLocationY = lastValueLocationY,
              lastValuePrelim = lastValuePrelim,
              yMax = yMax,
              yMin = yMin,
              yStep = yStep,
              yMaxLeft = yMaxLeft,
              yMinLeft = yMinLeft,
              yStepLeft = yStepLeft,
              labelFontSize = labelFontSize
  )

  #### 8. axes ####

  plotHookBox()

  drawYAxis(df = plot.data,
            dfLeft = plot.data.line,
            yMin = yMin,
            yMax = yMax,
            yStep = yStep,
            yMinLeft = yMinLeft,
            yMaxLeft = yMaxLeft,
            yStepLeft = yStepLeft,
            yAxisFontSize = yAxisFontSize,
            hadj = hadj,
            axisColorRight = axisColorRight,
            axisColorLeft = axisColorLeft,
            roundDig = roundDig,
            roundDigLeft = roundDigLeft,
            chartParametersRight = chartParametersRight,
            chartParametersLeft = chartParametersLeft)

  # an object that contains everything you could ever dream of having
  # to do with tick marks and labeling
  xTicksLabels <- defineXAxisBar(df_full = plot.data.full, series1Length = series1Length, series2Length = series2Length,
                                 series3Length = series3Length, bars = bars,
                                 xBarTickAdj = xBarTickAdj, xMajorTickFreq = xMajorTickFreq, xMinorTickFreq = xMinorTickFreq,
                                 xBarTicksSeries1 = xBarTicksSeries1, xBarTicksSeries2 = xBarTicksSeries2, xBarTicksSeries3 = xBarTicksSeries3,
                                 xSeriesDivider = xSeriesDivider)

  drawXAxis(xAxisFontSize = xAxisFontSize,
            xLabelFreq = xLabelFreq,
            xTicksLabels = xTicksLabels,
            xTicksBelowAxis = xTicksBelowAxis)


  #### 9. extra ####

  barChartLabels(bars = bars, df = plot.data.full, chartType = ifelse(!stacked, 'bar', 'stackedBar'), series = series,
                 series1Length = series1Length, series2Length = series2Length, series3Length = series3Length,
                 series2LabelFreq = series2LabelFreq, series3LabelFreq = series3LabelFreq,barLabelFontSize = barLabelFontSize, monthLabelLetters =monthLabelLetters,  lastValuePrelim = lastValuePrelim,
                 chartParametersRight = chartParametersRight)



}
