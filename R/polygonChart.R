#' @title Polygon Chart
#' @description This creates a polygon chart.
#' @export
#' 
#' @param dataFrame 
#' @param dataFrameLine 
#' @param famePath 
#' @param series 
#' @param seriesLine 
#' @param interp Boolean variable. Default is FALSE. If TRUE, replaces NA by interpolation using zoo::na.approx().
#' @param chartTitle Character variable.
#' @param chartTitleFontSize Default is 0.85.
#' @param seriesX Character vector. Default is c("date").
#' @param seriesXLabel Character variable. Typically used if seriesX != c("date").
#' @param startDate If seriesX == c("date"), character variable. If is.numeric(seriesX), numberic variable.
#' @param endDate 
#' @param xMajorTickFreq If seriesX == c("date"), character variable (eg. "year", "quarter", "month", "day").
#' @param xMinorTickFreq 
#' @param xLabelFreq 
#' @param xLabelCenter Boolean variable. Default is TRUE.
#' @param xTicksBelowAxis Boolean variable. Default is FALSE.
#' @param extraLimX Numeric variable. Adjusts x-axis limits.
#' @param yMin 
#' @param yMax 
#' @param yStep 
#' @param seriesScale 
#' @param unitsLabel Character variable.
#' @param hadj Numeric variable. Default is 0.5. Justification for all labels parallel to reading direction.
#' @param roundDig Integer indicating the number of decimal places to be used. Default is 2.
#' @param yMinLeft 
#' @param yMaxLeft 
#' @param yStepLeft 
#' @param seriesScaleLine 
#' @param unitsLabelLeft 
#' @param roundDigLeft Integer indicating the number of decimal places to be used. Default is 2.
#' @param axisColorRight Character variable. Default is "black".
#' @param axisColorLeft 
#' @param labelFontSize Default is 0.7.
#' @param xAxisFontSize Default is 0.65.
#' @param yAxisFontSize Default is 0.65.
#' @param legend Boolean variable. Default is TRUE.
#' @param legendNames Character vector.
#' @param legendCol Integer. Default is 1. Number of columns in legend.
#' @param legendColPlacement Numeric. Must be >= 0. Allows the columns to have better widths.
#' @param legendLocationX Numeric. Horizontal location of legend as percent of chart area. Default is 0.05.
#' @param legendLocationY Numeric. Vertical location of legend as percent of chart area. Default is 0.97.
#' @param legendFontSize Default is 0.75.
#' @param footNote Character vector. c("Note:  Best practice to make the footNote all one vector (for indentation), to double space after 'note' and 'source', and to have 'note' first and then 'source.'\ n Source:  PolicyPlot charting dos and donts.")
#' @param footNotePlacement Numeric. on which margin line, starting at 0 counting outwards.
#' @param footNoteFontSize Default is 0.55.
#' @param printLastValue Boolean. Default is TRUE. 
#' @param lastValueFreq If seriesX == c("date"), character variable (eg. "year", "quarter", "month", "day").
#' @param lastValue Specify lastValue
#' @param lastValueLocationX Numeric. Horizontal location of last value as percent of chart area. Default is 0.9.
#' @param lastValueLocationY Numeric. Vertical location of last value as percent of chart area. Default is 0.9.
#' @param lastValuePrelim Boolean. Default is FALSE. If TRUE, adds * after last value.
#' @param frequencyLabel Character variable (eg. "year", "quarter", "month", "day").
#' @param colors Character vector. Defaults to opt$colors.
#' @param colorsLine 
#' @param palette Character variable (eg. "s", "d", "q" / "sequential", "diverging", "qualitative").
#' @param scheme Character variable. See pickColors() in helper_colors.R for complete list.
#' @param dynamicColors Boolean. Default is FALSE. If TRUE, uses PaulTol color scheme.
#' @param lineAtZero Boolean. Default is FALSE. If TRUE, adds horizontal, solid, black line at y = 0.
#' @param recessions Boolean. Default is FALSE. If TRUE, shades recessions using fiafunctions::nber.shade().
#' @param customShading Boolean. Default is FALSE. If TRUE, shades region using fiafunctions::shadedRegion().
#' @param shadeStart 
#' @param shadeEnd 
#' @param lineType Vector. Can be a mixture of numeric and character (eg. 1, "solid", 2, "dashed", 3, "dotted").
#' @param lineWidth Numeric vector. fiafunctions default is 1.7.
#' @param border Boolean. Default is FALSE.
#' @param bottomToTop Boolean. Default is FALSE.
#' @param seriesBlackBorder Boolean. Default is FALSE.
#' @param xSeriesDivider Boolean. Default is FALSE.
#' @param PRESET list(), used for common chart set ups. Variables within the list will override the default values, unless overridden themselves by variables explicitly specified by the user in the chart call function.
#' @param dataType Depreciated. Used to signify if data was from an R object or FAME.
#' @param vertShift Depreciated. Replaced with lastValuePlacementY.
#' @param horizShift Depreciated. Replaced with lastValuePlacementX.
#' 
polygonChart <- function(
#dataFrame
  dataFrame = NULL,
  dataFrameLine = dataFrame,
  famePath = NULL,
  #famePathLine = famePath,
#series
  series = NULL,
  seriesLine = NULL,
  interp = FALSE,
#chartTitle 
  chartTitle = "",
  chartTitleFontSize = opt$paneltitlecex,
#x-axis
  seriesX = 'date',
  seriesXLabel = '',
  startDate = NULL,
  endDate = NULL,
  xMajorTickFreq = "year",
  xMinorTickFreq = "month",
  xLabelFreq = "month",
  xLabelCenter = TRUE,
  xTicksBelowAxis = FALSE,
  extraLimX = 0,
#y-axis
  yMin = NULL,
  yMax = NULL,
  yStep = NULL,
  seriesScale = 1,
  unitsLabel = "",
  hadj = opt$yaxis.pos,
  roundDig = 2,
#left/line y-axis
  yMinLeft = NULL,
  yMaxLeft = NULL,
  yStepLeft = NULL,
  seriesScaleLine = 1,
  unitsLabelLeft = "",
  roundDigLeft = 2,
#axis aesthetics
  axisColorRight = "black",
  axisColorLeft = "black",
  labelFontSize = opt$legend.cex,
  xAxisFontSize = opt$axis.cex,
  yAxisFontSize = opt$axis.cex,
#legend
  legend = TRUE,
  legendNames = NULL,
  legendCol = 1,
  legendColPlacement = NULL,
  legendLocationX = 0.05,
  legendLocationY = 0.97,
  legendFontSize = opt$key.cex,
#footnote
  footNote = "",
  footNotePlacement = NULL,
  footNoteFontSize = opt$foot.cex,
#lastValue
  printLastValue = TRUE,
  lastValueFreq = NA,
  lastValue = NULL,
  lastValueLocationX = 0.9,
  lastValueLocationY = 0.9,
  lastValuePrelim = FALSE,
#frequencyLabel
  frequencyLabel = "",
#colors 
  colors = NULL,
  colorsLine = NULL,
  palette = NULL,
  scheme = NULL,
  dynamicColors = FALSE,
#extras
  lineAtZero = TRUE,
  recessions = FALSE,
  customShading = FALSE,
  shadeStart = "",
  shadeEnd = "",
#lineType + lineWidth
  lineType = NULL,
  lineWidth = NULL,
#polygon specific
  border = FALSE,
  bottomToTop = FALSE,
  seriesBlackBorder = FALSE,
  xSeriesDivider = FALSE,
#PRESET
  PRESET = list(),
# DEPRECATED PARAMETERS #
  dataType = NULL,
  vertShift = NULL,
  horizShift = NULL
) {

  #### 0. deprecated and preset parameters ####

  if (!is.null(vertShift)) {
    lastValueLocationY <- vertShift
  }

  if (!is.null(horizShift)) {
    lastValueLocationX <- horizShift
  }

  deprecated_pars <- (list('dataType', 'vertShift', 'horizShift'))

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


  #### 1. create data subset ####

  plot.data <- getCopy(dataFrame = dataFrame,
                       series = series,
                       famePath = famePath,
                       startDate= startDate,
                       endDate= endDate,
                       seriesScale = seriesScale,
                       interp = interp)

  plot.data.left <- getCopy(dataFrame = dataFrameLine,
                            series = seriesLine,
                            famePath = famePath,
                            startDate = startDate,
                            endDate = endDate,
                            seriesScale = seriesScaleLine,
                            interp = interp)

  # specific to polygon chart: refuse to show NAs
  if (!interp) {

    if (sum(is.na(plot.data)) > 0) {
      stop("dataFrame contains NA values. Please either explicitly fill these or use interp = T.")
    }

    if (sum(is.na(plot.data.left)) > 0) {
      stop("dataFrameLine contains NA values. Please either explicitly fill these or use interp = T.")
    }

  }

  # split data into positive and negative
  plot.data.positive <- plot.data
  plot.data.negative <- plot.data
  for (s in series) {
    plot.data.positive[, s] <- pmax(plot.data.positive[, s], 0)
    plot.data.negative[, s] <- pmin(plot.data.negative[, s], 0)
  }

  plot.data$stackedPositiveHeight <- rowSums(data.frame(plot.data.positive[, series]))
  plot.data$stackedNegativeHeight <- rowSums(data.frame(plot.data.negative[, series]))

  #### 2. define chart limits ####

  yLims <- setYLims(df = plot.data,
                    dfLeft = plot.data.left,
                    series = c('stackedPositiveHeight', 'stackedNegativeHeight'),
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

  xLims = c(from=as.Date(floor_date(min(plot.data$date))), to=(max(plot.data$date) + extraLimX))


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

  #Finish setting up lineType and lineWidth
  lineFormats <- lineFormatting(lineType = lineType,
                                lineWidth = lineWidth,
                                series = series,
                                seriesLeft = seriesLine
  )
  lineType <- lineFormats$lineType
  lineWidth <- lineFormats$lineWidth


  #### 5. plot data ####

  # what about if there's no positive or negative?

  # whether we should plot the series from the bottom up, or from the top down (ie, last series plots closest to axis)
  plotOrder <- if (bottomToTop) 2:ncol(plot.data.positive) else ncol(plot.data.positive):2

  # if multiple series, stack to get the upper edge of polygon (lower for negative)
  if (length(series) > 1) {
    positive_tops <- cbind('date' = plot.data$date, as.data.frame(t(apply(as.data.frame(plot.data.positive[, plotOrder]), 1, cumsum))))
    negative_bottoms <- cbind('date' = plot.data$date, as.data.frame(t(apply(as.data.frame(plot.data.negative[, plotOrder]), 1, cumsum))))
  } else {
    positive_tops <- plot.data.positive
    negative_bottoms <- plot.data.negative
  }

  # use series that came before (or zero line) to determine bottom edge (upper for negative)
  positive_bottoms <- cbind('date' = positive_tops$date, 0, as.data.frame(positive_tops[, -c(1, ncol(positive_tops))]))
  negative_tops <- cbind('date' = negative_bottoms$date, 0, as.data.frame(negative_bottoms[, -c(1, ncol(negative_bottoms))]))

  # set up an empty plot
  plot(x = 0, y = 0,
       type = 'l',
       xlab = "",
       ylab = "",
       ylim =  c(yMin,yMax),
       axes = FALSE,
       col  = colors[1],
       lty  = lineType[1],
       lwd  = lineWidth[1],
       main = "",
       bty  = 'u',
       yaxs = "i",
       xlim = xLims,
       # panel.first is super important and will apply the flourishes before the data are plotted. This allows series to be plotted on top of the shading
       panel.first = {  flourish(lineAtZero = lineAtZero,
                                 df = plot.data,
                                 recessions = recessions,
                                 customShading = customShading,
                                 shadeEnd = shadeEnd,
                                 shadeStart = shadeStart,
                                 yMax = yMax,
                                 seriesX = seriesX)}
  )

  # add positive and negative polygons to chart
  for (i in 1:length(series)) {

    polygon(x = c(positive_tops$date, rev(positive_bottoms$date)),
            y = c(positive_tops[, i + 1], rev(positive_bottoms[, i + 1])),
            col = colors[plotOrder[i] - 1],
            border = ifelse(seriesBlackBorder, 'black', NA))

    polygon(x = c(negative_tops$date, rev(negative_bottoms$date)),
            y = c(negative_tops[, i + 1], rev(negative_bottoms[, i + 1])),
            col = colors[plotOrder[i] - 1],
            border = ifelse(seriesBlackBorder, 'black', NA))

  }

  # save chart parameters
  chartParametersRight <- par('usr')
  chartParametersLeft <- c(par('usr')[1:2], yMinLeft, yMaxLeft)


  #### 6. plot rest of series, if any ####

  # if we have left-hand series, plot those
  if (!is.null(seriesLine)){

    par('usr' = chartParametersLeft, new = T)
    plot(x = plot.data.left[['date']], y = plot.data.left[,2],
         type = 'l',
         xlab = "",
         ylab = "",
         ylim =  c(yMinLeft,yMaxLeft),
         axes = FALSE,
         col  = colors[1+length(series)],
         lty  = lineType[length(series)+1],
         lwd  = lineWidth[length(series)+1],
         main = "",
         bty  = 'u',
         yaxs = "i",
         xlim = xLims,
    )

    # plot rest of series, if any
    if(length(seriesLine) > 1){

      for(i in 2:(length(seriesLine))){
        lines(x = plot.data.left[['date']], y = plot.data.left[,i + 1],
              lty = lineType[length(series) + i],
              lwd = lineWidth[length(series) + i],
              col = colors[length(series) + i]
        )
      }
    }

    par('usr' = chartParametersRight)

  }

  #### 7. labels ####

  chartLabels(chartType = 'polygon',
              df = plot.data,
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
              labelFontSize = labelFontSize,
              seriesXLabel = seriesXLabel
  )


  #### 8. axes ####

  plotHookBox()

  drawYAxis(df = plot.data,
            dfLeft = plot.data.left,
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

  if (is.numeric(plot.data[[seriesX]])) {
    xTicksLabels <-defineXAxisNumeric(df = plot.data,
                                      xMajorTickFreq = xMajorTickFreq,
                                      xMinorTickFreq = xMinorTickFreq,
                                      roundDig = roundDig,
                                      seriesX = seriesX)
  } else {
    xTicksLabels <-defineXAxisDate(df = plot.data,
                                   xMajorTickFreq = xMajorTickFreq,
                                   xMinorTickFreq = xMinorTickFreq,
                                   xLabelCenter = xLabelCenter)
  }

  drawXAxis(xAxisFontSize = xAxisFontSize,
            xLabelFreq = xLabelFreq,
            xTicksBelowAxis = xTicksBelowAxis,
            xTicksLabels = xTicksLabels)

}
