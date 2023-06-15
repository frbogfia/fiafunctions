#' @title Line Chart
#' @description This creates a line chart.
#' @export
#' @examples
#' \dontrun{
#' lineChart(chartTitle="Example Line Chart",
#'          legendNames=c("Value 1", "Value 2"),
#'          series=c("value", "value2"),
#'          dataFrame=testDataFrame,
#'          frequencyLabel="Decribe Legend Here",
#'          #yStep=2,
#'          #yMin=-2,
#'          #yMax=40,
#'          footNote=c('Insert Footnote Text'),
#'          legendCol=1,
#'          startDate="2006-01-01",
#'          unitsLabel="Units of Data",
#'          footNotePlacement=1.5,
#'          colors = c("firebrick","steelblue3"),
#'          dataType="r"
#'          )
#' }
#'          
#' @param dataFrame 
#' @param dataFrameLeft 
#' @param famePath 
#' @param famePathLeft 
#' @param series 
#' @param seriesLeft 
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
#' @param seriesScaleLeft 
#' @param unitsLabelLeft 
#' @param roundDigLeft Integer indicating the number of decimal places to be used. Default is 2.
#' @param axisColorRight Character variable. Default is "black".
#' @param axisColorLeft 
#' @param labelFontSize Default is 0.7.
#' @param xAxisFontSize Default is 0.65.
#' @param yAxisFontSize Default is 0.65.
#' @param colorBandSeries Character variable. Color of banded series. Default is "#0000FF33".
#' @param seriesBandUpper Character vector. Series used to be upper line of polygon.
#' @param seriesBandLower Character vector. Series used to be lower line of polygon.
#' @param borderBand The color to draw the border. The default, NULL, means to use par("fg"). Use borderBand = NA to omit borders.
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
#' @param PRESET list(), used for common chart set ups. Variables within the list will override the default values, unless overridden themselves by variables explicitly specified by the user in the chart call function.
#' @param dataType Depreciated. Used to signify if data was from an R object or FAME.
#' @param vertShift Depreciated. Replaced with lastValuePlacementY.
#' @param horizShift Depreciated. Replaced with lastValuePlacementX.
#' 
lineChart <- function(
#dataFrame
  dataFrame = NULL,
  dataFrameLeft = dataFrame,
  famePath = NULL,
  famePathLeft = famePath,
#series
  series = NULL,
  seriesLeft = NULL,
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
  xLabelCenter = T,
  xTicksBelowAxis = F,
  extraLimX = 0,
#y-axis
  yMin = NULL,
  yMax = NULL,
  yStep = NULL,
  seriesScale = 1,
  unitsLabel = "",
  hadj = opt$yaxis.pos,
  roundDig = 2,
#left y-axis
  yMinLeft = NULL,
  yMaxLeft = NULL,
  yStepLeft = NULL,
  seriesScaleLeft = 1,
  unitsLabelLeft = "",
  roundDigLeft = 2,
#axis aesthetics
  axisColorRight = "black",
  axisColorLeft = "black",
  labelFontSize = opt$legend.cex,
  xAxisFontSize = opt$axis.cex,
  yAxisFontSize = opt$axis.cex,
#band info
  colorBandSeries = NA, # this is the series that is 'banded'
  seriesBandUpper = NA, # band itself
  seriesBandLower = NA, # band itself
  borderBand = F,
#legend
  legend = TRUE,
  legendNames = NULL,
  legendCol = 1,
  legendColPlacement = NULL,
  legendLocationX = 0.05,
  legendLocationY = 0.97,
  legendFontSize = opt$key.cex,
#footNote
  footNote = "",
  footNotePlacement = NULL,
  footNoteFontSize = opt$foot.cex,
#lastValue
  printLastValue = TRUE,
  lastValueFreq = NA,
  lastValue = NULL,
  lastValueLocationX = 0.9,
  lastValueLocationY = 0.9,
  lastValuePrelim = F,
#frequencyLabel
  frequencyLabel = "",
#colors
  colors = NULL,
  palette = NULL,
  scheme = NULL,
  dynamicColors = FALSE,
#extras
  lineAtZero = FALSE,
  recessions = FALSE,
  customShading = FALSE,
  shadeStart = "",
  shadeEnd = "",
#lineType + lineWidth
  lineType= NULL,
  lineWidth= NULL,
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

  #### 1. create data subsets ####

  plot.data <- getCopy(dataFrame = dataFrame,
                       series = series,
                       seriesX = seriesX,
                       famePath = famePath,
                       startDate= startDate,
                       endDate= endDate,
                       seriesScale = seriesScale,
                       interp = interp)

 plot.data.left <- getCopy(dataFrame = dataFrameLeft,
                              series = seriesLeft,
                              seriesX = seriesX,
                              famePath = famePathLeft,
                              startDate= startDate,
                              endDate= endDate,
                              seriesScale = seriesScaleLeft,
                              interp = interp)


  #### 2. define chart limits ####

  yLims <- setYLims(df = plot.data,
                    dfLeft = plot.data.left,
                    series = series,
                    seriesLeft = seriesLeft,
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

  if (is.numeric(plot.data[[seriesX]]) ){
    xLims  = c(from=min(plot.data[[seriesX]]), to=(max(plot.data[[seriesX]]) + extraLimX))
  }else{
    xLims  = c(from=as.Date(floor_date(min(plot.data$date))), to=(max(plot.data$date) + extraLimX))
  }
  
  #### 3. pick colors ####

  colors <- pickColors(series = series,
                       seriesLeft = seriesLeft,
                       colors = colors,
                       dynamicColors = dynamicColors,
                       palette = palette,
                       scheme = scheme)

  #### 4. type-specific formatting ####

  #Finish setting up lineType and lineWidth
  lineFormats <- lineFormatting(lineType = lineType,
                                lineWidth = lineWidth,
                                series = series,
                                seriesLeft = seriesLeft
  )
  lineType <- lineFormats$lineType
  lineWidth <- lineFormats$lineWidth

  #### 5. plot data ####

  # Right-hand data: create plot
  plot(x = plot.data[[seriesX]], y = plot.data[,2],
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

  # save chart parameters
  chartParametersRight <- par('usr')
  chartParametersLeft <- c(par('usr')[1:2], yMinLeft, yMaxLeft)

  #### 6. plot rest of series, if any ####
  if(length(series) > 1){
    for(i in 2:(length(series))){
      lines(x = plot.data[[seriesX]], y = plot.data[,i + 1],
            lty = lineType[i],
            lwd = lineWidth[i],
            col = colors[i],
      )
    }
  }

  # if we have left-hand series, plot those
  if (!is.null(seriesLeft)){

    par('usr' = chartParametersLeft, new = T)
    plot(x = plot.data.left[[seriesX]], y = plot.data.left[,2],
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
        # log = logLeftStr,
         xlim = xLims,
    )

    # plot rest of series, if any
    if(length(seriesLeft) > 1){

      for(i in 2:(length(seriesLeft))){
        lines(x = plot.data.left[[seriesX]], y = plot.data.left[,i + 1],
              lty = lineType[length(series) + i],
              lwd = lineWidth[length(series) + i],
              col = colors[length(series) + i],
        )
      }
    }

    par('usr' = chartParametersRight)

  }

  # Band data
  if (!is.na(seriesBandUpper) & !is.na(seriesBandLower)) {
    addSeriesBand(dataFrame = dataFrame,
                  seriesBandLower = seriesBandLower,
                  seriesBandUpper = seriesBandUpper,
                  seriesScale = 1,
                  colorBandSeries = colorBandSeries,
                  borderBand = borderBand,
                  startDate = startDate,
                  endDate = endDate,
                  famePath = famePath)
  }

  #### 7. labels ####


  chartLabels(chartType = 'line',
              df = plot.data,
              chartTitle = chartTitle,
              chartTitleFontSize = chartTitleFontSize,
              series = series,
              seriesLeft = seriesLeft,
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
