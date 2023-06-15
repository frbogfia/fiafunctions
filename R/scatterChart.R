#' @title Scatter Chart
#' @description This creates a scatter chart.
#' @export
#' @examples
#' \dontrun{
#' scatterChart(
#'      chartTitle = 'Sepal width vs Sepal Length',
#'      dataFrame = chart_data,
#'      seriesX = c('Sepal.Length'), # given if not using the default date for x axis, must be numeric
#'      series = c("setosa", "versicolor", "virginica"),
#'      legendNames = c("setosa", "versicolor", "virginica"),
#'      dynamicColors = T,
#'      yMax = 4.5,
#'      yMin = 2,
#'      yStep = 0.5,
#'      startDate = 4,
#'      endDate = 8,
#'      xMajorTickFreq = 0.5,
#'      xMinorTickFreq = .25,
#'      customShading = T,
#'      shadeStart = 5,
#'      shadeEnd = 7,
#'      trendLines = T,
#'    )
#'    }
#'    
#' @param dataFrame 
#' @param famePath 
#' @param series 
#' @param chartTitle Character variable.
#' @param chartTitleFontSize Default is 0.85.
#' @param seriesX Character vector. Default is c("date").
#' @param seriesXLabel Character variable. Typically used if seriesX != c("date").
#' @param startDate If seriesX == c("date"), character variable. If is.numeric(seriesX), numberic variable.
#' @param endDate 
#' @param xMajorTickFreq If seriesX == c("date"), character variable (eg. "year", "quarter", "month", "day").
#' @param xMinorTickFreq 
#' @param xLabelFreq 
#' @param xTicksBelowAxis Boolean variable. Default is FALSE.
#' @param extraLimX Numeric variable. Adjusts x-axis limits.
#' @param yMin 
#' @param yMax 
#' @param yStep 
#' @param seriesScale 
#' @param unitsLabel Character variable.
#' @param hadj Numeric variable. Default is 0.5. Justification for all labels parallel to reading direction.
#' @param roundDig Integer indicating the number of decimal places to be used. Default is 2.
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
#' @param printLastValue Boolean. Default is FALSE. 
#' @param lastValueFreq If seriesX == c("date"), character variable (eg. "year", "quarter", "month", "day").
#' @param lastValue Specify lastValue
#' @param lastValueLocationX Numeric. Horizontal location of last value as percent of chart area. Default is 0.9.
#' @param lastValueLocationY Numeric. Vertical location of last value as percent of chart area. Default is 0.9.
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
#' @param pointType Numeric vector. Defaults tp pch = 20.
#' @param trendLine Boolean variable. Default is FALSE.
#' @param lineType Vector. Can be a mixture of numeric and character (eg. 1, "solid", 2, "dashed", 3, "dotted").
#' @param lineWidth Numeric vector. fiafunctions default is 1.7.
#' @param dataType Depreciated. Used to signify if data was from an R object or FAME.
#' @param vertShift Depreciated. Replaced with lastValuePlacementY.
#' @param horizShift Depreciated. Replaced with lastValuePlacementX.
#' 
#Make legend labels match color of axes
scatterChart <- function(
#dataFrame
  dataFrame = NULL,
  famePath = NULL,
#series
  series = NULL,
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
  #xLabelCenter = T,
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
  legendColPlacement = strwidth(max(legendNames)),
  legendLocationX = 0.05,
  legendLocationY = 0.97,
  legendFontSize = opt$key.cex,
#footNote
  footNote = "",
  footNotePlacement = NULL,
  footNoteFontSize = opt$foot.cex,
#lastValue
  printLastValue = FALSE,
  lastValueFreq = NA,
  lastValue = NULL,
  lastValueLocationX = 0.9,
  lastValueLocationY = 0.9,
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
#scatter specific
  pointType = NULL,
  trendLine = F,
  lineType = NULL,
  lineWidth = NULL,
### Deprecated Parameters
  dataType = NULL,
  horizShift = NULL,
  vertShift = NULL
) {

  #### 1. create data subsets ####
  plot.data <- getCopy(dataFrame = dataFrame,
                       series = series,
                       famePath = famePath,
                       startDate= startDate,
                       endDate= endDate,
                       seriesScale = seriesScale,
                       interp = F,
                       seriesX = seriesX)


  #### 2. define chart limits ####

  yLims <- setYLims(df = plot.data,
                    dfLeft = plot.data.left,
                    series = series,
                    yMax = yMax,
                    yMin = yMin,
                    yStep = yStep,
                    yMaxLeft = NULL,
                    yMinLeft = NULL,
                    yStepLeft = NULL
                    )## Returns a list with all yLimits and steps
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
                       seriesLeft = NULL,
                       colors = colors,
                       dynamicColors = dynamicColors,
                       palette = palette,
                       scheme = scheme)
  
  if(is.null(pointType)){
    pointType <- rep(20, length(series))
  }

  #### 4. plot data ####
  plot(x = plot.data[[seriesX]], y = plot.data[,2],
       type = 'p',
       pch = pointType[1],
       xlab = "",
       ylab = "",
       ylim =  c(yMin,yMax),
       axes = FALSE,
       col  = colors[1],
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

  if(length(series) > 1){
    for(i in 2:(length(series))){
      points(x = plot.data[[seriesX]], y = plot.data[,i + 1],
             pch = pointType[i],
             col = colors[i]
      )
    }
  }
  
  if (trendLine){
    lineFormats <- lineFormatting(lineType = lineType,
                                  lineWidth = lineWidth,
                                  series = series,
                                  seriesLeft = NULL
    )
    lineType <- lineFormats$lineType
    lineWidth <- lineFormats$lineWidth
  
    for(i in 1:(length(series))){
      abline(lm(plot.data[,i+1] ~ plot.data[[seriesX]]), 
         col = colors[i], 
         lty = lineType[i], 
         lwd = lineWidth[i])
}}

  #### 5. labels ####

  chartLabels(chartType = 'scatter',
              df = plot.data,
              chartTitle = chartTitle,
              chartTitleFontSize = chartTitleFontSize,
              series = series,
              pointType = pointType,
              legend = legend,
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
              frequencyLabel = frequencyLabel,
              axisColorRight = axisColorRight,
              axisColorLeft = axisColorLeft,
              printLastValue = printLastValue,
              lastValuePrelim = FALSE,
              lastValueFreq = lastValueFreq,
              lastValue = lastValue,
              lastValueLocationX = lastValueLocationX,
              lastValueLocationY = lastValueLocationY,
              yMax = yMax,
              yMin = yMin,
              yStep = yStep,
              labelFontSize = labelFontSize,
              seriesXLabel = seriesXLabel
  )


  #### 6. axes ####

  plotHookBox()

  drawYAxis(df = plot.data,
            dfLeft = NULL,
            yMin = yMin,
            yMax = yMax,
            yStep = yStep,
            yAxisFontSize = yAxisFontSize,
            hadj = hadj,
            axisColorRight = axisColorRight,
            axisColorLeft = axisColorLeft,
            roundDig = roundDig,
            chartParametersRight = chartParametersRight)

  if (is.numeric(plot.data[[seriesX]])) {
    xTicksLabels <-defineXAxisNumeric(df = plot.data,
                                      xMajorTickFreq = xMajorTickFreq,
                                      xMinorTickFreq = xMinorTickFreq,
                                      roundDig = roundDig,
                                      seriesX = seriesX)
  } else {
    xTicksLabels <-defineXAxisDate(df = plot.data,
                                  xMajorTickFreq = xMajorTickFreq,
                                  xMinorTickFreq = xMinorTickFreq)
  }

  drawXAxis(xAxisFontSize = xAxisFontSize,
            xLabelFreq = xLabelFreq,
            xTicksBelowAxis = xTicksBelowAxis,
            xTicksLabels = xTicksLabels)

}
