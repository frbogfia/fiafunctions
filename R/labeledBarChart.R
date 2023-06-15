#' @title Bar Chart With X-axis Labels
#' @description This creates a bar chart that has labels on the x-axis instead of dates
#' @export
labeledBarChart <- function(
  series ="",
  dataFrame=NA,
  colors= opt$colors,
  yMin=NULL,
  yMax=NULL,
  yStep=NULL,
  xLabels = "",
  xTicksLeft=.56,
  legendCol=1,
  legendNames="",
  legend = FALSE,
  chartTitle="",
  chartTitleFontSize = opt$paneltitlecex,
  labelFontSize = opt$legend.cex,
  unitsLabel="",
  unitsLabelLeft="",
  frequencyLabel="",
  axisColorRight = "black",
  axisColorLeft = "black",
  beside=FALSE,
  footNote="",
  footNotePlacement=1.2,
  footNoteFontSize = opt$foot.cex,
  seriesScale=1,
  yMaxLab=yMax,
  xLabelsScale = 0.65,
  xLabelsPerp = TRUE,
  singleBarColor = TRUE,
  barBorder = T,
  barBorderColor = 'black',
  barBorderWidth = 1,
  legendLocationX    =0.05,
  legendLocationY    =1,
  legendColPlacement = NULL,
  legendInset =.1,
  legendFontSize=opt$key.cex,
  dynamicColors=FALSE,
  centerFirstLabels = FALSE,
  palette = NULL,
  scheme = NULL
)
{

  #### 1. create data subsets ####
  plot.data <- dataFrame %>%
    select(series) %>%
    mutate_at(series, .funs = list(~ . * seriesScale)) %>% 
    as.data.frame()

  # split bar data into positive and negative
  plot.data.positive <- plot.data
  plot.data.negative <- plot.data
  for (s in series) {
    plot.data.positive[, s] <- pmax(plot.data.positive[, s], 0)
    plot.data.negative[, s] <- pmin(plot.data.negative[, s], 0)
  }


  plot.data$stackedBarPositiveHeight <- rowSums(plot.data.positive[, series, drop = F])
  plot.data$stackedBarNegativeHeight <- rowSums(plot.data.negative[, series, drop = F])

  #### 2. define chart limits ####

  yLims <- setYLims(df = plot.data,
                    series = c('stackedBarPositiveHeight', 'stackedBarNegativeHeight'),
                    yMax = yMax,
                    yMin = yMin,
                    yStep = yStep,
                    yMaxLeft = NULL,
                    yMinLeft = NULL,
                    yStepLeft = NULL

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

  #This is sets the makes the color of the bars all the same if true - Maybe this should be deprecated?
  if(singleBarColor==TRUE){
    colors <- rep(colors[1],nrow(plot.data))
  }

  #### 4. Plot Bars ####
  
  # have to adjust line settings for bar border width
  savelwd <- par('lwd')
  par('lwd' = barBorderWidth)

  #This function plots the rows of your dataframe as a bar and the columns represent stacked bars for each row. For example if each row is a firm and the columns are assets and liabilities, then make sure your xLabels correspond to however many rows/firms you want to plot. the bars will be stacked bars of each indivudal firms assets and liabilities
  if(length(series) ==1){
    bars <- barplot(height =c(plot.data.positive[,c(series)]),
                    col = colors[1:nrow(plot.data.positive)],
                    yaxt = 'n',
                    bty = 'u',
                    ylim = c(yMin*1.1, yMax*1.1),
                    #xlim = c(xbounds[[1]],xbounds[[2]]),
                    width =1,
                    xaxt="n",
                    tck = 0,
                    main="",
                    beside=beside,
                    axes=FALSE,
                    horiz=FALSE,
                    border = ifelse(barBorder, barBorderColor, F)
    )

    par(new=TRUE)
    #plot and stack negative values
    bars2 <- barplot(height = plot.data.negative[,c(series)],
                     col = colors[1:nrow(plot.data.negative)],
                     yaxt = 'n',
                     bty = 'u',
                     ylim = c(yMin*1.1, yMax*1.1),
                     #xlim = c(xbounds[[1]],xbounds[[2]]),
                     width =1,
                     xaxt="n",
                     tck = 0,
                     main="",
                     beside=beside,
                     axes=FALSE,
                     border = ifelse(barBorder, barBorderColor, F)
    )
  }else{
    bars <- barplot(height = as.matrix(plot.data.positive[,c(series)]) %>% t(),
                    #col = colors[1:nrow(plot.data.positive)],
                    col = colors,
                    yaxt = 'n',
                    bty = 'u',
                    ylim = c(yMin*1.1, yMax*1.1),
                    #xlim = c(xbounds[[1]],xbounds[[2]]),
                    width =1,
                    xaxt="n",
                    tck = 0,
                    main="",
                    beside=beside,
                    axes=FALSE,
                    horiz=FALSE,
                    border = ifelse(barBorder, barBorderColor, F)
    )

    par(new=TRUE)
    #plot and stack negative values
    bars2 <- barplot(height = as.matrix(plot.data.negative[,c(series)]) %>% t(),
                     #col = colors[1:nrow(plot.data.negative)],
                     col = colors,
                     yaxt = 'n',
                     bty = 'u',
                     ylim = c(yMin*1.1, yMax*1.1),
                     #xlim = c(xbounds[[1]],xbounds[[2]]),
                     width =1,
                     xaxt="n",
                     tck = 0,
                     main="",
                     beside=beside,
                     axes=FALSE,
                     border = ifelse(barBorder, barBorderColor, F)
    )

  }
  
  # back to original line settings
  par('lwd' = savelwd)

  #### 5. Add labels ####

  chartLabels(chartType = 'stackedBar',
              df = plot.data.full,
              chartTitle = chartTitle,
              chartTitleFontSize = chartTitleFontSize,
              series = series,
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
              unitsLabel = unitsLabel,
              colors = colors,
              unitsLabelLeft = unitsLabelLeft,
              frequencyLabel = frequencyLabel,
              axisColorRight = axisColorRight,
              axisColorLeft = axisColorLeft,
              printLastValue = F,

              yMax = yMax,
              yMin = yMin,
              yStep = yStep,
              yMaxLeft = yMaxLeft,
              yMinLeft = yMinLeft,
              yStepLeft = yStepLeft,
              labelFontSize = labelFontSize
  )


  #### 6. Create Axes ####
  if (xLabelsPerp == FALSE) {
    las.xLabels <- 0
  }else{
    las.xLabels <- 2
  }

  par(new=TRUE)
  xLabels=xLabels
  plotHookBox(lwd=1.5)
  # I added this bit here that tries to center the labels on the bars. if you have 2 series per group the first bar should have the group label and the second one should be blank
  if(centerFirstLabels ==FALSE){
    axis(1,at = bars,
         labels = xLabels,
         tick = FALSE,
         las=las.xLabels,
         cex.axis=xLabelsScale,
         lwd=2,
         line=-.5)}
  else{
    axis(1,
         at = (bars+(bars[1,1]*.47)),
         labels = xLabels,
         tick = FALSE,
         las=las.xLabels,
         cex.axis=xLabelsScale,
         lwd=2,
         line=-.5)
  }
  #This does all the Y axis ticks
  axis(side = 4,
       at = seq(yMin,yMax, by = yStep),
       labels = FALSE,
       tck = opt$tick.length+.005,
       cex.axis = opt$axis.cex,
       las = 2)
  axis(side = 2,
       at = seq(yMin,yMax, by = yStep),
       labels = FALSE,
       tck = opt$tick.length+.005,
       cex.axis = opt$axis.cex,
       las = 2)
  axis(side = 4,
       at = seq(yMin,yMaxLab, by = yStep),
       labels = TRUE,
       tck = opt$tick.length+.005,
       cex.axis = opt$axis.cex, las = 2)

  #This is for the x axis ticks
  if(beside==FALSE){
    axis(1,
         at = bars-xTicksLeft,
         labels =FALSE,
         tick = TRUE,
         las=1,
         tck =opt$tick.length+.015,
         cex.axis=opt$axis.cex)}
  else{
    #This makes it so the x axis ticks separate the groups of bars instead of each bar
    minBarTick <- bars[1,]-.52
    maxBarTick <- bars[nrow(bars),]+.52
    bars <- c(minBarTick,maxBarTick)

    axis(1,
         at = bars,
         labels =FALSE,
         tick = TRUE,
         las=1,
         tck =opt$tick.length+.015,
         cex.axis=opt$axis.cex)
  }

}


