#' @title legend2
#' @description Allows the columns in base R legend to have better widths.
#' @export
legend2 <- function(x, y = NULL, legend, fill = NULL, col = par("col"),
                     border = "black", lty, lwd, pch, angle = 45, density = NULL,
                     bty = "o", bg = par("bg"), box.lwd = par("lwd"), box.lty = par("lty"),
                     box.col = par("fg"), pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd,
                     xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0,
                                                                                 0.5), text.width = NULL, text.col = par("col"), text.font = NULL,
                     merge = do.lines && has.pch, trace = FALSE, plot = TRUE,
                     ncol = 1, horiz = FALSE, title = NULL, inset = 0, xpd, title.col = text.col,
                     title.adj = 0.5, seg.len = 2) {

  # in the event a column width is specified, need these to default to 0
  xExtra2 <- 0
  xExtra3 <- 0

  if (missing(legend) && !missing(y) && (is.character(y) ||
                                         is.expression(y))) {
    legend <- y
    y <- NULL
  }
  mfill <- !missing(fill) || !missing(density)
  if (!missing(xpd)) {
    op <- par("xpd")
    on.exit(par(xpd = op))
    par(xpd = xpd)
  }
  title <- as.graphicsAnnot(title)
  if (length(title) > 1)
    stop("invalid 'title'")
  legend <- as.graphicsAnnot(legend)
  n.leg <- if (is.call(legend))
    1
  else length(legend)
  if (n.leg == 0)
    stop("'legend' is of length 0")
  auto <- if (is.character(x))
    match.arg(x, c("bottomright", "bottom", "bottomleft",
                   "left", "topleft", "top", "topright", "right", "center"))
  else NA
  if (is.na(auto)) {
    xy <- xy.coords(x, y, setLab = FALSE)
    x <- xy$x
    y <- xy$y
    nx <- length(x)
    if (nx < 1 || nx > 2)
      stop("invalid coordinate lengths")
  }
  else nx <- 0
  xlog <- par("xlog")
  ylog <- par("ylog")
  rect2 <- function(left, top, dx, dy, density = NULL, angle,
                    ...) {
    r <- left + dx
    if (xlog) {
      left <- 10^left
      r <- 10^r
    }
    b <- top - dy
    if (ylog) {
      top <- 10^top
      b <- 10^b
    }
    rect(left, top, r, b, angle = angle, density = density,
         ...)
  }
  segments2 <- function(x1, y1, dx, dy, ...) {
    x2 <- x1 + dx
    if (xlog) {
      x1 <- 10^x1
      x2 <- 10^x2
    }
    y2 <- y1 + dy
    if (ylog) {
      y1 <- 10^y1
      y2 <- 10^y2
    }
    segments(x1, y1, x2, y2, ...)
  }
  points2 <- function(x, y, ...) {
    if (xlog)
      x <- 10^x
    if (ylog)
      y <- 10^y
    points(x, y, ...)
  }
  text2 <- function(x, y, ...) {
    if (xlog)
      x <- 10^x
    if (ylog)
      y <- 10^y
    text(x, y, ...)
  }
  if (trace) {
    catn <- function(...) do.call(cat, c(lapply(list(...),
                                                formatC), "\n"))
    fv <- function(...) paste(vapply(lapply(list(...), formatC),
                                     paste, collapse = ",", ""), collapse = ", ")
  }
  Cex <- cex * par("cex")
  if (is.null(text.width)) {
    # browser()
    # text.width <- max(abs(strwidth(legend, units = "user",
    #                                cex = cex, font = text.font)))
    npercol <- ceiling(length(legend) / ncol)

    col.width <- rep(NA, ncol)
    for (i in 1:ncol) {
      legendColIndex <- seq(from = npercol * (i - 1) + 1, by = 1, length.out = npercol)
      col.width[i] <- max(abs(strwidth(legend[legendColIndex], units = "user",
                                       cex = cex, font = text.font)), na.rm = T)
    }

    # text.width <- min(col.width)
    text.width <- 0
    # xExtra2 <- rep(c(0, col.width - min(col.width)), each = npercol)[1:length(legend)]
    xExtra2 <- rep(cumsum(c(0, col.width[-length(col.width)])), each = npercol)[1:length(legend)]
    xExtra3 <- rep(cumsum(c(0, rep(.015 * (par('usr')[2] - par('usr')[1]), ncol - 1))), each = npercol)[1:length(legend)]
    # browser()
  }
  else if (!is.numeric(text.width) || text.width < 0)
    stop("'text.width' must be numeric, >= 0")
  xyc <- xyinch(par("cin"), warn.log = FALSE)
  xc <- Cex * xyc[1L]
  yc <- Cex * xyc[2L]
  if (any(n_ <- xc < 0))
    text.width[n_] <- -text.width[n_]
  xchar <- xc
  xextra <- 0
  yextra <- yc * (y.intersp - 1)
  ymax <- yc * max(1, strheight(legend, units = "user", cex = cex)/yc)
  ychar <- yextra + ymax
  if (trace)
    catn("  xchar=", fv(xchar), "; (yextra, ychar)=", fv(yextra,
                                                         ychar))
  if (mfill) {
    xbox <- xc * 0.8
    ybox <- yc * 0.5
    dx.fill <- max(xbox)
  }
  do.lines <- (!missing(lty) && (is.character(lty) || any(lty >
                                                            0))) || !missing(lwd)
  n.legpercol <- if (horiz) {
    if (ncol != 1)
      warning(gettextf("horizontal specification overrides: Number of columns := %d",
                       n.leg), domain = NA)
    ncol <- n.leg
    1
  }
  else ceiling(n.leg/ncol)
  has.pch <- !missing(pch) && length(pch) > 0
  if (do.lines) {
    x.off <- if (merge)
      -0.7
    else 0
  }
  else if (merge)
    warning("'merge = TRUE' has no effect when no line segments are drawn")
  if (has.pch) {
    if (is.character(pch) && !is.na(pch[1L]) && nchar(pch[1L],
                                                      type = "c") > 1) {
      if (length(pch) > 1)
        warning("not using pch[2..] since pch[1L] has multiple chars")
      np <- nchar(pch[1L], type = "c")
      pch <- substr(rep.int(pch[1L], np), 1L:np, 1L:np)
    }
    if (!is.character(pch))
      pch <- as.integer(pch)
  }
  if (is.na(auto)) {
    if (xlog)
      x <- log10(x)
    if (ylog)
      y <- log10(y)
  }
  if (nx == 2) {
    x <- sort(x)
    y <- sort(y)
    left <- x[1L]
    top <- y[2L]
    w <- diff(x)
    h <- diff(y)
    w0 <- w/ncol
    x <- mean(x)
    y <- mean(y)
    if (missing(xjust))
      xjust <- 0.5
    if (missing(yjust))
      yjust <- 0.5
  }
  else {
    h <- (n.legpercol + !is.null(title)) * ychar + yc
    xch1 <- max(xchar)
    w0 <- text.width + (x.intersp + 1) * xch1
    if (mfill)
      w0 <- w0 + dx.fill
    if (do.lines)
      w0 <- w0 + (seg.len + x.off) * xch1
    w <- ncol * w0 + 0.5 * xch1
    if (!is.null(title) && (abs(tw <- strwidth(title, units = "user",
                                               cex = cex) + 0.5 * xchar)) > abs(w)) {
      xextra <- (tw - w)/2
      w <- tw
    }
    if (is.na(auto)) {
      left <- x - xjust * w
      top <- y + (1 - yjust) * h
    }
    else {
      usr <- par("usr")
      inset <- rep_len(inset, 2)
      insetx <- inset[1L] * (usr[2L] - usr[1L])
      left <- switch(auto, bottomright = , topright = ,
                     right = usr[2L] - w - insetx, bottomleft = ,
                     left = , topleft = usr[1L] + insetx, bottom = ,
                     top = , center = (usr[1L] + usr[2L] - w)/2)
      insety <- inset[2L] * (usr[4L] - usr[3L])
      top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] +
                      h + insety, topleft = , top = , topright = usr[4L] -
                      insety, left = , right = , center = (usr[3L] +
                                                             usr[4L] + h)/2)
    }
  }
  if (plot && bty != "n") {
    if (trace)
      catn("  rect2(", left, ",", top, ", w=", w, ", h=",
           h, ", ...)", sep = "")
    rect2(left, top, dx = w, dy = h, col = bg, density = NULL,
          lwd = box.lwd, lty = box.lty, border = box.col)
  }
  # browser()
  xt <- left + xchar + xextra + (w0 * rep.int(0:(ncol - 1),
                                              rep.int(n.legpercol, ncol)))[1L:n.leg] + xExtra2 + xExtra3
  yt <- top - 0.5 * yextra - ymax - (rep.int(1L:n.legpercol,
                                             ncol)[1L:n.leg] - 1 + !is.null(title)) * ychar
  if (mfill) {
    if (plot) {
      if (!is.null(fill))
        fill <- rep_len(fill, n.leg)
      rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox,
            col = fill, density = density, angle = angle,
            border = border)
    }
    xt <- xt + dx.fill
  }
  if (plot && (has.pch || do.lines))
    col <- rep_len(col, n.leg)
  if (missing(lwd) || is.null(lwd))
    lwd <- par("lwd")
  if (do.lines) {
    if (missing(lty) || is.null(lty))
      lty <- 1
    lty <- rep_len(lty, n.leg)
    lwd <- rep_len(lwd, n.leg)
    ok.l <- !is.na(lty) & (is.character(lty) | lty > 0) &
      !is.na(lwd)
    if (trace)
      catn("  segments2(", xt[ok.l] + x.off * xchar, ",",
           yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)")
    if (plot)
      segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len *
                  xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l],
                col = col[ok.l])
    xt <- xt + (seg.len + x.off) * xchar
  }
  if (has.pch) {
    pch <- rep_len(pch, n.leg)
    pt.bg <- rep_len(pt.bg, n.leg)
    pt.cex <- rep_len(pt.cex, n.leg)
    pt.lwd <- rep_len(pt.lwd, n.leg)
    ok <- !is.na(pch)
    if (!is.character(pch)) {
      ok <- ok & (pch >= 0 | pch <= -32)
    }
    else {
      ok <- ok & nzchar(pch)
    }
    x1 <- (if (merge && do.lines)
      xt - (seg.len/2) * xchar
      else xt)[ok]
    y1 <- yt[ok]
    if (trace)
      catn("  points2(", x1, ",", y1, ", pch=", pch[ok],
           ", ...)")
    if (plot)
      points2(x1, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok],
              bg = pt.bg[ok], lwd = pt.lwd[ok])
  }
  xt <- xt + x.intersp * xchar
  if (plot) {
    if (!is.null(title))
      text2(left + w * title.adj, top - ymax, labels = title,
            adj = c(title.adj, 0), cex = cex, col = title.col)
    text2(xt, yt, labels = legend, adj = adj, cex = cex,
          col = text.col, font = text.font)
  }

  # print('cheguei')

  invisible(list(rect = list(w = w, h = h, left = left, top = top),
                 text = list(x = xt, y = yt)))
}



#' @title getCopy
#' @description This pulls the data from fame if necessary, subsets the data to the desired range, and applies any scaling/interpolation.
#' @export
getCopy <- function(dataFrame = NULL, #dataFrame,
                    series = NULL, #series,
                    famePath = NULL, #famePath,
                    # dataType= NULL, #dataType,
                    startDate= NULL, #startDate,
                    endDate= NULL, #endDate,
                    seriesScale= 1, # seriesScale
                    interp = F,
                    seriesX = 'date'
) {

  # if dataFrame & famePath is null, that means we don't have a second or third series.
  # if series is null, that means we don't have a left or a line series.
  if ((is.null(dataFrame) & is.null(famePath)) | is.null(series)) {
    return(NULL)
  }

  # for consistency
  dataFrame <- as.data.frame(dataFrame)


  if (!is.null(famePath)) {
    if (!exists("getfame_dt")) {
      stop("stfm.helper must be loaded for accessing fame.")
    }

    df <- getfame_dt(series,
                     db = famePath)
    df <- as.data.frame(df)
  } else if (!is.null(dataFrame)) {
    df <- subset(dataFrame,select=c(seriesX,series))
    df <- as.data.frame(df)
  } else {
    stop("No data provided.")
  }

  if(is.null(startDate)==TRUE){startDate <- min(df[[seriesX]], na.rm = T)}
  if(is.null(endDate )==TRUE){endDate <- max(df[[seriesX]], na.rm = T)}
  #Allow start date to be specified.  If no start date is specified, startDate defaults to the beginning of data available
  df <- df[df[[seriesX]] >= startDate,]
  df <- df[df[[seriesX]] <= endDate,]


  # Convert series per seriesScale
  for(i in 2:(length(series)+1)){
    df[,i]=df[,i]*(seriesScale)
  }

  if(interp==TRUE){

    df_interp <- tryCatch(na.approx(data.matrix(df)), error = function(cond) {
      warning("Data was not successfully interpolated. Continuing with charting.")
      return(df)
    }) %>% as.data.frame

    df_interp[[seriesX]] <- df[[seriesX]]
    df <- df_interp
  }

  return(df)
}


#' @title setYLims
#' @description This sets the Y-axis limits for the chart given the users inputted yMax, yMin, and yStep parameters. This is done for both the right and left axis at the same time.
#' @export
setYLims <- function(df = NULL,
                     dfLeft = NULL,
                     series = NULL,
                     seriesLeft = NULL,
                     yMax = NULL,
                     yMin = NULL,
                     yStep = NULL,
                     yMaxLeft = NULL,
                     yMinLeft = NULL,
                     yStepLeft = NULL
){ ## Returns a list with all yLimits and steps
  #### Set dynamic y-axis parameters ####
  #Right Y axis
  #Apply dynamic y axis parameter function to max and min values in the first series
  yAxisParameters <- dynamicYAxisParameters(max(df[,c(series)], na.rm = TRUE), min(df[,c(series)], na.rm = TRUE))

  #Set parameters using the vector returned by the function
  if(is.null(yMax)==TRUE){yMax <- yAxisParameters[1]}
  if(is.null(yMin)==TRUE){yMin <- yAxisParameters[2]}
  if(is.null(yStep)==TRUE){yStep <- yAxisParameters[3]}

  #Left Y axis
  if(!is.null(seriesLeft)){
    yAxisParametersLeft <- dynamicYAxisParameters(max(dfLeft[,c(seriesLeft)], na.rm = TRUE), min(dfLeft[,c(seriesLeft)], na.rm = TRUE))

    #Set parameters using the vector returned by the function
    if(is.null(yMaxLeft)==TRUE){yMaxLeft <- yAxisParametersLeft[1]}
    if(is.null(yMinLeft)==TRUE){yMinLeft <- yAxisParametersLeft[2]}
    if(is.null(yStepLeft)==TRUE){yStepLeft <- yAxisParametersLeft[3]}
  }

  #! add this in here instead
  if(is.null(yMaxLeft)==TRUE){yMaxLeft <- yMax}
  if(is.null(yMinLeft)==TRUE){yMinLeft <- yMin}
  if(is.null(yStepLeft)==TRUE){yStepLeft <- yStep}

  return(list('yMin' = yMin, "yMax" = yMax, "yStep" = yStep,
              'yMinLeft' = yMinLeft, 'yMaxLeft' = yMaxLeft, "yStepLeft" = yStepLeft))
}

#' @title lineFormatting
#' @description This assigns line types and line widths if none are supplied.
#' @export
lineFormatting <- function(lineType = NULL,
                           lineWidth = NULL,
                           series = NULL,
                           seriesLeft = NULL) {

  #Finish setting up lineType and lineWidth
  if(is.null(lineType)) {
    lineType <- rep(1, length(series) + ifelse(is.null(seriesLeft), 0, length(seriesLeft)))
  }
  if(is.null(lineWidth)) {
    lineWidth <- rep(1.7, length(series) + ifelse(is.null(seriesLeft), 0, length(seriesLeft)))
  }

  return(list("lineType" = lineType,
              "lineWidth" = lineWidth))
}

#' @title addSeriesBand
#' @description This plots the series band if supplied.
#' @export
addSeriesBand <- function(dataFrame = NULL,
                          seriesBandLower = NULL,
                          seriesBandUpper = NULL,
                          seriesScale = 1,
                          colorBandSeries = NULL,
                          borderBand = NULL,
                          startDate = NULL,
                          endDate = NULL,
                          famePath = NULL){

  plot.data.band <- getCopy(dataFrame = dataFrame,
                            series = c(seriesBandUpper, seriesBandLower),
                            famePath = famePath,
                            # dataType= dataType,
                            startDate= startDate,
                            endDate= endDate,
                            seriesScale= seriesScale,
                            interp = F)


  # Convert series per seriesScaleBand
  for (i in 2:3) {
    plot.data.band[,i] = plot.data.band[,i] * (seriesScale)
  }

  # Define polygon shape
  xshade <- c(plot.data.band$date, rev(plot.data.band$date))
  yshade <- c(plot.data.band[, 2], rev(plot.data.band[, 3]))

  # Define polygon color

  # get color of banded series if provided, else use default
  colorBand <- "#0000FF33"
  if (!is.na(colorBandSeries)) {
    if (dynamicColors) {
      colorBand <- color_wash(colors[which(series == colorBandSeries)], type = "hex")
    } else {
      colorBand <- color_wash(col2rgb(colors)[, which(series == colorBandSeries)], type = "rgb")
    }
  }
  polygon(xshade, yshade, col = colorBand, border = borderBand)
}

#' @title flourish
#' @description This applies recession/custom shading and lineAtZero.
#' @export
flourish <- function(lineAtZero = F,
                     df = NULL,
                     recessions = F,
                     customShading = F,
                     shadeEnd = '',
                     shadeStart = '',
                     colors = NA,
                     yMax = NA,
                     seriesX = 'date'){

  par('xpd' = F)

  #Shade recessions
  if (recessions == TRUE){
    # Actually insert shaded bars
    nber.shade(plotteddates = df$date,
               shadeyval = yMax+1)
  }

  #customizable Shaded region
  if (customShading == TRUE){

    # Actually insert shaded bars
    shadedRegion(plotteddates = df[[seriesX]],
                 shadeyval = yMax+1,
                 shadeStartDate = shadeStart,
                 shadeEndDate =shadeEnd)
  }


  if (lineAtZero==TRUE){
    abline(h=0)
  }

}


#' @title chartLabels
#' @description This creates the legend and title as well as extra labels such as the frequencyLabel, unitsLabel, lastValue, and footnote.
#' @export
chartLabels <- function(chartType,
                        df = NULL,
                        chartTitle = NULL,
                        chartTitleFontSize = NULL,
                        series = NULL,
                        seriesLeft = NULL,
                        legend = NULL,
                        pointType = NULL,
                        lineType = NULL,
                        lineWidth = NULL,
                        legendCol = NULL,
                        legendLocationX = NULL,
                        legendLocationY = NULL,
                        legendFontSize = NULL,
                        legendColPlacement = NULL,
                        legendNames = NULL,
                        footNote = NULL,
                        footNoteFontSize = NULL,
                        footNotePlacement = NULL,
                        xLabelFreq,
                        xTicksBelowAxis,
                        unitsLabel = NULL,
                        colors = NULL,
                        unitsLabelLeft = NULL,
                        frequencyLabel = NULL,
                        axisColorRight = NULL,
                        axisColorLeft = NULL,
                        printLastValue = NULL,
                        lastValueFreq = NULL,
                        lastValue = NULL,
                        lastValueLocationX,
                        lastValueLocationY,
                        lastValuePrelim = NULL,
                        yMax = NULL,
                        yMin = NULL,
                        yStep = NULL,
                        yMaxLeft = NULL,
                        yMinLeft = NULL,
                        yStepLeft = NULL,
                        labelFontSize = NULL,
                        seriesXLabel = ''
){

  set_chart_parameters()

  # assume we want to put a legend iff we provide legend names and legend == T (for now)
  if (!is.null(legendNames) & legend) {

    # quick nonsense to make sure number of legend names lines up with the number of series
    legendNamesFin <- legendNames[1:(length(series) + length(seriesLeft))]

    # defaults that can be overwritten by chartType if needed - Just setting the default here to lineChart's. We will likely define a chartype for everything though
    numLineSeries <- length(series) + length(seriesLeft)
    default_pch <- rep(NA, length(series) + length(seriesLeft))
    default_lty <- rep(lineType, numLineSeries)
    default_lwd <- rep(lineWidth, numLineSeries)
    default_bty <- 'n'

    # number of series to pair with boxes or lines
    if (chartType == 'line') {

    } else if (chartType %in% c('bar', 'stackedBar', 'polygon')) {

      if(is.null(seriesLeft)){
        numBoxSeries <- length(series)
        default_pch <- rep(15, numBoxSeries)

        #numLineSeries <- length(seriesLeft)
        default_lty <- c(rep(NA, numBoxSeries)) #, rep(lineType, numLineSeries))

        default_lwd <- c(rep(NA, numBoxSeries)) #, rep(lineWidth, numLineSeries))
      } else{
        numBoxSeries <- length(series)
        default_pch <- append(rep(15, numBoxSeries), rep(NA, length(seriesLeft)))
        default_lty <- append(rep(NA, numBoxSeries), rep(lineType, length(seriesLeft)))
        default_lwd <- append(rep(NA, numBoxSeries), rep(lineWidth, length(seriesLeft)))
      }


    } else if (chartType == 'scatter') {
      default_bty <- 'o'
      if (is.null(pointType)){
        default_pch <- rep(20, numLineSeries)
      }else{
        default_pch = pointType
      }
      default_lty <- NA
      default_lwd <- NA
    }

    # make legend. have to suppress warnings because for some reason if you specify
    # ncol and it's not a multiple of your number of total series R ah holler
    suppressWarnings(
      legend2(x = placeX(legendLocationX),
              y = placeY(legendLocationY),
              legend = legendNamesFin,
              cex = legendFontSize,
              col = colors,
              xpd = NA,
              inset = .1,
              y.intersp = .85,
              ncol = legendCol,
              bty = default_bty,
              pch = default_pch,
              lty = default_lty,
              lwd = default_lwd,
              seg.len = rep(ifelse(chartType == 'line', 2, 0.5), length(series) + length(seriesLeft)),
              text.col = c(rep(axisColorRight, length(series)), rep(axisColorLeft, length(seriesLeft))),
              text.width = legendColPlacement)
    )
  }
  ### printLastValue
  if (printLastValue==TRUE){

    # has a date been provided?
    lastdate <- if (is.null(lastValue)) last(df$date) else as.Date(lastValue)

    #If a frequency isn't specified, lastValueFreq is set to match frequencyLabel
    if(is.na(lastValueFreq)){
      lastValueFreq <- frequencyLabel
    }

    #Create last value label to match frequency. If it's not monthly, quarterly, or annual, the exact month and day is displayed
    if (lastValueFreq %in% c("Monthly", 'month')) {
      lastValueLabel <- month_abbrevs(lastdate)
    }else if (lastValueFreq %in% c("Quarterly", 'quarter')) {
      lastValueLabel <- paste("Q", format(as.yearqtr(lastdate),  "%q"), sep = "")
    }else if (lastValueFreq %in% c('Annual', 'Yearly', 'year')) {
      lastValueLabel <- format(lastdate, "%Y")
    }else {
      lastValueLabel <- paste(month_abbrevs(lastdate), "\n", format(lastdate, "%d"), sep = "")
    }

    # if preliminary, add an asterisk
    if (lastValuePrelim) {
      lastValueLabel <- stringr::str_c(lastValueLabel, '*')
    }

    text(lastValueLabel, x = placeX(lastValueLocationX), y = placeY(lastValueLocationY), cex = .55)
  }

  ## Create Title
  title(main = chartTitle,
        cex.main = chartTitleFontSize,
        line=paneltitle.line+0.15,font.main=1,
        adj=0)

  ### Units and Freq labels
  mtext(unitsLabel, side = 3, line =0 , adj = 1, outer = FALSE, cex = labelFontSize, col = axisColorRight)
  mtext(unitsLabelLeft, side = 3, line =0 , adj = 0, outer = FALSE, cex = labelFontSize, col = axisColorLeft)

  mtext(frequencyLabel, side = 3, line =-.7 , adj = .08, outer = FALSE, cex = labelFontSize)


  ### seriesX label
  mtext(side = 1, line = 1, text = seriesXLabel, cex = labelFontSize * 1.1)

  ## FootNote ##
  footnote(footNote,
           line= if (!is.null(footNotePlacement)) footNotePlacement else {
             1 + (.125 * !is.na(xLabelFreq)) + (.125 * (!is.na(xLabelFreq) & xLabelFreq != 'year')) +
               .2 * (xTicksBelowAxis) + (.1 * (seriesXLabel != ''))
           },
           cex = footNoteFontSize)

}



#' @title defineXAxisBar
#' @description This defines the X-axis for Bar Charts
#' @export
barChartLabels <- function(bars,
                           df = plot.data.full,
                           chartType,
                           series,
                           series1Length,
                           series2Length,
                           series3Length,
                           series2LabelFreq,
                           series3LabelFreq,
                           lastValuePrelim,
                           chartParametersRight,
                           barLabelFontSize,
                           monthLabelLetters
                           ) {

  par('usr' = chartParametersRight)

  # buffer space: use 5% of available y space in chart
  buffer <- .05 * (chartParametersRight[4] - chartParametersRight[3])

  # list of labels: these will be placed over each bar (group). Default to blank
  lablist <- rep("", series1Length + series2Length + series3Length)

  # add series 2 labels to list
  if (!is.na(series2LabelFreq) & series2Length > 0) {

    # what positions to put the labels in
    num <- seq(from = series1Length + 1, by = 1, length.out = series2Length)

    # add labels to list
    if (is.null(series2LabelFreq)) {

    } else if (series2LabelFreq == 'quarter') {
      lablist[num] <- as.yearqtr(df$date[num]) %>% stringr::str_remove("[:digit:]{4} ")
    } else if (series2LabelFreq == 'month') {
      lablist[num] <- month_abbrevs(df$date[num])
      
      if(monthLabelLetters){
        lablist[num] <- substr(month_abbrevs(df$date[num]),1,1)
      }
      
    } else if (series2LabelFreq %in% c('dai', 'day')) {
      lablist[num] <- substr(df$date[num], 9, 10)
    }

  }

  # add series 3 labels to list
  if (!is.na(series3LabelFreq) & series3Length > 0) {

    # what positions to put the labels in
    num <- seq(from = series1Length + series2Length + 1, by = 1, length.out = series3Length)

    # add labels to list
    if (is.null(series2LabelFreq)) {

    } else if (series3LabelFreq == 'quarter') {
      lablist[num] <- as.yearqtr(df$date[num]) %>% stringr::str_remove("[:digit:]{4} ")
    } else if (series3LabelFreq == 'month') {
      lablist[num] <- month_abbrevs(df$date[num])
      
      if(monthLabelLetters){
        lablist[num] <- substr(month_abbrevs(df$date[num]),1,1)
      }
      
    } else if (series3LabelFreq %in% c('dai', 'day')) {
      lablist[num] <- substr(df$date[num], 9, 10)
    }

  }

  # asterisk if last value
  if (lastValuePrelim) {
    lablist[length(lablist)] <- stringr::str_c(lablist[length(lablist)], '*')
  }

  # x positions of labels: this is easy
  ex <- colMeans(bars)

  # y positions of labels; first, get the positive and negative heights of
  # each bar.  Then condense into vectors (this will depend on whether the
  # bars are stacked or not).  Finally decide whether to place on top or bottom.
  why.positive <- as.matrix(df[, series], ncol = length(series))
  why.negative <- as.matrix(df[, series], ncol = length(series))
  for (i in 1:length(series)) {
    why.positive[, i] <- pmax(why.positive[, i], 0)
    why.negative[, i] <- pmin(why.negative[, i], 0)
  }

  # if we have multiple series, stack
  if (length(series) > 1) {
    if (chartType == 'bar') {
      why.positive <- apply(why.positive, 1, max, na.rm = T)
      why.negative <- apply(why.negative, 1, min, na.rm = T)
    } else if (chartType == 'stackedBar') {
      why.positive <- apply(why.positive, 1, function(x) sum(x[!is.na(x) & x > 0]))
      why.negative <- apply(why.negative, 1, function(x) sum(x[!is.na(x) & x > 0]))
    }
  }

  top_labels <- (why.positive >= abs(why.negative))

  # note: if we want to only put labels above bars, then just make why equal to why.positive + buffer.
  # also have to account for NAs which is kinda annoying
  why <- sapply(1:length(top_labels), function(i) {
    if (is.na(top_labels[i]) | top_labels[i]) {
      coalesce(why.positive[i], 0) + buffer
    } else {
      coalesce(why.negative[i], 0) - buffer
    }
  })

  # add labels to bars
  for (i in 1:length(lablist)) {
    text(x = ex[i], y = why[i], labels = lablist[i], cex = barLabelFontSize)
  }

}


