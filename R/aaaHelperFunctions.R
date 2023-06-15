 #' @title Place Function
#' @description This does something.
#' @export
#Function that determines the place of a number
place <- function(x) {
     #If x is not a decimal
     if(x >= 1 | x <= -1) {
          #Stores the number of digits in the absolute value of x (so we don't count negative signs)
          place <- nchar(trunc(abs(x)))
          #Convert to powers of 10. (i.e. one digit is 1, two is 10, three is 100, etc.)
          place <- 10^(place-1)
     #If x is a decimal
     }else{
          #Count the number of zeroes after the decimal (i.e. 0.001 has 2 zeroes)
          place <- attr(regexpr("(?<=\\.)0+", x, perl = TRUE), "match.length")
          #RegExpr counts no matches as -1, so adjust -1 to zero
          if(place == -1){place <- 0}
          #Convert to powers of 10. (i.e. no zeroes is 0.1, one zero is 0.01, etc.)
          place <- 10^-(place+1)
     }
return(place)
}



#' @title Dynamic Axes
#' @description This does something else.
#' @export
#Function to dynamically set Y axis max, min and step. It takes the maximum and minimum values in the data as arguments and returns a vector with max, min, and step.
dynamicYAxisParameters <- function(dataMax, dataMin) {

     #Apply function to find places of maximum and minimum values. Multiply by 1.1 to account for extra space at the top
     maxPlace <- place(dataMax*1.1)
     minPlace <- place(dataMin*1.1)

     #Round up data max to nearest value in that place to get max y value
     yHeightMax <- plyr::round_any(dataMax*1.1, maxPlace, f = ceiling)

     #If the data min is greater than 0, then automatically set it to zero. If it's less than zero, round it down to the nearest value in the min place to get min y value
     yHeightMin <- ifelse(dataMin >= 0, 0, plyr::round_any(dataMin*1.1, minPlace, f = floor))

     #Create the dynamicstep.

     #Take larger of place values for max or min
     biggestPlace <- max(c(maxPlace,minPlace))

     #If the
     if(biggestPlace > maxPlace) {yHeightMax <- biggestPlace}
     if(biggestPlace > minPlace & yHeightMin < 0) {yHeightMin <- -1 * biggestPlace}

     #Find the range of the y axis, used to dynamically set the ystep
     yRange <- yHeightMax - yHeightMin

     #Find how many time the place value can fit into the range
     stepSplit <- yRange/biggestPlace
     #If it's two or less, set step to place value divided by 5
     if(stepSplit <= 2) {
          dynamicStep <- biggestPlace/5
     #If it's five or less, set step to place value divided by 2
     } else if(stepSplit<=5){
          dynamicStep <- biggestPlace/2
     #If it's greater than 5, use place value as step
     } else {dynamicStep <- biggestPlace}
      
     
     #Store max, min, and step in one vector
     yAxisParameters <- c(yHeightMax, yHeightMin, dynamicStep)
#Return parameters
return (yAxisParameters)
}

#' @title Eventline
#' @description Creates a vertical line with label. It uses graphics::segments() and graphics::text()
#' @export
#' 
#' @param date 
#' @param lineHeight Numeric. Height of line as percent of chart area.
#' @param textHeight Numeric. Vertical location of label as percent of chart area. If oneLine == FALSE, default is lineHeight + 0.05. If oneLine == TRUE, default is lineHeight + 0.09.
#' @param cex Default is 0.75.
#' @param adj Numeric vector which specify the x and y adjustment ('justification') of the label. With 0 for left/bottom and 1 for right/top. Default is 0.5 (centered).
#' @param pos Numeric. A position specifier for the label. Overrides any adj value. Values of 1, 2, 3, and 4, correspond to below, to the left of, above, and to the right of.
#' @param offset Numeric. If pos is specified, controls the distance of the text label from the specified coordinate in fractions of a character width.
#' @param lwd Numeric vector. Default is 1.
#' @param lty Vector. Can be a mixture of numeric and character (eg. 1, "solid", 2, "dashed", 3, "dotted"). Default is 1 (solid).
#' @param oneLine Boolean. Changes the default lineHeight and default distance between lineHeight and textHeight.
#' @param label Character vector or expression specifying the text to be written.
#' @param lastFOMCdate Boolean. Default is FALSE.
#' 
eventLine <- function(
  date = NA, 
  lineHeight = NULL, 
  textHeight = NULL,
  cex = opt$label.cex, 
  adj = NULL, 
  pos = NULL, 
  offset = NULL, 
  lwd = 1, 
  lty = 1, 
  oneLine = FALSE, 
  label, 
  lastFOMCdate = FALSE
){
  

    if (lastFOMCdate == TRUE) {
        date = get_recent_fomc(count=1)
        label = c("Last FOMC")
    }

    xPos = as.numeric(as.Date(date))

    if (is.null(adj)) {
            adj = 0.5
        }
    
    if (is.null(lineHeight)) {
        if (oneLine) {
            lineHeight = 0.91
        }
        else {
            lineHeight = 0.82
        }
    }
    if (is.null(textHeight)) {
        if (oneLine) {
            textHeight = lineHeight + 0.05
        }
        else {
            textHeight = lineHeight + 0.09
        }
    }
    
    if (!is.null(pos) & is.null(offset)){
      offset = 0
    }
    
    label = label
    segments(xPos, par("usr")[3], xPos, lineHeight * (par("usr")[4] -
        par("usr")[3]) + par("usr")[3], lty = lty, lwd = lwd)
    text(xPos, textHeight * (par("usr")[4] - par("usr")[3]) +
        par("usr")[3], label, cex = cex, adj = adj, pos = pos, offset = offset)
}


#' @title Eventline
#' @description This does something else.
eventLineDate <- function (date = fomcdates(-1), lineHeight = NULL, textHeight = NULL,
                           cex = 0.4, adj = 0.5, lwd = 1, oneLine = FALSE,label){

    xPos = as.numeric(as.Date(date))

    if (is.null(adj)) {
        if (((par("usr")[2] - xPos)/(par("usr")[2] - par("usr")[1])) <
            1/8) {
            adj = 0.9
        }
        else {
            adj = 0.5
        }
    }
    if (is.null(lineHeight)) {
        if (oneLine) {
            lineHeight = 0.91
        }
        else {
            lineHeight = 0.91
        }
    }
    if (is.null(textHeight)) {
        if (oneLine) {
            textHeight = lineHeight + 0.05
        }
        else {
            textHeight = lineHeight + 0.03
        }
    }
    label = label
    segments(xPos, par("usr")[3], xPos, lineHeight * (par("usr")[4] -
                                                          par("usr")[3]) + par("usr")[3], lty = 1, lwd = lwd)
    text(xPos, textHeight * (par("usr")[4] - par("usr")[3]) +
             par("usr")[3], label, cex = cex, adj = adj)
}


#' @title Footnote
#' @description This makes the footnote
#' @export
footnote <- function(note, indent.spaces = NULL, line=1, cex = opt$foot.cex, font = par("font"))
{

    xends=c(par("usr")[1:2])

    cex.width = cex/par()$cex

    # Make sure the note parameter is a vector #
    if(is.vector(note) == F) {
        note = c(note)
    }
    # Create the indentation string #
    if(is.null(indent.spaces)) {
        if(font == 1) {
            indentspace = "    "
        }
        else if(font == 2) {
            indentspace = "  "
        }
        else {
            indentspace = "     "
        }
    }
    else {
        indentspace = paste(rep(" ", indent.spaces), collapse = "")
    }
    plotwidth = xends[2]-xends[1]

    # Loop through the vector of note #
    for(i in 1:length(note)) {
        string = paste(indentspace, note[i], sep = "")
        while(strwidth(string, units="user", cex = cex.width)>plotwidth) {
            words = unlist(strsplit(string," "))
            index = length(words) -1
            if(index == 1){
                print(plotwidth)
                return(1)
            }
            newstring = paste(words[1:index],sep="",collapse=" ")
            while(strwidth(newstring, units="user", cex = cex.width)>plotwidth) {
                index = index - 1

                if(index == 1){
                    print(plotwidth)
                    print("\n")
                    print(strwidth(newstring))
                    print(newstring)
                    return(1)
                }
                newstring = paste(words[1:index],sep="",collapse=" ")

            }

            mtext(newstring, side = 1, line = line, cex = cex,
                  adj = 0, font = font, at=xends[1])

            line = line + 1.0*cex
            string = paste(words[(index+1):length(words)],sep= "",collapse=" ")
        }
        mtext(string, side = 1, line = line, cex = cex, adj = 0, font
              = font, at=xends[1])
        line = line + 1*cex
    }
}


#' @title NBER Shading
#' @description This shades recessions
#' @export
nber.shade <- function(plotteddates,
                       shadeyval,
                       shadecol = "gray88",
                       bottom   = -10000
){


    ## Shade for each time period

    for(i in 1:nrow(shadedata)){

        shadestartdate <- as.Date(shadedata$startdate[i], format="%m/%d/%Y")
        shadeenddate <-  as.Date(shadedata$enddate[i], format="%m/%d/%Y")

        shadedates <- plotteddates[plotteddates >= shadestartdate &
                                       plotteddates <= shadeenddate]

        # need a y value for the shade to stop at (max of the plot)
        shadey <- rep(shadeyval,length(shadedates))

        lines(shadedates,
              shadey,
              col=shadecol,
              type="l")

        par(new=TRUE)

        zero <- as.matrix(rep(bottom,length(shadey-shadey)))


        polygon(c(shadedates[1:length(shadedates)],
                  shadedates[length(shadedates):1]),
                c(zero[1:length(zero)],shadey[length(shadey):1]),
                col=shadecol,
                border=shadecol,
                lty="solid"
        )

        par(new=TRUE)

    }
}


#' @title plothook
#' @description makes hooked box
#' @export
plotHookBox <- function (col = "black", lwd = par("lwd"))
{
    box(bty = "u")
    lines(grconvertX(c(0.018, 0, 0, 1, 1, 0.982), "npc", "user"),
          grconvertY(c(1, 1, 0, 0, 1, 1), "npc", "user"), col = col,
          lwd = lwd + 0.1)
}




#' @title chart parameters
#' @description sets chart's parameters
#' @export
set_chart_parameters <- function (..., envir = parent.frame(), set_colors = TRUE, set_sizes = TRUE)
{
    env <- as.environment(envir)
    if (set_colors) {
        my.colors = c("black", "red", "blue", "green", "orange",
                      "purple", "HotPink", "yellow", "sienna", "DarkTurquoise",
                      "gray50", "firebrick", "SteelBlue", "SeaGreen", "OrangeRed",
                      "MediumPurple4", "Maroon3", "DarkGoldenrod1", "brown",
                      "Turquoise", "gray60", "red3", "DodgerBlue", "ForestGreen",
                      "coral", "DarkViolet", "DeepPink", "LightGoldenrod3",
                      "peru", "MediumAquamarine", "gray70", "tomato3",
                      "SkyBlue", "chartreuse", "orange3", "MediumPurple",
                      "pink3", "gold", "SandyBrown", "Aquamarine", "white",
                      "IndianRed", "LightBlue", "DarkOliveGreen3", "orange2",
                      "violet", "LightPink", "LightGoldenrod1", "Tan")
        assign("color_palate", my.colors, envir = env, ...)
    }
    if (set_sizes) {
        assign("axis.cex", 0.75, envir = env, ...)
        assign("axis.units.cex", 0.7, envir = env, ...)
        assign("bullet.cex", 0.95, envir = env, ...)
        assign("chartlabel.adj", 0.04, envir = env, ...)
        assign("chartlabel.cex", 0.75, envir = env, ...)
        assign("chartlabel.line", -1, envir = env, ...)
        assign("exhibittitle.cex", 0.96, envir = env, ...)
        assign("fomc.cex", 0.62, envir = env, ...)
        assign("footnote.cex", 0.6, envir = env, ...)
        assign("footnote.line", 1.1, envir = env, ...)
        assign("key.cex", 0.75, envir = env, ...)
        assign("key.loc", c(0.25, -0.3), envir = env, ...)
        assign("paneltitle.cex", 0.8, envir = env, ...)
        assign("paneltitle.line", 0.85, envir = env, ...)
        assign("paneltitle.line.add", 0.95, envir = env, ...)
        assign("splot.cex", 0.8, envir = env, ...)
        assign("tick.length", 0.03, envir = env, ...)
        assign("table.cex", 0.85, envir = env, ...)
        assign("ylab.cex", 0.85, envir = env, ...)
    }
    invisible(NULL)
}


#' @title Month Abbreviations
#' @description Creates Month Abbreviations
#' @export
month_abbrevs <- function (month_number)
{
    abbrevs <- c("Jan.", "Feb.", "Mar.", "Apr.", "May", "June",
                 "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.")
    if (lubridate::is.Date(month_number))
        month_number <- month(month_number)
    return(abbrevs[month_number])
}






#' @title event lines
#' @description This creates event lines inthe chart
#' @export
eventLine <- function (date = NA, lineHeight = NULL, textHeight = NULL,
                       cex = opt$label.cex, adj = NULL, lwd = 1, oneLine = FALSE, label, lastFOMCdate = FALSE){

    if (lastFOMCdate == TRUE) {
        date = get_recent_fomc(count=1)
        label = c("Last FOMC")
    }

    xPos = as.numeric(as.Date(date))

    if (is.null(adj)) {
        if (((par("usr")[2] - xPos)/(par("usr")[2] - par("usr")[1])) <
            1/8) {
            adj = 0.9
        }
        else {
            adj = 0.5
        }
    }
    if (is.null(lineHeight)) {
        if (oneLine) {
            lineHeight = 0.91
        }
        else {
            lineHeight = 0.82
        }
    }
    if (is.null(textHeight)) {
        if (oneLine) {
            textHeight = lineHeight + 0.05
        }
        else {
            textHeight = lineHeight + 0.09
        }
    }
    label = label
    segments(xPos, par("usr")[3], xPos, lineHeight * (par("usr")[4] -
        par("usr")[3]) + par("usr")[3], lty = 1, lwd = lwd)
    text(xPos, textHeight * (par("usr")[4] - par("usr")[3]) +
        par("usr")[3], label, cex = cex, adj = 0.5)
}




#' @title General Shading
#' @description This shades recessions
#' @export
shadedRegion <- function(plotteddates,
                       shadeyval,
                       shadeStartDate,
                       shadeEndDate,
                       shadecol = "gray88",
                       shadeColBorder = "gray88",
                       bottom   = -10000
){

    ## Bring in important dates file. Defaults to recession
    shadedata <-data.frame(startdate = as.Date(c(shadeStartDate)), enddate = as.Date(c(shadeEndDate)))

    ## Shade for each time period

    for(i in 1:nrow(shadedata)){

        shadestartdate <- as.Date(shadedata$startdate[i], format="%m/%d/%Y")
        shadeenddate <-  as.Date(shadedata$enddate[i], format="%m/%d/%Y")

        shadedates <- plotteddates[plotteddates >= shadestartdate &
                                       plotteddates <= shadeenddate]

        # need a y value for the shade to stop at (max of the plot)
        shadey <- rep(shadeyval,length(shadedates))

        lines(shadedates,
              shadey,
              col=shadecol,
              type="l")

        par(new=TRUE)

        zero <- as.matrix(rep(bottom,length(shadey-shadey)))


        polygon(c(shadedates[1:length(shadedates)],
                  shadedates[length(shadedates):1]),
                c(zero[1:length(zero)],shadey[length(shadey):1]),
                col=shadecol,
                border=shadeColBorder,
                lty="solid"
        )

        par(new=TRUE)

    }
}


#' @title automatic color selector
#' @description This selects color schemes based on the number of series
#' @export
colorSelector <- function(numberOfSeries,
                         colorScheme ="paulTol"){

    if (colorScheme == "paulTol") {
        colorList <- list(tol1qualitative=c("#4477AA"),
            tol2qualitative=c("#4477AA", "#CC6677"),
            tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677"),
            tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
            tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
            tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499"),
            tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499"),
            tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499"),
            tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499"),
            tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
            tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
            tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499"))
    }
    colorListSelected <- colorList[[numberOfSeries]]
    return(colorListSelected)

}

#' @title nonsense
#' @description This takes a color and creates a washout, lighter version
#'@export
color_wash <- function(color_code, type = "hex") {

    if (type == "hex") {

        print(color_code)
        color_code <- gsub("#", "", color_code)

        match <-  c('0' = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8,
                    '9' = 9, 'A' = 10, 'B' = 11, 'C' = 12, 'D' = 13, 'E' = 14, 'F' = 15, 'a' = 10,
                    'b' = 11, 'c' = 12, 'd' = 13, 'e' = 14, 'f' = 15)

        match_rev <- names(match)
        names(match_rev) <- unname(match)

        dec_code <- rep(0, 6)
        for (i in 1:6) {
            dec_code[i] = as.numeric(match[substr(color_code, i, i)])
        }
        print(dec_code)

        # make lighter
        new_dec_code <- floor(dec_code * 1.5)

        new_dec_code[new_dec_code > 15] <- 15

        # turn into rgb
        rgb_code <- c('red' = new_dec_code[1] * 16 + new_dec_code[2],
                      'green' = new_dec_code[3] * 16 + new_dec_code[4],
                      'blue' = new_dec_code[5] * 16 + new_dec_code[6])
        rgb_code <- rgb_code / 255

        print(rgb_code)
        return(rgb(rgb_code[1], rgb_code[2], rgb_code[3], alpha = .15))

    } else { 

        color_code <- color_code * 1.5
        color_code[color_code > 255] <- 255
        color_code <- color_code / 255

        return(rgb(color_code[1], color_code[2], color_code[3], alpha = .15))
    }


}
