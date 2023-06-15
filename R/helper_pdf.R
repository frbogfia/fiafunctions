#' @title Set PDF Parameters
#' @description newPage can be used to set FIA's standard PDF parameters. It sets the outer margins (omi) as (bottom = 0.275", left = 0.2", top = 0.65", right = 0.5") and the margin size (mai) as (bottom = 3.5", left = 3", top = 3.5", right = 3").
#' @export
newPage <- function(){
  par(omi=c(0.275,0.2,0.65,0.5), mai=c(3.5,3,3.5,3))
}
 
#' @title Set Graphical Parameters
#' @description chartLocation specifies your chart location. Parameters can be set by specifying standard locations (eg. topLeft, topRight) or by specifying coordinates of points.
#'
#' @param location Character variable. Six equal-sized graphs (three rows, two columns): top/middle/bottom, left/right (eg. topLeft, middleRight). Three equal-sized graphs (three rows): top/middle/bottom third (eg. topThird). Two equal-sized graphs (two rows) top/bottom half (eg. topHalf). Six unequal sized graphs (three rows, two columns, left-hand side 2/3rds, right-hand side 1/3rd): top/middle/bottom left2/right2 (eg. tL2, tL_big, tR_small). You can specify either in camelCase, lowercase, nn, nN, NN, lower case.
#' @param margin A numerical vector c(bottom, left, top, right). The default is c(4, 2, 3, 3).
#' @param x0/y0 coordinates of points *from* which to draw the chart. X is from left to right. Y is from bottom to top. Points are based off of the proportion of the PDF page. Eg. x0 = 0, x1 = 1/2 would be the left half of the page.
#' @param x1/y1 coordinates of points *to* which to draw the chart.
#' @param newOverride Boolean variable which by default is FALSE. If TRUE and the location provided is not in the top left portion (eg. topLeft, topThird), it will clean the frame before drawing (aka start a new page). If you provided coordinates, FALSE will start a new page and TRUE will draw on the same page.
#' @export
chartLocation <- function(location = '',
                          margin = c(4, 2, 3, 3),
                          x0 = NULL,
                          x1 = NULL,
                          y0 = NULL,
                          y1 = NULL,
                          newOverride = FALSE){

  # Top left (1/6)
  if (newOverride == FALSE & (location == 'topLeft' | location == 'topleft' | location == 'tl' | location == 'tL' | location == 'TL' | location == 'top left')){
    par(fig=c(0, 1/2, 2/3, 1), mar=margin) }

  else if (newOverride == TRUE & (location == 'topLeft' | location == 'topleft' | location == 'tl' | location == 'tL' | location == 'TL' | location == 'top left')){
    par(fig=c(0, 1/2, 2/3, 1), mar=margin, new = TRUE) }

  # Top right (1/6)
  else if (newOverride == FALSE & (location == 'topRight' | location == 'topright' | location == 'tr' | location == 'tR' | location == 'TR' | location == 'top right')){
    par(fig=c(1/2, 1, 2/3, 1), mar=margin, new = TRUE) }

  else if (newOverride == TRUE & (location == 'topRight' | location == 'topright' | location == 'tr' | location == 'tR' | location == 'TR' | location == 'top right')){
    par(fig=c(1/2, 1, 2/3, 1), mar=margin) }

  # Middle left (1/6)
  else if (newOverride == FALSE & (location == 'middleLeft' | location == 'middleleft' | location == 'ml' | location == 'mL' | location == 'ML' | location == 'middle left')){
    par(fig=c(0, 1/2, 1/3, 2/3), mar=margin, new = TRUE) }

  else if (newOverride == TRUE & (location == 'middleLeft' | location == 'middleleft' | location == 'ml' | location == 'mL' | location == 'ML' | location == 'middle left')){
    par(fig=c(0, 1/2, 1/3, 2/3), mar=margin) }

  # Middle right (1/6)
  else if (newOverride == FALSE & (location == 'middleRight' | location == 'middleright' | location == 'mr' | location == 'mR' | location == 'MR' | location == 'middle right')){
    par(fig=c(1/2, 1, 1/3, 2/3), mar=margin, new = "TRUE") }

  else if (newOverride == TRUE & (location == 'middleRight' | location == 'middleright' | location == 'mr' | location == 'mR' | location == 'MR' | location == 'middle right')){
    par(fig=c(1/2, 1, 1/3, 2/3), mar=margin) }

  # Bottom left (1/6)
  else if (newOverride == FALSE & (location == 'bottomLeft' | location == 'bottomleft' | location == 'bl' | location == 'bL' | location == 'BL' | location == 'bottom left')){
    par(fig=c(0, 1/2, 0, 1/3), mar=margin, new = "TRUE") }

  else if (newOverride == TRUE & (location == 'bottomLeft' | location == 'bottomleft' | location == 'bl' | location == 'bL' | location == 'BL' | location == 'bottom left')){
    par(fig=c(0, 1/2, 0, 1/3), mar=margin) }

  # Bottom right (1/6)
  else if (newOverride == FALSE & (location == 'bottomRight' | location == 'bottomright' | location == 'br' | location == 'bR' | location == 'BR' | location == 'bottom right')){
    par(fig=c(1/2, 1, 0, 1/3), mar=margin, new = "TRUE") }

  else if (newOverride == TRUE & (location == 'bottomRight' | location == 'bottomright' | location == 'br' | location == 'bR' | location == 'BR' | location == 'bottom right')){
    par(fig=c(1/2, 1, 0, 1/3), mar=margin) }


  # Top third (1/3)
  else if (newOverride == FALSE & (location == 'topThird' | location == 'topthird' | location == 'tt' | location == 'tT' | location == 'TT' | location == 'top third')){
    par(fig=c(0, 1, 2/3, 1), mar=margin) }

  else if (newOverride == TRUE & (location == 'topThird' | location == 'topthird' | location == 'tt' | location == 'tT' | location == 'TT' | location == 'top third')){
    par(fig=c(0, 1, 2/3, 1), mar=margin, new = "TRUE") }

  # Middle third (1/3)
  else if (newOverride == FALSE & (location == 'middleThird' | location == 'middlethird' | location == 'mt' | location == 'mT' | location == 'MT' | location == 'middle third')){
    par(fig=c(0, 1, 1/3, 2/3), mar=margin, new = "TRUE") }

  else if (newOverride == TRUE & (location == 'middleThird' | location == 'middlethird' | location == 'mt' | location == 'mT' | location == 'MT' | location == 'middle third')){
    par(fig=c(0, 1, 1/3, 2/3), mar=margin) }

  # Bottom third (1/3)
  else if (newOverride == FALSE & (location == 'bottomThird' | location == 'bottomthird' | location == 'bt' | location == 'bT' | location == 'BT' | location == 'bottom third')){
    par(fig=c(0, 1, 0, 1/3), mar=margin, new = "TRUE") }

  else if (newOverride == TRUE & (location == 'bottomThird' | location == 'bottomthird' | location == 'bt' | location == 'bT' | location == 'BT' | location == 'bottom third')){
    par(fig=c(0, 1, 0, 1/3), mar=margin) }


  # Top half (1/2)
  else if (newOverride == FALSE & (location == 'topHalf' | location == 'tophalf' | location == 'th' | location == 'tH' | location == 'TH' | location == 'top half')){
    par(fig=c(0, 1, 1/2, 1), mar=margin) }

  else if (newOverride == TRUE & (location == 'topHalf' | location == 'tophalf' | location == 'th' | location == 'tH' | location == 'TH' | location == 'top half')){
    par(fig=c(0, 1, 1/2, 1), mar=margin, new = "TRUE") }

  # Bottom half (1/2)
  else if (newOverride == FALSE & (location == 'bottomHalf' | location == 'bottomhalf' | location == 'bh' | location == 'bH' | location == 'BH' | location == 'bottom half')){
    par(fig=c(0, 1, 0, 1/2), mar=margin, new = "TRUE") }

  else if (newOverride == TRUE & (location == 'bottomHalf' | location == 'bottomhalf' | location == 'bh' | location == 'bH' | location == 'BH' | location == 'bottom half')){
    par(fig=c(0, 1, 0, 1/2), mar=margin) }


  # Top left 2/3rds
  else if (newOverride == FALSE & (location == 'tL_big' | location == 'topLeft2' | location == 'topleft2' | location == 'tl2' | location == 'tL2' | location == 'TL2' | location == 'top left2' | location == 'top left 2')){
    par(fig=c(0, 2/3, 2/3, 1), mar=margin) }

  else if (newOverride == TRUE & (location == 'tL_big' | location == 'topLeft2' | location == 'topleft2' | location == 'tl2' | location == 'tL2' | location == 'TL2' | location == 'top left2' | location == 'top left 2')){
    par(fig=c(0, 2/3, 2/3, 1), mar=margin, new = "TRUE") }

  # Top right 1/3
  else if (newOverride == FALSE & (location == 'tR_small' | location == 'topRight2' | location == 'topright2' | location == 'tr2' | location == 'tR2' | location == 'TR2' | location == 'top right2' | location == 'top right 2')){
    par(fig=c(2/3, 1, 2/3, 1), mar=margin, new = "TRUE") }

  else if (newOverride == TRUE & (location == 'tR_small' | location == 'topRight2' | location == 'topright2' | location == 'tr2' | location == 'tR2' | location == 'TR2' | location == 'top right2' | location == 'top right 2')){
    par(fig=c(2/3, 1, 2/3, 1), mar=margin) }

  # Middle left 2/3
  else if (newOverride == FALSE & (location == 'mL_big' | location == 'middleLeft2' | location == 'middleleft2' | location == 'ml2' | location == 'mL2' | location == 'ML2' | location == 'middle left2' | location == 'middle left 2')){
    par(fig=c(0, 2/3, 1/3, 2/3), mar=margin, new = "TRUE") }

  else if (newOverride == TRUE & (location == 'mL_big' | location == 'middleLeft2' | location == 'middleleft2' | location == 'ml2' | location == 'mL2' | location == 'ML2' | location == 'middle left2' | location == 'middle left 2')){
    par(fig=c(0, 2/3, 1/3, 2/3), mar=margin) }

  # Middle right 1/3
  else if (newOverride == FALSE & (location == 'mR_small' | location == 'middleRight2' | location == 'middleright2' | location == 'mr2' | location == 'mR2' | location == 'MR2' | location == 'middle right2' | location == 'middle right 2')){
    par(fig=c(2/3, 1, 1/3, 2/3), mar=margin, new = "TRUE") }

  else if (newOverride == TRUE & (location == 'mR_small' | location == 'middleRight2' | location == 'middleright2' | location == 'mr2' | location == 'mR2' | location == 'MR2' | location == 'middle right2' | location == 'middle right 2')){
    par(fig=c(2/3, 1, 1/3, 2/3), mar=margin) }

  # Bottom left 2/3
  else if (newOverride == FALSE & (location == 'bL_big' | location == 'bottomLeft2' | location == 'bottomleft2' | location == 'bl2' | location == 'bL2' | location == 'BL2' | location == 'bottom left2' | location == 'bottom left 2')){
    par(fig=c(0, 2/3, 0, 1/3), mar=margin, new = "TRUE") }

  else if (newOverride == TRUE & (location == 'bL_big' | location == 'bottomLeft2' | location == 'bottomleft2' | location == 'bl2' | location == 'bL2' | location == 'BL2' | location == 'bottom left2' | location == 'bottom left 2')){
    par(fig=c(0, 2/3, 0, 1/3), mar=margin) }

  # Bottom right 1/3
  else if (newOverride == FALSE & (location == 'bR_small' | location == 'bottomRight2' | location == 'bottomright2' | location == 'br2' | location == 'bR2' | location == 'BR2' | location == 'bottom right2' | location == 'bottom right 2')){
    par(fig=c(2/3, 1, 0, 1/3), mar=margin, new = "TRUE") }

  else if (newOverride == TRUE & (location == 'bR_small' | location == 'bottomRight2' | location == 'bottomright2' | location == 'br2' | location == 'bR2' | location == 'BR2' | location == 'bottom right2' | location == 'bottom right 2')){
    par(fig=c(2/3, 1, 0, 1/3), mar=margin) }


  # Freestyle
  else if (newOverride == FALSE & location == ''){
    par(fig=c(x0, x1, y0, y1), mar=margin)}

  else if (newOverride == TRUE & location == ''){
    par(fig=c(x0, x1, y0, y1), mar=margin, new = "TRUE") }

}

 
