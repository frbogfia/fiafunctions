#' @title DaTable
#' @description This creates a data table
#' @export
datable <- function(dataFrame, series, columnNames = NULL, chartTitle = '', footNote = '',
                    # how much white space to leave on each side of the table
                    marLeft = .01, marRight = .01, marTop = .01, marBottom = .01,
                    # shift entire table left/down, use negative values to shift up/right
                    shiftLeft = .05, shiftDown = 0,
                    # shift the first column/row only.  Use colSpacing instead.
                    firstColShift = 0, firstRowShift = 0,
                    # what percentage of the table width each column should take up,
                    # a vector the length of the number of columns you have
                    colSpacing = NULL,
                    # text sizes
                    fontSize = .5, colFontSize = .5, chartTitleFontSize = .85,
                    # centering the names above the columns (adj)
                    colAlignment = 0, chartTitleAdj = 0,
                    # specify what colors to use for each entry.  MUST be a matrix
                    # equal in size to the size of dataFrame[, series].
                    colorMatrix = NULL
) {

  ting <- as.data.frame(dataFrame)[, series]

  # if column names are not provided, then use the names of the data frame
  if (is.null(columnNames)) columnNames <- names(ting)

  # if no colors provided, use black for everything
  if (is.null(colorMatrix)) colorMatrix <- matrix(nrow = nrow(ting), ncol = ncol(ting), 'black')

  # number of rows and columns, plus buffers
  num_rows <- nrow(ting) + 1 + firstRowShift
  num_cols <- ncol(ting)

  # rows: spread evenly across the page
  row_space <- (1 - marTop - marBottom) / num_rows

  # actual y values of row starts
  why <- 1 - (shiftDown + marTop + row_space * (1:num_rows - 1))


  # column spacing: if not specified, assume equal widths
  if (is.null(colSpacing)) colSpacing <- rep(1 / num_cols, num_cols)

  # scale to sum to one in case user didn't follow that rule
  colSpacing <- colSpacing / sum(colSpacing)

  # column starts
  colSpacing <- dplyr::lag(colSpacing, default = 0)

  # how far to hop over to place each column
  col_hops <- cumsum(colSpacing) * (1 - marLeft - marRight - shiftLeft)

  # actual x values of column starts
  ex <- shiftLeft + marLeft + col_hops
  ex[1] <- ex[1] + firstColShift

  plot.new()

  # add text
  for (i in 1:ncol(ting)) {

    for (j in 1:nrow(ting)) {

      text(ex[i], why[j + 1 + firstRowShift], ting[j, i], cex = fontSize, adj = 0, col = colorMatrix[j, i])

    }

  }

  # add column headers
  for (i in 1:ncol(ting)) {

    text(ex[i], why[1] , columnNames[i], cex = colFontSize, adj = colAlignment, font = 2)

  }

  # add box and ting
  plotHookBox()

  title(main = chartTitle,
        cex.main = chartTitleFontSize,
        # cex.main = .85,
        line=0.5,
        font.main=1,
        adj= chartTitleAdj)

  footnote(footNote)


}
