#' @title Pick Colors
#' @description This assigns colors.
#' @export
pickColors <- function(series = NULL,
                       seriesLeft = NULL,
                       colors = NULL,
                       dynamicColors = FALSE,
                       palette = NULL,
                       scheme = NULL) {

  # if colors is provided, return immediately.
  # otherwise default to opt colors
  if (!is.null(colors)) {
    return(colors)
  } else {
    colors <- opt$colors
  }

  # Defaults to opt$colors (black, firebrick, steelblue etc.)
  # If dynamicColors = T & is.null(palette) & is.null(scheme), uses paulTol selections
  # If !is.null(palette) & !is.null(scheme)
    # palette
        # scheme:

    # sequential
        # YlGn
        # BlPu
        # GnBl
        # OrRd
        # PuBlGn
        # YlOrRd
        # Bl
        # Gn
        # Gy
        # Pu
        # Rd
    # diverging,
        # BrBG
        # PiYG
        # PRGn
        # PuOr
        # RdBl
        # RdYlBl
    # qualitative
        # paired
        # dark
        # light
    # rainbow

  if (dynamicColors==FALSE & is.null(palette) & is.null(scheme)) {
  #### Set opt$colors ####
  colors <- opt$colors
  return(colors)
  } else {


  if (dynamicColors==TRUE & is.null(palette) & is.null(scheme)) {
    lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

    colorSelector_dynamic <- function(lengthOfSeries,
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

      colorListSelected <- colorList[[lengthOfSeries]]
      return(colorListSelected)
    }
    # Get colors
    colors <- colorSelector_dynamic(lengthOfSeries, colorScheme ="paulTol")
    return(colors)
    }else {


      if (!is.null(palette) & !is.null(scheme)) {
        if(palette=='s' | palette=='sequential') {
          lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

          colorSelector_sequential <- function(lengthOfSeries, scheme = scheme) {

            #____ yellow to green ####
            if (scheme == 'YlGn' | scheme == 'ylgn' | scheme == 'YellowGreen' | scheme == 'yellow2green' | scheme == 'yellowgreen'){
              lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

              colorList <- list(tol1qualitative=c("#addd8e"),
                                tol2qualitative=c("#f7fcb9", "#31a354"),
                                tol3qualitative=c("#f7fcb9", "#addd8e", "#31a354"),
                                tol4qualitative=c("#ffffcc", "#c2e699", "#78c679", "#238443"),
                                tol5qualitative=c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837"),
                                tol6qualitative=c("#ffffcc", "#d9f0a3", "#addd8e", "#78c679", "#31a354","#006837"),
                                tol7qualitative=c("#ffffcc", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443","#005a32"),
                                tol8qualitative=c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443","#005a32"),
                                tol9qualitative=c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529"))

              colorListSelected <- colorList[[lengthOfSeries]]
              return(colorListSelected)

            } else {

            #____ blue to purple ####
            if (scheme == 'BlPu' | scheme == 'blpu' | scheme == 'BluePurple' | scheme == 'blue2purple' | scheme == 'bluepurple'){
              lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

              colorList <- list(tol1qualitative=c("#9ebcda"),
                                tol2qualitative=c("#e0ecf4", "#8856a7"),
                                tol3qualitative=c("#e0ecf4", "#9ebcda", "#8856a7"),
                                tol4qualitative=c("#edf8fb", "#b3cde3", "#8c96c6", "#88419d"),
                                tol5qualitative=c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c"),
                                tol6qualitative=c("#edf8fb", "#bfd3e6", "#9ebcda", "#8c96c6", "#8856a7","#810f7c"),
                                tol7qualitative=c("#edf8fb", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d","#6e016b"),
                                tol8qualitative=c("#f7fcfd", "#e0ecf4", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d","#6e016b"),
                                tol9qualitative=c("#f7fcfd", "#e0ecf4", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b"))

              colorListSelected <- colorList[[lengthOfSeries]]
              return(colorListSelected)
            } else {

            #____ green to blue ####
            if (scheme == 'GnBl' | scheme == 'gnbl' | scheme == 'GreenBlue' | scheme == 'green2blue' | scheme == 'greenblue'){
              lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

              colorList <- list(tol1qualitative=c("#a8ddb5"),
                                tol2qualitative=c("#e0f3db", "#43a2ca"),
                                tol3qualitative=c("#e0f3db", "#a8ddb5", "#43a2ca"),
                                tol4qualitative=c("#f0f9e8", "#bae4bc", "#7bccc4", "#2b8cbe"),
                                tol5qualitative=c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac"),
                                tol6qualitative=c("#f0f9e8", "#ccebc5", "#a8ddb5", "#7bccc4", "#43a2ca", "#0868ac"),
                                tol7qualitative=c("#f0f9e8", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe","#08589e"),
                                tol8qualitative=c("#f7fcf0", "#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe","#08589e"),
                                tol9qualitative=c("#f7fcf0", "#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#0868ac", "#084081"))

              colorListSelected <- colorList[[lengthOfSeries]]
              return(colorListSelected)
            } else {

            #____ orange to red ####
            if (scheme == 'OrRd' | scheme == 'orrd' | scheme == 'OrangeRed' | scheme == 'orange2red' | scheme == 'orangered'){
              lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

              colorList <- list(tol1qualitative= c("#fdbb84"),
                                tol2qualitative= c("#fee8c8", "#e34a33"),
                                tol3qualitative= c("#fee8c8", "#fdbb84", "#e34a33"),
                                tol4qualitative= c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f"),
                                tol5qualitative= c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#b30000"),
                                tol6qualitative= c("#fef0d9", "#fdd49e", "#fdbb84", "#fc8d59", "#e34a33", "#b30000"),
                                tol7qualitative= c("#fef0d9", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#990000"),
                                tol8qualitative= c("#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#990000"),
                                tol9qualitative= c("#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000"))

              colorListSelected <- colorList[[lengthOfSeries]]
              return(colorListSelected)
            } else {

            #____ purple to blue to green ####
            if (scheme == 'PuBlGn' | scheme == 'publgn' | scheme == 'PurpleBlueGreen' | scheme == 'purple2blue2green' | scheme == 'purplebluegreen'){
              lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

              colorList <- list(tol1qualitative= c("#a6bddb"),
                                tol2qualitative= c("#ece2f0", "#1c9099"),
                                tol3qualitative= c("#ece2f0", "#a6bddb", "#1c9099"),
                                tol4qualitative= c("#f6eff7", "#bdc9e1", "#67a9cf", "#02818a"),
                                tol5qualitative= c("#f6eff7", "#bdc9e1", "#67a9cf", "#1c9099", "#016c59"),
                                tol6qualitative= c("#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf", "#1c9099", "#016c59"),
                                tol7qualitative= c("#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf", "#3690c0", "#02818a", "#016450"),
                                tol8qualitative= c("#fff7fb", "#ece2f0", "#d0d1e6", "#a6bddb", "#67a9cf", "#3690c0", "#02818a", "#016450"),
                                tol9qualitative= c("#fff7fb", "#ece2f0", "#d0d1e6", "#a6bddb", "#67a9cf", "#3690c0", "#02818a", "#016c59", "#014636"))

              colorListSelected <- colorList[[lengthOfSeries]]
              return(colorListSelected)
            } else {

            #____ yellow to orange to red ####
            if (scheme == 'YlOrRd' | scheme == 'ylorrd' | scheme == 'YellowOrangeRed' | scheme == 'yellow2orange2red' | scheme == 'yelloworangered'){
              lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

              colorList <- list(tol1qualitative= c("#feb24c"),
                                tol2qualitative= c("#ffeda0", "#f03b20"),
                                tol3qualitative= c("#ffeda0", "#feb24c", "#f03b20"),
                                tol4qualitative= c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c"),
                                tol5qualitative= c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"),
                                tol6qualitative= c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026"),
                                tol7qualitative= c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026"),
                                tol8qualitative= c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026"),
                                tol9qualitative= c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026"))

              colorListSelected <- colorList[[lengthOfSeries]]
              return(colorListSelected)
            } else {

            #____ blues ####
            if (scheme == 'Bl' | scheme == 'bl' | scheme == 'Blue' | scheme == 'blue' | scheme == 'blues'){
              lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

              colorList <- list(tol1qualitative= c("#9ecae1"),
                                tol2qualitative= c("#deebf7", "#3182bd"),
                                tol3qualitative= c("#deebf7", "#9ecae1", "#3182bd"),
                                tol4qualitative= c("#eff3ff", "#bdd7e7", "#6baed6", "#2171b5"),
                                tol5qualitative= c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c"),
                                tol6qualitative= c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c"),
                                tol7qualitative= c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#084594"),
                                tol8qualitative= c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#084594"),
                                tol9qualitative= c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b"))

              colorListSelected <- colorList[[lengthOfSeries]]
              return(colorListSelected)
            } else {

            #____ greens ####
            if (scheme == 'Gn' | scheme == 'gn' | scheme == 'Green' | scheme == 'green' | scheme == 'greens'){
              lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

              colorList <- list(tol1qualitative= c("#a1d99b"),
                                tol2qualitative= c("#e5f5e0", "#31a354"),
                                tol3qualitative= c("#e5f5e0", "#a1d99b", "#31a354"),
                                tol4qualitative= c("#edf8e9", "#bae4b3", "#74c476", "#238b45"),
                                tol5qualitative= c("#edf8e9", "#bae4b3", "#74c476", "#31a354", "#006d2c"),
                                tol6qualitative= c("#edf8e9", "#c7e9c0", "#a1d99b", "#74c476", "#31a354", "#006d2c"),
                                tol7qualitative= c("#edf8e9", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32"),
                                tol8qualitative= c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32"),
                                tol9qualitative= c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b"))

              colorListSelected <- colorList[[lengthOfSeries]]
              return(colorListSelected)
            } else {

            #____ grays ####
            if (scheme == 'Gy' | scheme == 'gy' | scheme == 'Gray' | scheme == 'gray' | scheme == 'grays'){
              lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

              colorList <- list(tol1qualitative= c("#bdbdbd"),
                                tol2qualitative= c("#f0f0f0", "#636363"),
                                tol3qualitative= c("#f0f0f0", "#bdbdbd", "#636363"),
                                tol4qualitative= c("#f7f7f7", "#cccccc", "#969696", "#525252"),
                                tol5qualitative= c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525"),
                                tol6qualitative= c("#f7f7f7", "#d9d9d9", "#bdbdbd", "#969696", "#636363", "#252525"),
                                tol7qualitative= c("#f7f7f7", "#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252", "#252525"),
                                tol8qualitative= c("#ffffff", "#f0f0f0", "#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252", "#252525"),
                                tol9qualitative= c("#ffffff", "#f0f0f0", "#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252", "#252525", "#000000"))

              colorListSelected <- colorList[[lengthOfSeries]]
              return(colorListSelected)
            } else {

            #____ purples ####
            if (scheme == 'Pu' | scheme == 'pu' | scheme == 'Purple' | scheme == 'purple' | scheme == 'purples'){
              lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

              colorList <- list(tol1qualitative= c("#bcbddc"),
                                tol2qualitative= c("#efedf5", "#756bb1"),
                                tol3qualitative= c("#efedf5", "#bcbddc", "#756bb1"),
                                tol4qualitative= c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3"),
                                tol5qualitative= c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#756bb1", "#54278f"),
                                tol6qualitative= c("#f2f0f7", "#dadaeb", "#bcbddc", "#9e9ac8", "#756bb1", "#54278f"),
                                tol7qualitative= c("#f2f0f7", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#4a1486"),
                                tol8qualitative= c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#4a1486"),
                                tol9qualitative= c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d"))

              colorListSelected <- colorList[[lengthOfSeries]]
              return(colorListSelected)
            } else {

            #____ reds ####
            if (scheme == 'Rd' | scheme == 'rd' | scheme == 'Red' | scheme == 'red' | scheme == 'reds'){
              lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

              colorList <- list(tol1qualitative= c("#fc9272"),
                                tol2qualitative= c("#fee0d2", "#de2d26"),
                                tol3qualitative= c("#fee0d2", "#fc9272", "#de2d26"),
                                tol4qualitative= c("#fee5d9", "#fcae91", "#fb6a4a", "#cb181d"),
                                tol5qualitative= c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15"),
                                tol6qualitative= c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15"),
                                tol7qualitative= c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#99000d"),
                                tol8qualitative= c("#fff5f0", "#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#99000d"),
                                tol9qualitative= c("#fff5f0", "#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#a50f15", "#67000d"))

              colorListSelected <- colorList[[lengthOfSeries]]
              return(colorListSelected)
            }}}}}}}}}}}

        colorListSelected <- colorList[[lengthOfSeries]]
        return(colorListSelected)
        }

        # Get colors
        colors <- colorSelector_sequential(lengthOfSeries, scheme = scheme)
        return(colors)
         } else {


            if(palette=='d' | palette=='diverging') {
              lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

              colorSelector_diverging <- function(lengthOfSeries, scheme = scheme){
                #____ brown to green ####
                if (scheme == 'BrGn' | scheme == 'brgn' | scheme == 'BrownGreen' | scheme == 'brown2green' | scheme == 'browngreen'){
                  lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

                  colorList <- list(tol1qualitative= c("#f5f5f5"),
                                    tol2qualitative= c("#d8b365", "#5ab4ac"),
                                    tol3qualitative= c("#d8b365", "#f5f5f5", "#5ab4ac"),
                                    tol4qualitative= c("#a6611a", "#dfc27d", "#80cdc1", "#018571"),
                                    tol5qualitative= c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571"),
                                    tol6qualitative= c("#8c510a", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e"),
                                    tol7qualitative= c("#8c510a", "#d8b365", "#f6e8c3", "#f5f5f5", "#c7eae5", "#5ab4ac", "#01665e"),
                                    tol8qualitative= c("#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e"),
                                    tol9qualitative= c("#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#f5f5f5", "#c7eae5", "#80cdc1", "#35978f", "#01665e"),
                                    tol10qualitative= c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30"),
                                    tol11qualitative= c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#f5f5f5", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30"))

                  colorListSelected <- colorList[[lengthOfSeries]]
                  return(colorListSelected)
                } else {

                #____ pink to green ####
                if (scheme == 'PkGn' | scheme == 'pkgn' | scheme == 'PinkGreen' | scheme == 'pink2green' | scheme == 'pinkgreen'){
                  lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

                  colorList <- list(tol1qualitative= c("#f7f7f7"),
                                    tol2qualitative= c("#e9a3c9", "#a1d76a"),
                                    tol3qualitative= c("#e9a3c9", "#f7f7f7", "#a1d76a"),
                                    tol4qualitative= c("#d01c8b", "#f1b6da", "#b8e186", "#4dac26"),
                                    tol5qualitative= c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26"),
                                    tol6qualitative= c("#c51b7d", "#e9a3c9", "#fde0ef", "#e6f5d0", "#a1d76a", "#4d9221"),
                                    tol7qualitative= c("#c51b7d", "#e9a3c9", "#fde0ef", "#f7f7f7", "#e6f5d0", "#a1d76a", "#4d9221"),
                                    tol8qualitative= c("#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#e6f5d0", "#b8e186", "#7fbc41", "#4d9221"),
                                    tol9qualitative= c("#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#f7f7f7", "#e6f5d0", "#b8e186", "#7fbc41", "#4d9221"),
                                    tol10qualitative= c("#8e0152", "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#e6f5d0", "#b8e186", "#7fbc41", "#4d9221", "#276419"),
                                    tol11qualitative= c("#8e0152", "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#f7f7f7", "#e6f5d0", "#b8e186", "#7fbc41", "#4d9221", "#276419"))

                  colorListSelected <- colorList[[lengthOfSeries]]
                  return(colorListSelected)
                } else {

                #____ purple to green ####
                if (scheme == 'PuGn' | scheme == 'pugn' | scheme == 'PurpleGreen' | scheme == 'purple2green' | scheme == 'purplegreen'){
                  lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

                  colorList <- list(tol1qualitative= c("#f7f7f7"),
                                    tol2qualitative= c("#af8dc3", "#7fbf7b"),
                                    tol3qualitative= c("#af8dc3", "#f7f7f7", "#7fbf7b"),
                                    tol4qualitative= c("#7b3294", "#c2a5cf", "#a6dba0", "#008837"),
                                    tol5qualitative= c("#7b3294", "#c2a5cf", "#f7f7f7", "#a6dba0", "#008837"),
                                    tol6qualitative= c("#762a83", "#af8dc3", "#e7d4e8", "#d9f0d3", "#7fbf7b", "#1b7837"),
                                    tol7qualitative= c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7", "#d9f0d3", "#7fbf7b", "#1b7837"),
                                    tol8qualitative= c("#762a83", "#9970ab", "#c2a5cf", "#e7d4e8", "#d9f0d3", "#a6dba0", "#5aae61", "#1b7837"),
                                    tol9qualitative= c("#762a83", "#9970ab", "#c2a5cf", "#e7d4e8", "#f7f7f7", "#d9f0d3", "#a6dba0", "#5aae61", "#1b7837"),
                                    tol10qualitative= c("#40004b", "#762a83", "#9970ab", "#c2a5cf", "#e7d4e8", "#d9f0d3", "#a6dba0", "#5aae61", "#1b7837", "#00441b"),
                                    tol11qualitative= c("#40004b", "#762a83", "#9970ab", "#c2a5cf", "#e7d4e8", "#f7f7f7", "#d9f0d3", "#a6dba0", "#5aae61", "#1b7837", "#00441b"))

                  colorListSelected <- colorList[[lengthOfSeries]]
                  return(colorListSelected)
                } else {

                #____ purple to orange ####
                if (scheme == 'PuOr' | scheme == 'puor' | scheme == 'PurpleOrange' | scheme == 'purple2orange' | scheme == 'purpleorange'){
                  lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

                  colorList <- list(tol1qualitative= c("#f7f7f7"),
                                    tol2qualitative= c("#f1a340", "#998ec3"),
                                    tol3qualitative= c("#f1a340", "#f7f7f7", "#998ec3"),
                                    tol4qualitative= c("#e66101", "#fdb863", "#b2abd2", "#5e3c99"),
                                    tol5qualitative= c("#e66101", "#fdb863", "#f7f7f7", "#b2abd2", "#5e3c99"),
                                    tol6qualitative= c("#b35806", "#f1a340", "#fee0b6", "#d8daeb", "#998ec3", "#542788"),
                                    tol7qualitative= c("#b35806", "#f1a340", "#fee0b6", "#f7f7f7", "#d8daeb", "#998ec3", "#542788"),
                                    tol8qualitative= c("#b35806", "#e08214", "#fdb863", "#fee0b6", "#d8daeb", "#b2abd2", "#8073ac", "#542788"),
                                    tol9qualitative= c("#b35806", "#e08214", "#fdb863", "#fee0b6", "#f7f7f7", "#d8daeb", "#b2abd2", "#8073ac", "#542788"),
                                    tol10qualitative= c("#7f3b08", "#b35806", "#e08214", "#fdb863", "#fee0b6", "#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b"),
                                    tol11qualitative= c("#7f3b08", "#b35806", "#e08214", "#fdb863", "#fee0b6", "#f7f7f7", "#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b"))

                  colorListSelected <- colorList[[lengthOfSeries]]
                  return(colorListSelected)
                } else {

                #____ red to blue ####
                if (scheme == 'RdBl' | scheme == 'rdbl' | scheme == 'RedBlue' | scheme == 'red2blue' | scheme == 'redblue'){
                  lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

                  colorList <- list(tol1qualitative= c("#f7f7f7"),
                                    tol2qualitative= c("#ef8a62", "#67a9cf"),
                                    tol3qualitative= c("#ef8a62", "#f7f7f7", "#67a9cf"),
                                    tol4qualitative= c("#ca0020", "#f4a582", "#92c5de", "#0571b0"),
                                    tol5qualitative= c("#ca0020", "#f4a582", "#f7f7f7", "#92c5de", "#0571b0"),
                                    tol6qualitative= c("#b2182b", "#ef8a62", "#fddbc7", "#d1e5f0", "#67a9cf", "#2166ac"),
                                    tol7qualitative= c("#b2182b", "#ef8a62", "#fddbc7", "#f7f7f7", "#d1e5f0", "#67a9cf", "#2166ac"),
                                    tol8qualitative= c("#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac"),
                                    tol9qualitative= c("#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac"),
                                    tol10qualitative= c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"),
                                    tol11qualitative= c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"))

                  colorListSelected <- colorList[[lengthOfSeries]]
                  return(colorListSelected)
                } else {

                #____ red to yellow to blue ####
                if (scheme == 'RdYlBl' | scheme == 'rdylbl' | scheme == 'RedYellowBlue' | scheme == 'red2yellow2blue' | scheme == 'redyellowblue'){
                  lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

                  colorList <- list(tol1qualitative= c("#ffffbf"),
                                    tol2qualitative= c("#fc8d59", "#91bfdb"),
                                    tol3qualitative= c("#fc8d59", "#ffffbf", "#91bfdb"),
                                    tol4qualitative= c("#d7191c", "#fdae61", "#abd9e9", "#2c7bb6"),
                                    tol5qualitative= c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"),
                                    tol6qualitative= c("#d73027", "#fc8d59", "#fee090", "#e0f3f8", "#91bfdb", "#4575b4"),
                                    tol7qualitative= c("#d73027", "#fc8d59", "#fee090", "#ffffbf", "#e0f3f8", "#91bfdb", "#4575b4"),
                                    tol8qualitative= c("#d73027", "#f46d43", "#fdae61", "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4"),
                                    tol9qualitative= c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4"),
                                    tol10qualitative= c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"),
                                    tol11qualitative= c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"))

                  colorListSelected <- colorList[[lengthOfSeries]]
                  return(colorListSelected)
                }}}}}}

                colorListSelected <- colorList[[lengthOfSeries]]
                return(colorListSelected)
            }

                # Get colors
                colors <- colorSelector_diverging(lengthOfSeries, scheme = scheme)
                return(colors)
              } else {


                if(palette=='q' | palette=='qualitative') {
                  lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

                  colorSelector_qualitative <- function(lengthOfSeries, scheme = scheme){

                    #____ blue to green ####
                    if (scheme == 'paired' | scheme == 'pair' | scheme == 'Paired' | scheme == 'Pair') {
                      lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

                      colorList <- list(tol1qualitative=c("#1f78b4"),
                                        tol2qualitative=c("#1f78b4", "#33a02c"),
                                        tol3qualitative=c("#a6cee3", "#1f78b4", "#b2df8a"),
                                        tol4qualitative=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"))

                      colorListSelected <- colorList[[lengthOfSeries]]
                      return(colorListSelected)
                    } else {

                    #____ dark ####
                    if (scheme == 'dark' | scheme == 'Dark') {
                      lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

                      colorList <- list(tol1qualitative=c("#1b9e77"),
                                        tol2qualitative=c("#1b9e77", "#d95f02"),
                                        tol3qualitative=c("#1b9e77", "#d95f02", "#7570b3"))

                      colorListSelected <- colorList[[lengthOfSeries]]
                      return(colorListSelected)
                    } else {

                    #____ light ####
                    if (scheme == 'light' | scheme == 'Light') {
                      lengthOfSeries <- sum(c(length(series),length(seriesLeft)))

                      colorList <- list(tol1qualitative=c("#66c2a5"),
                                        tol2qualitative=c("#66c2a5", "#fc8d62"),
                                        tol3qualitative=c("#66c2a5", "#fc8d62", "#8da0cb"))

                      colorListSelected <- colorList[[lengthOfSeries]]
                      return(colorListSelected)
                    }}}

                  colorListSelected <- colorList[[lengthOfSeries]]
                  return(colorListSelected)
                }
                  # Get colors
                  colors <- colorSelector_qualitative(lengthOfSeries, scheme = scheme)
                  return(colors) }

              }}}}}}






