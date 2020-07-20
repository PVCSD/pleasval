#' Spartan Colors and Styles
#'
#' @description An internal named list of colors for a consistant theme for PVCSD
#'
spartanStyles <- c(
  'Dark Blue'    = "#011638",
  'Spartan Blue' = "#003569",
  'Light Blue'   = "#26547C",
  'Blue Grey'    = "#4B6C8C",
  'Spartan Grey' = "#D6D6D6",
  'Blue Gray'    = "#4B6C8C",
  'Spartan Gray' = "#D6D6D6",
  'silver'       = "#C1C1C1",
  'Dark Grey'    = "#565656",
  'Dark Gray'    = "#565656",
  'Mustard'      = "#FFC857",
  'salmon'       = "#D16666",
  'steel'        = "#C1C1C1"
)

#' Extract Styles
#'
#' @description An internal function to extract the named colors
#'
#' @param ... A list of color names
#'
#' @return A list of hex codes
pv_styles <- function(...){
  cols <-  c(...)

  if (is.null(cols))
    return(spartanStyles)

  spartanStyles[cols]
}

#' A list of PVCSD color palettes
#'
#' @description An Internal list of palettes for consistant styling
#'
#' @return A list of named colors
spartanPalettes <- list(
  `main`  = pv_styles('Dark Blue', 'Spartan Blue', 'Light Blue',  'Dark Grey'),
  `blues` = pv_styles('Dark Blue', 'Spartan Blue', 'Light Blue', 'Blue Grey'),
  `mono`  = pv_styles('Spartan Blue'),
  `cat`   = pv_styles('salmon', 'Mustard', 'Dark Grey','Steel', 'Dark Blue' )
)

#' Fetch the spartan palettes
#'
#' @description An internal function that creates a color ramp palette if needed
#'
#' @return A palette
spartan_palette <- function(palette = "main", reverse = FALSE, ...) {
  pal <- spartanPalettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

#' Spartan Colors Palette for ggplot2
#'
#' @description Set the colors and fill for ggplot objects to colors that fit the spartan palettes
#'
#' @param palette Name of a PVCSD color palette (see Details)
#' @param discrete Boolean: Should the palette be discrete or continuous?
#' @param reverse Boolean: Should the palette be reversed?
#' @param ... Additional optional arguments
#'
#' @md
#' @details
#' ## Palettes
#' - `main`  The default palette scales from blue to grey
#' - `blues` A monochromatic blue palette
#' - `mono`  A single color palette
#' - `cat`   A color palette for categorical variables
#' @rdname scale_color_pleasval
#' @export
scale_color_spartan <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- spartan_palette(palette = palette, reverse = reverse)


  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("spartan", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @rdname  scale_color_pleasval
#' @export
scale_fill_spartan <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {


  pal <- spartan_palette(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("spartan", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

