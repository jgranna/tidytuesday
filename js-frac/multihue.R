#' Multi-hue color scales
#'
#' `multihue()`, `multihue.diverge()` address the problem of creating multi-hue (multi "stop")
#' color scales with perceptually linear lightness gradients and smooth hue and chroma gradients
#' (with no first or second order discontinuities).
#'
#' Pallettes produced by `multihue()`, `multihue.diverge()` and `multihue.constantL()` use bezier
#' interpolation in HCL space to produce smooth, transient-free color scales. After interpolation,
#' the scales are stretched to correct any non-linearity in brightness gradient introduced by the
#' presence of intermediate color "stops." The result is a perceptually linear brightness gradient,
#' even when printed in black and white.
#'
#' The palette is specified by a vector of colors (or two vectors in the case of
#' `multihue.diverge`).  The first and last colors of each vector are endpoints of a color scale,
#' and optional intermediate color "stops" are treated as control points for a bezier curve
#' connecting the endpoints in HCL colorspace. These intermediate color stops are therefore not
#' actually present in the final color palette, but influence it. Colors may defined as hex codes or
#' with hue, chroma and lightness values, as in [grDevices::hcl()].
#'
#' These functions are built on top of Gregor Aisch's [chroma.js](https://gka.github.io/chroma.js/)
#' javascript library. The functions access chroma.js javascript functions using the
#' [V8](https://CRAN.R-project.org/package=V8) package.
#'
#' Palette characteristics:
#'
#' * perceptually linear brightness gradients
#' * smooth hue gradient throughout (no "corners")
#' * multiple color stops allow increased color discrimination between levels
#' * hue and chroma gradients not necessarily linear. Only brightness is adjusted for linearity.
#'
#' @param n Integer. Number of levels in palette (number of bins).  Default: `n = 11`.
#' @param colors Character vector. (Only used with `multiHue`.) A vector of RGB colors specified as
#'   hex color codes.
#' @param lcolors Character vector. (Only used with `multiHue.diverge`.) A vector of RGB colors
#'   specified as hex color codes, defining the left side of a diverging palette.
#' @param rcolors Character vector. (Only used with `multiHue.diverge`.) A vector of RGB colors
#'   specified as hex color codes, defining the right side of a diverging palette.
#' @param continuous Logical. (Default: TRUE) If continuous=TRUE (the default), A divergent palette
#'   with no central discontinuity is created by joining `lcolors` and `rcolors`. The last color of
#'   `lcolors` and the first color of `rcolors` must be identical. If continuous=FALSE `lcolors` and
#'   `rcolors` will be abutted, creating a discontinuity last color of `lcolors` and the first color
#'   of `rcolors` are not identical. A discontinuous palette will always contain an even number of
#'   colors (`n` will be rounded down to the nearest even number).
#' @param h Numeric vector. (Only used with `multiHue.constantL`.) Hue angles for color stops.
#' @param c Numeric vector. (Only used with `multiHue.constantL`.) Chroma value(s) for color stops.
#' @param l Numeric. (Only used with `multiHue.constantL`.) Brightness value for color stops.
#'
#' @return character vector. A vector of RGB colors specified as hex color codes (#RRGGBB).
#' @importFrom grDevices hcl
#' @examples
#' p <- multiHue()
#' colorbar(p)
#'
#' ## Maroon to yellow through salmon and pink
#' stops <- c('#800000', '#CD4949', '#FF9696','#FFFF00')
#' colorbar(stops) # display stops
#' palette <- multiHue(1024,colors=stops)
#' colorbar(palette) # display palette
#'
#' ## Divergent (default)
#' p <- multiHue.diverge(1024)
#' colorbar(p)
#'
#' ## Divergent with discontinuity: left = Blue to Cyan, right = Yellow to Red
#' leftstops <- c("#3136AA","#3980B0","#00C5C0","#8EFDFD")
#' colorbar(leftstops)
#' rightstops <- c("#F3F300","#FF8CB4","#CD4C4C","#8F0000")
#' colorbar(rightstops)
#' palette <- multiHue.diverge(1024,leftstops,rightstops,continuous=FALSE)
#' colorbar(palette)
#'
#' ## Constant Lightness (and chroma) color palette:
#' ## 4 color stops define the bezier curve through HCL space; n=6 colors are output.
#' multiHue.constantL(n=6, h=c(-180, -105, -45, 0),c=50,l=60)

#' @export

multiHue <- function(n=11,colors=c("#081d58", "#086699", "#08BEC1", "#ffff66")) {
  context <- V8::v8()
  context$source(system.file("js", "chroma.min.js", package = "datacolor"))
  context$assign("n", n)
  context$assign("colors", array(colors))
  palette <- context$eval("chroma.bezier(colors).scale().mode('lab').correctLightness().colors(n);")
  return(unlist(strsplit(palette, split=",")))
}

#' @rdname multiHue
#' @export
multiHue.diverge <- function(n=11,
                             lcolors = c('#0000d0', '#0479C8', '#08C8C8','#ffff00'),
                             rcolors = c('#ffff00', '#FF9696', '#CD4949','#800000'),
                             continuous=TRUE
){
  if (continuous==TRUE & lcolors[length(lcolors)]!=rcolors[1]) {
    stop("If continuous=TRUE (the default), the last left hand color and the first right hand color
         must be the same. Fix colors or set continuous=FALSE.")
  }
  if (continuous==FALSE & (n %% 2) != 0) {
    n <- n-1
    warning(paste0("If continuous=FALSE, n must be an even number.  Rounding n down to ", n, "."))
  }
  c <- as.logical(continuous) # continuous
  n <- as.integer(n)
  m <- as.integer(0.5*n)+c
  e <- !(n%%2) # even flag
  
  context <- V8::v8()
  context$source(system.file("js", "chroma.min.js", package = "datacolor"))
  context$assign("m", m)
  context$assign("lcolors", array(lcolors))
  context$assign("rcolors", array(rcolors))
  l <- context$eval("chroma.bezier(lcolors).scale().mode('lab').correctLightness().colors(m);")
  r <- context$eval("chroma.bezier(rcolors).scale().mode('lab').correctLightness().colors(m);")
  lpalette <- unlist(strsplit(l, split=","))
  rpalette <- unlist(strsplit(r, split=","))
  palette <- c(lpalette[1:(m-e+!c)],rpalette[(1+c):m])
  return(palette)
}

#' @rdname multiHue
#' @export
multiHue.constantL <- function(n=11,h=c(0,-45,-90,-135),c=50,l=60) {
  if (length(l)>1) {
    warning(paste0("Length of l is greater than 1.  Using only first element of l (", l[1], ")."))
  }
  colors <- grDevices::hcl(h=h,c=c,l=l[1])
  context <- V8::v8()
  context$source(system.file("js", "chroma.min.js", package = "datacolor"))
  context$assign("n", n)
  context$assign("colors", array(colors))
  palette <- context$eval("chroma.bezier(colors).scale().mode('lab').colors(n);")
  return(unlist(strsplit(palette, split=",")))
}