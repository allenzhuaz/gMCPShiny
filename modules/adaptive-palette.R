#' Adaptive palette (discrete)
#'
#' Create a discrete palette that will use the first `n` colors from
#' the supplied color values when the palette has enough colors.
#' Otherwise, use an interpolated color palette.
#'
#' @param values Color values.
pal_ramp <- function(values) {
  force(values)
  function(n) {
    if (n <= length(values)) {
      values[seq_len(n)]
    } else {
      colorRampPalette(values, alpha = TRUE)(n)
    }
  }
}

#' Adaptive color palette generator
#'
#' Adaptive color palette generator for ggsci color palettes using `pal_ramp()`.
#'
#' @param name Color palette name in ggsci
#' @param palette Color palette type in ggsci
#' @param alpha Transparency level, a real number in (0, 1].
#'
#' @details See `names(ggsci:::ggsci_db)` for all color palette names in ggsci.
#' See `names(ggsci:::ggsci_db$"pal")` for available palette types under
#' the palette `pal`.
pal_adaptive <- function(name, palette, alpha = 1) {
  if (alpha > 1L | alpha <= 0L) stop("alpha must be in (0, 1]")

  if(name == "Merck"){
    raw_cols <- c(
      "#00857C",
      "#6ECEB2",
      "#BFED33",
      "#FFF063",
      "#0C2340",
      "#5450E4",
      "#688CE8",
      "#69B8F7"
    )
  } else if(name %in% c("Okabe-Ito")){
    raw_cols <- palette.colors(palette = name)[-1] # remove the first element of black
  } else{
  raw_cols <- ggsci:::ggsci_db[[name]][[palette]]
  }
  raw_cols_rgb <- col2rgb(raw_cols)
  alpha_cols <- rgb(
    raw_cols_rgb[1L, ], raw_cols_rgb[2L, ], raw_cols_rgb[3L, ],
    alpha = alpha * 255L, names = names(raw_cols),
    maxColorValue = 255L
  )

  pal_ramp(unname(alpha_cols))
}

#' Adaptive color palette generator for hGraph
#'
#' Adaptive color palette generator for hGraph using `pal_adaptive()` and `pal_ramp()`.
#'
#' @param pal_name Color palette name in ggsci
#' @param n How many different colors for the selected color palette
#' @param alpha Transparency level, a real number in (0, 1].
hgraph_palette <- function(pal_name, n, alpha = 1){

  if(pal_name == "Grey"){
    grey.colors(n, alpha = alpha)
  } else if(grepl("--", pal_name)) { # need input value format fitting ggsci palette "name--palette"
    pal_adaptive(name = unlist(strsplit(pal_name, split = "--"))[1], palette = unlist(strsplit(pal_name, split = "--"))[2], alpha = alpha)(n)
  } else{
    pal_adaptive(name = pal_name, alpha = alpha)(n)
  }

}
