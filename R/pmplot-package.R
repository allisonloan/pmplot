#' pmplot: Brackets and Panel Markups for ggplot2
#'
#' Utilities to draw top/bottom/left/right brackets with labels in panel-normalized
#' coordinates (NPC; 0–1), including helpers for stacking, mixing orientations,
#' and facet-specific annotations. Auto-bumps plot margins and disables clipping
#' when you place brackets outside the panel (e.g., x or y > 1).
#'
#' @section Key functions:
#' * [bracket_grob()] – low-level grob builder (NPC coords)
#' * [add_horizontal_top()], [add_horizontal_bot()], [add_vertical_right()], [add_vertical_left()] – add one bracket with auto margins
#' * [add_many_horizontal()], [add_many_vertical()] – vectorized add
#' * [add_horizontal_stack()], [add_vertical_stack()] – stacked brackets outside/inside
#' * [add_brackets()] – mix horizontals + verticals in one call
#' * [add_faceted_brackets()] – facet-specific brackets from a specs data frame
#'
#' @section Coordinate system:
#' All positions are in panel NPC (0–1). Values outside [0,1] draw off-panel; helpers
#' expand plot margins automatically to keep annotations visible.
#'
#' @docType package
#' @name pmplot
#' @aliases pmplot-package
#' @import ggplot2
#' @import grid
NULL
