#' Build a bracket grob in panel NPC coordinates
#'
#' Creates a bracket (with caps and a label) in **panel-normalized coordinates** (NPC; 0–1).
#' Use the sugar helpers for common orientations, or call this directly for maximum control.
#'
#' @param side One of `"top"`, `"bottom"`, `"left"`, `"right"`.
#' @param start,end For horizontal: x range (0–1). For vertical: y range (0–1).
#' @param pos For horizontal: y (0–1). For vertical: x (0–1). Values outside [0,1] place the bracket outside the panel.
#' @param label Text label.
#' @param cap Length of the cap “tick” in NPC units. Default `0.015`.
#' @param label_offset Gap between bracket and label in NPC units. Default `0.05`.
#' @param lwd Line width. Default `1.5`.
#' @param label_size Label size (points). Default `10`.
#' @param fontface Font face (`"plain"`, `"bold"`, etc.). Default `"plain"`.
#' @param family Font family. Default `"Arial"`.
#' @param cex Deprecated; use `label_size`.
#'
#' @return A \code{grid} grob for use with \code{ggplot2::annotation_custom()}.
#' @examples
#' # See helpers like add_horizontal_top() for usage.
#' @export
bracket_grob <- function(
    side = c("top","bottom","left","right"),
    start, end, pos,
    label,
    cap = 0.015,
    label_offset = 0.05,
    lwd = 1.5,
    cex = 0.9,              # kept for legacy calls, but ignored if label_size given
    fontface = "plain",     # <- not bold by default
    family = "Arial",       # <- new default family
    label_size = 10         # <- new default (pt)
){
  side <- match.arg(side)
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required.")
  }
  # If someone passes a different cex but leaves label_size at default, you can
  # uncomment the next 2 lines to honor cex:
  # if (!missing(cex) && missing(label_size)) {
  #   label_size <- 10 * cex
  # }

  withCallingHandlers({
    if (side %in% c("top","bottom")) {
      y_line <- grid::unit(c(pos, pos), "npc")
      x_line <- grid::unit(c(start, end), "npc")
      cap_dir <- if (side == "top") -cap else cap
      x_capL <- grid::unit(start, "npc")
      x_capR <- grid::unit(end,   "npc")
      y_cap  <- grid::unit(c(pos, pos + cap_dir), "npc")
      y_lab  <- grid::unit(pos + if (side == "top") label_offset else -label_offset, "npc")
      x_lab  <- grid::unit((start + end)/2, "npc")
      rot    <- 0

      grid::grobTree(
        grid::linesGrob(x = x_line, y = y_line, gp = grid::gpar(lwd = lwd)),
        grid::linesGrob(x = x_capL, y = y_cap,  gp = grid::gpar(lwd = lwd)),
        grid::linesGrob(x = x_capR, y = y_cap,  gp = grid::gpar(lwd = lwd)),
        grid::textGrob(
          label, x = x_lab, y = y_lab, rot = rot,
          gp = grid::gpar(fontsize = label_size, fontface = fontface, fontfamily = family)
        )
      )
    } else {
      x_line <- grid::unit(c(pos, pos), "npc")
      y_line <- grid::unit(c(start, end), "npc")
      cap_dir <- if (side == "right") -cap else cap
      y_capT <- grid::unit(start, "npc")
      y_capB <- grid::unit(end,   "npc")
      x_cap  <- grid::unit(c(pos, pos + cap_dir), "npc")
      x_lab  <- grid::unit(pos + if (side == "right") label_offset else -label_offset, "npc")
      y_lab  <- grid::unit((start + end)/2, "npc")
      rot    <- if (side == "right") 270 else 90

      grid::grobTree(
        grid::linesGrob(x = x_line, y = y_line, gp = grid::gpar(lwd = lwd)),
        grid::linesGrob(x = x_cap,  y = y_capT, gp = grid::gpar(lwd = lwd)),
        grid::linesGrob(x = x_cap,  y = y_capB, gp = grid::gpar(lwd = lwd)),
        grid::textGrob(
          label, x = x_lab, y = y_lab, rot = rot,
          gp = grid::gpar(fontsize = label_size, fontface = fontface, fontfamily = family)
        )
      )
    }
  }, error = function(e) stop("Failed to build bracket_grob: ", e$message))
}


# ------------------------
# Internal: margin + clip
# ------------------------

#' Ensure off-panel drawing and sufficient plot margins
#'
#' Internal helper that turns \code{clip = "off"} and guarantees a minimum plot
#' margin on each side (in points). It preserves the larger of current vs requested.
#'
#' @param p A ggplot object.
#' @param top,right,bottom,left Minimum margins in points.
#' @return A ggplot with updated coord/theme.
#' @keywords internal
.bump_margin <- function(p, top = 0, right = 0, bottom = 0, left = 0) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  current <- ggplot2::theme_get()$plot.margin
  if (is.null(current)) current <- grid::unit(c(5.5, 5.5, 5.5, 5.5), "pt")
  new_margins <- grid::unit.c(
    grid::unit(max(as.numeric(current[[1]]), top),    "pt"),
    grid::unit(max(as.numeric(current[[2]]), right),  "pt"),
    grid::unit(max(as.numeric(current[[3]]), bottom), "pt"),
    grid::unit(max(as.numeric(current[[4]]), left),   "pt")
  )
  p + ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(plot.margin = new_margins)
}

#' Compute margin bumps for stacked/outside brackets
#'
#' Inspects a specs data frame and increases margins by side based on how many
#' distinct outside layers are present (e.g., y > 1 for top, x > 1 for right).
#'
#' @param p A ggplot object.
#' @param specs_df Data frame with at least columns \code{side}, \code{pos}.
#' @param base_pt Base margin (points) if any outside layer exists on a side.
#' @param step_pt Extra margin (points) per additional outside layer on that side.
#' @param side_col,pos_col Column names in \code{specs_df} for side and position.
#' @return A ggplot with updated margins.
#' @keywords internal
.bump_for_specs <- function(p, specs_df, base_pt = 40, step_pt = 14, side_col = "side", pos_col = "pos") {
  need_top    <- specs_df[[side_col]] == "top"    & specs_df[[pos_col]] > 1
  need_bottom <- specs_df[[side_col]] == "bottom" & specs_df[[pos_col]] < 0
  need_right  <- specs_df[[side_col]] == "right"  & specs_df[[pos_col]] > 1
  need_left   <- specs_df[[side_col]] == "left"   & specs_df[[pos_col]] < 0

  bump <- function(mask) {
    if (!any(mask)) return(0)
    n_layers <- length(unique(round(specs_df[[pos_col]][mask], 4)))
    base_pt + step_pt * (n_layers - 1)
  }

  .bump_margin(
    p,
    top    = bump(need_top),
    right  = bump(need_right),
    bottom = bump(need_bottom),
    left   = bump(need_left)
  )
}

# ------------------------
# Grob sugar (return grobs only)
# ------------------------

#' Vertical bracket grob on the right side
#'
#' Returns a grob for a right-side vertical bracket. Use with \code{\link{add_bracket}}
#' or higher-level helpers (\code{\link{add_vertical_right}}) to add to a ggplot.
#'
#' @inheritParams bracket_grob
#' @param ystart,yend Vertical start/end (0–1); @param x Horizontal NPC (0–1).
#' @return A grid grob.
#' @export
add_vertical_right_grob <- function(label, ystart, yend, x, ...) {
  bracket_grob("right", start = ystart, end = yend, pos = x, label = label, ...)
}

#' Vertical bracket grob on the left side
#'
#' @inheritParams add_vertical_right_grob
#' @return A grid grob.
#' @export
add_vertical_left_grob <- function(label, ystart, yend, x, ...) {
  bracket_grob("left", start = ystart, end = yend, pos = x, label = label, ...)
}

#' Top horizontal bracket grob
#'
#' @inheritParams bracket_grob
#' @param xstart,xend Horizontal start/end (0–1); @param y Vertical NPC (0–1).
#' @return A grid grob.
#' @export
add_horizontal_top_grob <- function(label, xstart, xend, y, ...) {
  bracket_grob("top", start = xstart, end = xend, pos = y, label = label, ...)
}

#' Bottom horizontal bracket grob
#'
#' @inheritParams add_horizontal_top_grob
#' @return A grid grob.
#' @export
add_horizontal_bot_grob <- function(label, xstart, xend, y, ...) {
  bracket_grob("bottom", start = xstart, end = xend, pos = y, label = label, ...)
}

# ------------------------
# Plot-aware single-add helpers (bake margins here)
# ------------------------

#' Add a top horizontal bracket to a ggplot (with auto margin)
#'
#' Adds a top horizontal bracket and ensures sufficient top margin and off-panel drawing.
#'
#' @param p A ggplot object.
#' @inheritParams add_horizontal_top_grob
#' @param margin_pt Minimum top margin in points. Default `40`.
#' @return A ggplot object with the bracket added.
#' @examples
#' \donttest{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
#' add_horizontal_top(p, "Top", 0.2, 0.8, 1.05)
#' }
#' @export
add_horizontal_top <- function(p, label, xstart, xend, y, margin_pt = 40, ...) {
  p <- .bump_margin(p, top = margin_pt)
  add_bracket(p, add_horizontal_top_grob(label, xstart, xend, y, ...))
}

#' Add a bottom horizontal bracket to a ggplot (with auto margin)
#'
#' @inheritParams add_horizontal_top
#' @param margin_pt Minimum bottom margin in points. Default `40`.
#' @export
add_horizontal_bot <- function(p, label, xstart, xend, y, margin_pt = 40, ...) {
  p <- .bump_margin(p, bottom = margin_pt)
  add_bracket(p, add_horizontal_bot_grob(label, xstart, xend, y, ...))
}

#' Add a right vertical bracket to a ggplot (with auto margin)
#'
#' @inheritParams add_horizontal_top
#' @param ystart,yend,x See \code{\link{add_vertical_right_grob}}.
#' @param margin_pt Minimum right margin in points. Default `40`.
#' @export
add_vertical_right <- function(p, label, ystart, yend, x, margin_pt = 40, ...) {
  p <- .bump_margin(p, right = margin_pt)
  add_bracket(p, add_vertical_right_grob(label, ystart, yend, x, ...))
}

#' Add a left vertical bracket to a ggplot (with auto margin)
#'
#' @inheritParams add_vertical_right
#' @param margin_pt Minimum left margin in points. Default `40`.
#' @export
add_vertical_left <- function(p, label, ystart, yend, x, margin_pt = 40, ...) {
  p <- .bump_margin(p, left = margin_pt)
  add_bracket(p, add_vertical_left_grob(label, ystart, yend, x, ...))
}

# ------------------------
# Generic add + many + stack + composite + facet helpers
# ------------------------

#' Add a single bracket grob to a ggplot
#'
#' Convenience wrapper around \code{ggplot2::annotation_custom()}.
#'
#' @param p A ggplot object.
#' @param grob A grob from \code{*_grob()} functions.
#' @return A ggplot with the annotation added.
#' @export
add_bracket <- function(p, grob) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  p + ggplot2::annotation_custom(grob = grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
}

#' Add many horizontal brackets (auto margin by side)
#'
#' Vectorized helper. Pass equal-length \code{labels}, \code{xstart}, \code{xend};
#' a single \code{y} shared by all; choose \code{side = "top"} or \code{"bottom"}.
#'
#' @param p A ggplot.
#' @param labels Character vector.
#' @param xstart,xend Numeric vectors (0–1).
#' @param y Numeric (0–1). Can be outside (e.g., >1 for top, <0 for bottom).
#' @param side "top" or "bottom".
#' @param margin_pt Minimum margin (points) on the relevant side. Default `40`.
#' @param ... Passed to \code{\link{bracket_grob}} (e.g., \code{lwd}, \code{cex}, \code{label_offset}).
#'
#' @return A ggplot with multiple brackets.
#' @examples
#' \donttest{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
#' add_many_horizontal(p, c("A","B"), c(0.1,0.55), c(0.45,0.9), 1.05, side = "top")
#' }
#' @export
add_many_horizontal <- function(p, labels, xstart, xend, y,
                                side = c("top","bottom"), margin_pt = 40, ...) {
  side <- match.arg(side)
  stopifnot(length(labels) == length(xstart), length(labels) == length(xend))
  p <- switch(side,
              "top"    = .bump_margin(p, top = margin_pt),
              "bottom" = .bump_margin(p, bottom = margin_pt))
  for (i in seq_along(labels)) {
    g <- if (side == "top") {
      add_horizontal_top_grob(labels[i], xstart[i], xend[i], y, ...)
    } else {
      add_horizontal_bot_grob(labels[i], xstart[i], xend[i], y, ...)
    }
    p <- add_bracket(p, g)
  }
  p
}

#' Add many vertical brackets (auto margin by side)
#'
#' Vectorized helper. Pass equal-length \code{labels}, \code{ystart}, \code{yend};
#' a single \code{x} shared by all; choose \code{side = "right"} or \code{"left"}.
#'
#' @param p A ggplot.
#' @param labels Character vector.
#' @param ystart,yend Numeric vectors (0–1).
#' @param x Numeric (0–1). Can be outside (e.g., >1 for right, <0 for left).
#' @param side "right" or "left".
#' @param margin_pt Minimum margin (points) on the relevant side. Default `40`.
#' @param ... Passed to \code{\link{bracket_grob}}.
#'
#' @return A ggplot with multiple brackets.
#' @examples
#' \donttest{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
#' add_many_vertical(p, c("L","H"), c(0.1,0.55), c(0.45,0.9), 1.1, side = "right")
#' }
#' @export
add_many_vertical <- function(p, labels, ystart, yend, x,
                              side = c("right","left"), margin_pt = 40, ...) {
  side <- match.arg(side)
  stopifnot(length(labels) == length(ystart), length(labels) == length(yend))
  p <- switch(side,
              "right" = .bump_margin(p, right = margin_pt),
              "left"  = .bump_margin(p, left  = margin_pt))
  for (i in seq_along(labels)) {
    g <- if (side == "right") {
      add_vertical_right_grob(labels[i], ystart[i], yend[i], x, ...)
    } else {
      add_vertical_left_grob(labels[i], ystart[i], yend[i], x, ...)
    }
    p <- add_bracket(p, g)
  }
  p
}

#' Add multiple horizontal and/or vertical brackets in one call
#'
#' Takes a specs data frame (rows = brackets) and adds them all. Automatically
#' adjusts margins for outside/stacked layers on each side.
#'
#' @param p A ggplot.
#' @param specs_df Data frame with columns:
#'   \itemize{
#'     \item \code{side} — one of "top","bottom","left","right"
#'     \item \code{start} — start coordinate (x for horizontal, y for vertical), 0–1
#'     \item \code{end} — end coordinate (x for horizontal, y for vertical), 0–1
#'     \item \code{pos} — orthogonal position (y for horizontal, x for vertical)
#'     \item \code{label} — text label
#'   }
#' @param base_pt Base margin (points) if an outside layer exists on a side. Default `40`.
#' @param step_pt Extra margin (points) per additional outside layer on that side. Default `14`.
#' @param ... Passed to \code{\link{bracket_grob}} (\code{lwd}, \code{cex}, \code{label_offset}, etc.).
#'
#' @return A ggplot with all brackets added.
#' @examples
#' \donttest{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) + ggplot2::geom_boxplot()
#' specs <- data.frame(
#'   side  = c("right","right","top","top"),
#'   start = c(0.2, 0.6, 0.2, 0.5),
#'   end   = c(0.5, 0.9, 0.4, 0.8),
#'   pos   = c(1.05, 1.15, 1.05, 1.15),
#'   label = c("VR1","VR2","HT1","HT2")
#' )
#' add_brackets(p, specs, label_offset = 0.07)
#' }
#' @export
add_brackets <- function(p, specs_df, base_pt = 40, step_pt = 14, ...) {
  p <- .bump_for_specs(p, specs_df, base_pt = base_pt, step_pt = step_pt)
  for (i in seq_len(nrow(specs_df))) {
    side  <- specs_df[["side"]][i]
    start <- specs_df[["start"]][i]
    end   <- specs_df[["end"]][i]
    pos   <- specs_df[["pos"]][i]
    lab   <- specs_df[["label"]][i]
    grob_i <- switch(
      side,
      "top"    = bracket_grob("top",    start, end, pos, lab, ...),
      "bottom" = bracket_grob("bottom", start, end, pos, lab, ...),
      "left"   = bracket_grob("left",   start, end, pos, lab, ...),
      "right"  = bracket_grob("right",  start, end, pos, lab, ...)
    )
    p <- add_bracket(p, grob_i)
  }
  p
}

#' Stack multiple horizontal brackets
#'
#' Adds several horizontal brackets at different \code{y} positions (can be outside).
#' Automatically bumps top/bottom margins for stacked outside layers.
#'
#' @param p A ggplot.
#' @param labels Character vector, length N.
#' @param xstart,xend Numeric vectors (0–1), length N.
#' @param y Numeric vector (0–1; can be outside), length N.
#' @param side "top" or "bottom".
#' @param base_pt,step_pt Margin handling (see \code{\link{add_brackets}}).
#' @param ... Passed to \code{\link{bracket_grob}}.
#'
#' @return A ggplot with all brackets added.
#' @examples
#' \donttest{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
#' add_horizontal_stack(
#'   p,
#'   labels = c("A","B"),
#'   xstart = c(0.1, 0.55),
#'   xend   = c(0.45, 0.9),
#'   y      = c(1.05, 1.13),
#'   side   = "top"
#' )
#' }
#' @export
add_horizontal_stack <- function(p, labels, xstart, xend, y,
                                 side = c("top","bottom"),
                                 base_pt = 40, step_pt = 14, ...) {
  side <- match.arg(side)
  stopifnot(length(labels) == length(xstart),
            length(labels) == length(xend),
            length(labels) == length(y))
  specs_df <- data.frame(
    side  = rep(side, length(labels)),
    start = xstart,
    end   = xend,
    pos   = y,
    label = labels,
    stringsAsFactors = FALSE
  )
  add_brackets(p, specs_df, base_pt = base_pt, step_pt = step_pt, ...)
}

#' Stack multiple vertical brackets
#'
#' Adds several vertical brackets at different \code{x} positions (can be outside).
#' Automatically bumps left/right margins for stacked outside layers.
#'
#' @param p A ggplot.
#' @param labels Character vector, length N.
#' @param ystart,yend Numeric vectors (0–1), length N.
#' @param x Numeric vector (0–1; can be outside), length N.
#' @param side "right" or "left".
#' @param base_pt,step_pt Margin handling (see \code{\link{add_brackets}}).
#' @param ... Passed to \code{\link{bracket_grob}}.
#'
#' @return A ggplot with all brackets added.
#' @examples
#' \donttest{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
#' add_vertical_stack(
#'   p,
#'   labels = paste("B", 1:3),
#'   ystart = c(0.1, 0.4, 0.7),
#'   yend   = c(0.3, 0.6, 0.9),
#'   x      = c(1.04, 1.10, 1.18),
#'   side   = "right"
#' )
#' }
#' @export
add_vertical_stack <- function(p, labels, ystart, yend, x,
                               side = c("right","left"),
                               base_pt = 40, step_pt = 14, ...) {
  side <- match.arg(side)
  stopifnot(length(labels) == length(ystart),
            length(labels) == length(yend),
            length(labels) == length(x))
  specs_df <- data.frame(
    side  = rep(side, length(labels)),
    start = ystart,
    end   = yend,
    pos   = x,
    label = labels,
    stringsAsFactors = FALSE
  )
  add_brackets(p, specs_df, base_pt = base_pt, step_pt = step_pt, ...)
}

#' Add facet-targeted brackets using a spec data frame
#'
#' Adds brackets only to the facets whose rows match \code{specs_df}'s facet columns.
#' Automatically adjusts margins for outside/stacked layers per side.
#'
#' @param p A faceted ggplot.
#' @param specs_df Data frame with the facet columns present in \code{p$data}, plus
#'   NPC columns \code{start}, \code{end}, \code{pos}, and \code{side}, and a \code{label}.
#' @param start_col,end_col,pos_col,label_col,side_col Column names in \code{specs_df}.
#' @param margin_pt Base margin (points) used by the stacking-aware bump. Default `40`.
#' @param ... Passed to \code{\link{bracket_grob}}.
#'
#' @return A ggplot with facet-specific brackets.
#' @examples
#' \donttest{
#' pf <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
#'   ggplot2::geom_point() +
#'   ggplot2::facet_wrap(~cyl)
#'
#' specs <- data.frame(
#'   cyl   = c(4, 6),
#'   side  = c("top","right"),
#'   start = c(0.2, 0.2),
#'   end   = c(0.8, 0.8),
#'   pos   = c(1.05, 1.05),
#'   label = c("Facet 4", "Facet 6")
#' )
#' add_faceted_brackets(pf, specs)
#' }
#' @export
add_faceted_brackets <- function(p, specs_df,
                                 start_col = "start", end_col = "end", pos_col = "pos",
                                 label_col = "label", side_col = "side",
                                 margin_pt = 40, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }

  # stacking-aware margin bump
  p <- .bump_for_specs(p, specs_df,
                       base_pt = margin_pt, step_pt = 14,
                       side_col = side_col, pos_col = pos_col)

  for (i in seq_len(nrow(specs_df))) {
    side  <- specs_df[[side_col]][i]
    start <- specs_df[[start_col]][i]
    end   <- specs_df[[end_col]][i]
    pos   <- specs_df[[pos_col]][i]
    lab   <- specs_df[[label_col]][i]

    grob_i <- switch(
      side,
      "top"    = bracket_grob("top",    start, end, pos, lab, ...),
      "bottom" = bracket_grob("bottom", start, end, pos, lab, ...),
      "left"   = bracket_grob("left",   start, end, pos, lab, ...),
      "right"  = bracket_grob("right",  start, end, pos, lab, ...)
    )

    p <- p + ggplot2::annotation_custom(
      grob = grob_i,
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
      data = specs_df[i, , drop = FALSE]
    )
  }
  p
}
#' A minimal, print-friendly theme with Arial 10pt (not bold)
#'
#' Use this to make all text default to Arial, 10 pt, plain (not bold).
#' Call [set_pmplot_theme()] once per session to apply globally.
#'
#' @param base_family Font family, default "Arial".
#' @param base_size   Base text size (points), default 10.
#' @param base_face   Base text face, default "plain".
#' @return A ggplot2 theme object.
#' @export
theme_pmplot <- function(base_family = "Arial", base_size = 10, base_face = "plain") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(family = base_family, size = base_size, face = base_face)
    )
}

#' Apply pmplot theme globally
#'
#' Sets [theme_pmplot()] as the active theme for this R session.
#' @inheritParams theme_pmplot
#' @return (Invisibly) the previous theme, so you can restore it if needed.
#' @examples
#' \dontrun{
#' old <- set_pmplot_theme()
#' # ... make plots ...
#' ggplot2::theme_set(old)  # restore
#' }
#' @export
set_pmplot_theme <- function(base_family = "Arial", base_size = 10, base_face = "plain") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  old <- ggplot2::theme_set(theme_pmplot(base_family, base_size, base_face))
  invisible(old)
}

