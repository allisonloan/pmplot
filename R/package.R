#' Build a bracket grob in panel NPC coordinates
#'
#' Creates a bracket (with caps and a label) in panel-normalized coordinates (NPC; 0–1).
#' @param side One of "top","bottom","left","right".
#' @param start,end For horizontal: x range (0–1). For vertical: y range (0–1).
#' @param pos For horizontal: y (0–1). For vertical: x (0–1). Values outside [0,1] draw outside the panel.
#' @param label Text label.
#' @param cap Length of the cap "tick" in NPC units. Default 0.015.
#' @param label_offset Gap between bracket and label in NPC. Default 0.05.
#' @param lwd Line width. Default 1.5.
#' @param label_size Label size (points). Default 10.
#' @param fontface Font face ("plain","bold",...). Default "plain".
#' @param family Font family. Default NULL to avoid system font issues on CI.
#' @param cex Deprecated; use label_size.
#' @return A grid grob for use with ggplot2::annotation_custom()
#' @export
bracket_grob <- function(
    side = c("top","bottom","left","right"),
    start, end, pos, label,
    cap = 0.015, label_offset = 0.05, lwd = 1.5,
    label_size = 10, fontface = "plain", family = NULL,
    cex = NULL
){
  side <- match.arg(side)
  if (!requireNamespace("grid", quietly = TRUE)) stop("Package 'grid' is required.")

  # Soft deprecate `cex`
  if (!is.null(cex)) {
    if (requireNamespace("lifecycle", quietly = TRUE)) {
      lifecycle::deprecate_warn("1.0.0", "bracket_grob(cex)", "bracket_grob(label_size)")
    } else {
      warning("`cex` is deprecated; use `label_size`.", call. = FALSE)
    }
    # Translate cex (~line-height scale) to points if label_size not set explicitly
    if (missing(label_size) || is.null(label_size)) label_size <- 10 * cex
  }

  withCallingHandlers({
    if (side %in% c("top","bottom")) {
      y_line <- grid::unit(c(pos, pos), "npc")
      x_line <- grid::unit(c(start, end), "npc")
      cap_dir <- if (side == "top") -cap else cap
      x_capL <- grid::unit(start, "npc"); x_capR <- grid::unit(end, "npc")
      y_cap  <- grid::unit(c(pos, pos + cap_dir), "npc")
      y_lab  <- grid::unit(pos + if (side == "top") label_offset else -label_offset, "npc")
      x_lab  <- grid::unit((start + end)/2, "npc")

      grid::grobTree(
        grid::linesGrob(x = x_line, y = y_line, gp = grid::gpar(lwd = lwd)),
        grid::linesGrob(x = x_capL, y = y_cap,  gp = grid::gpar(lwd = lwd)),
        grid::linesGrob(x = x_capR, y = y_cap,  gp = grid::gpar(lwd = lwd)),
        grid::textGrob(label, x = x_lab, y = y_lab,
                       gp = grid::gpar(fontsize = label_size, fontface = fontface, fontfamily = family))
      )
    } else {
      x_line <- grid::unit(c(pos, pos), "npc")
      y_line <- grid::unit(c(start, end), "npc")
      cap_dir <- if (side == "right") -cap else cap
      y_capT <- grid::unit(start, "npc"); y_capB <- grid::unit(end, "npc")
      x_cap  <- grid::unit(c(pos, pos + cap_dir), "npc")
      x_lab  <- grid::unit(pos + if (side == "right") label_offset else -label_offset, "npc")
      y_lab  <- grid::unit((start + end)/2, "npc")
      rot    <- if (side == "right") 270 else 90

      grid::grobTree(
        grid::linesGrob(x = x_line, y = y_line, gp = grid::gpar(lwd = lwd)),
        grid::linesGrob(x = x_cap,  y = y_capT, gp = grid::gpar(lwd = lwd)),
        grid::linesGrob(x = x_cap,  y = y_capB, gp = grid::gpar(lwd = lwd)),
        grid::textGrob(label, x = x_lab, y = y_lab, rot = rot,
                       gp = grid::gpar(fontsize = label_size, fontface = fontface, fontfamily = family))
      )
    }
  }, error = function(e) stop("Failed to build bracket_grob: ", e$message))
}

# ------------------------
# Internal: margins + helpers
# ------------------------

#' @keywords internal
.bump_margin <- function(p, top = 0, right = 0, bottom = 0, left = 0) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("grid", quietly = TRUE))     stop("Package 'grid' is required.")
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
# Core adders
# ------------------------

#' Add a single bracket grob to a ggplot
#' @export
add_bracket <- function(p, grob) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  p + ggplot2::annotation_custom(grob = grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
}

#' Add multiple horizontal and/or vertical brackets in one call (specs df)
#' Expected columns: side, start, end, pos, label
#' @export
add_brackets <- function(p, specs_df, base_pt = 40, step_pt = 14, ...) {
  p <- .bump_for_specs(p, specs_df, base_pt = base_pt, step_pt = step_pt)
  for (i in seq_len(nrow(specs_df))) {
    side  <- specs_df[["side"]][i]
    start <- specs_df[["start"]][i]
    end   <- specs_df[["end"]][i]
    pos   <- specs_df[["pos"]][i]
    lab   <- specs_df[["label"]][i]
    p <- add_bracket(p, bracket_grob(side, start, end, pos, lab, ...))
  }
  p
}

#' Add facet-targeted brackets using a specs df (rows matched to facet columns)
#' @export
add_faceted_brackets <- function(p, specs_df,
                                 start_col = "start", end_col = "end", pos_col = "pos",
                                 label_col = "label", side_col = "side",
                                 margin_pt = 60, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  p <- .bump_for_specs(p, specs_df,
                       base_pt = margin_pt, step_pt = 14,
                       side_col = side_col, pos_col = pos_col)

  for (i in seq_len(nrow(specs_df))) {
    side  <- specs_df[[side_col]][i]
    start <- specs_df[[start_col]][i]
    end   <- specs_df[[end_col]][i]
    pos   <- specs_df[[pos_col]][i]
    lab   <- specs_df[[label_col]][i]

    grob_i <- bracket_grob(side, start, end, pos, lab, ...)
    p <- p + ggplot2::annotation_custom(grob = grob_i, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  }
  p
}

# ------------------------
# Group-aware positions
# ------------------------

#' Compute NPC start/end from group indices (equal-width bins)
#' @export
npc_from_groups <- function(n_groups, g_start, g_end = g_start, along = c("x","y"), pad = 0) {
  along <- match.arg(along)
  stopifnot(n_groups >= 1, g_start >= 1, g_end >= g_start, g_end <= n_groups)
  stopifnot(pad >= 0, pad < 0.5)
  w <- 1 / n_groups
  start_raw <- (g_start - 1) * w
  end_raw   <- g_end * w
  pad_abs   <- pad * w
  c(start_raw + pad_abs, end_raw - pad_abs)
}

#' Add bracket(s) by group indices (any side)
#' @export
add_by_groups <- function(p, side = c("top","bottom","left","right"),
                          labels, n_groups, g_start, g_end = g_start,
                          pos = if (match.arg(side, c("top","bottom","left","right")) %in% c("top","right")) 1.06 else -0.06,
                          pad = 0.02, margin_pt = 60, ...) {
  side <- match.arg(side)
  len <- max(length(labels), length(g_start), length(g_end), length(pos))
  labels  <- rep(labels,  length.out = len)
  g_start <- rep(g_start, length.out = len)
  g_end   <- rep(g_end,   length.out = len)
  pos     <- rep(pos,     length.out = len)

  if (side == "top")    p <- .bump_margin(p, top    = margin_pt)
  if (side == "bottom") p <- .bump_margin(p, bottom = margin_pt)
  if (side == "left")   p <- .bump_margin(p, left   = margin_pt)
  if (side == "right")  p <- .bump_margin(p, right  = margin_pt)

  for (i in seq_len(len)) {
    rng <- npc_from_groups(
      n_groups,
      g_start[i], g_end[i],
      along = if (side %in% c("top","bottom")) "x" else "y",
      pad = pad
    )
    p <- add_bracket(p, bracket_grob(side, rng[1], rng[2], pos[i], labels[i], ...))
  }
  p
}

# ------------------------
# Helper: extract panel ranges & map data -> NPC
# ------------------------
.to_npc_coords <- function(p, side, start, end, pos) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  b  <- ggplot2::ggplot_build(p)
  # Use first panel by default; extend as needed for faceting
  pp <- b$layout$panel_params[[1]]

  .get_range <- function(pp, axis = c("x","y")) {
    axis <- match.arg(axis)
    # ggplot2 >= 3.4: pp$x$range$range; older: pp$x.range
    if (!is.null(pp[[axis]]) && !is.null(pp[[axis]]$range$range)) return(pp[[axis]]$range$range)
    if (!is.null(pp[[paste0(axis, ".range")]]))                 return(pp[[paste0(axis, ".range")]])
    if (!is.null(pp[[axis]]$range))                            return(pp[[axis]]$range)
    stop("Could not extract ", axis, " range from ggplot panel.")
  }

  xr <- .get_range(pp, "x")
  yr <- .get_range(pp, "y")

  .norm <- function(v, r) (v - r[1]) / (r[2] - r[1])

  if (side %in% c("top","bottom")) {
    list(
      start = .norm(start, xr),
      end   = .norm(end,   xr),
      pos   = .norm(pos,   yr)
    )
  } else {
    list(
      start = .norm(start, yr),
      end   = .norm(end,   yr),
      pos   = .norm(pos,   xr)
    )
  }
}
# helper (unchanged): map data -> NPC using panel ranges
.to_npc_coords <- function(p, side, start, end, pos) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  b  <- ggplot2::ggplot_build(p)
  pp <- b$layout$panel_params[[1]]

  .get_range <- function(pp, axis = c("x","y")) {
    axis <- match.arg(axis)
    if (!is.null(pp[[axis]]) && !is.null(pp[[axis]]$range$range)) return(pp[[axis]]$range$range)
    if (!is.null(pp[[paste0(axis, ".range")]]))                 return(pp[[paste0(axis, ".range")]])
    if (!is.null(pp[[axis]]$range))                            return(pp[[axis]]$range)
    stop("Could not extract ", axis, " range from ggplot panel.")
  }

  xr <- .get_range(pp, "x")
  yr <- .get_range(pp, "y")
  .norm <- function(v, r) (v - r[1]) / (r[2] - r[1])

  if (side %in% c("top","bottom")) {
    list(start = .norm(start, xr), end = .norm(end, xr), pos = .norm(pos, yr))
  } else {
    list(start = .norm(start, yr), end = .norm(end, yr), pos = .norm(pos, xr))
  }
}
# helper (unchanged): map data -> NPC using panel ranges
.to_npc_coords <- function(p, side, start, end, pos) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  b  <- ggplot2::ggplot_build(p)
  pp <- b$layout$panel_params[[1]]

  .get_range <- function(pp, axis = c("x","y")) {
    axis <- match.arg(axis)
    if (!is.null(pp[[axis]]) && !is.null(pp[[axis]]$range$range)) return(pp[[axis]]$range$range)
    if (!is.null(pp[[paste0(axis, ".range")]]))                 return(pp[[paste0(axis, ".range")]])
    if (!is.null(pp[[axis]]$range))                            return(pp[[axis]]$range)
    stop("Could not extract ", axis, " range from ggplot panel.")
  }

  xr <- .get_range(pp, "x")
  yr <- .get_range(pp, "y")
  .norm <- function(v, r) (v - r[1]) / (r[2] - r[1])

  if (side %in% c("top","bottom")) {
    list(start = .norm(start, xr), end = .norm(end, xr), pos = .norm(pos, yr))
  } else {
    list(start = .norm(start, yr), end = .norm(end, yr), pos = .norm(pos, xr))
  }
}
# ------------------------
# UPDATED: add_brackets_simple() with coord = "npc" | "data"
# ------------------------
#' Add brackets on one side with vectorized inputs.
#'
#' @param p ggplot object
#' @param side One of "top","bottom","left","right"
#' @param labels Character vector (length 1 ok)
#' @param start,end Positions along the bracket axis.
#'        If coord = "npc", values must be in [0,1].
#'        If coord = "data", values are in data units of that axis (e.g., x for top/bottom).
#' @param pos Position of the bracket line on the perpendicular axis.
#'        If coord = "npc", in [0,1] (values outside draw outside the panel).
#'        If coord = "data", in data units of the perpendicular axis (y for top/bottom, x for left/right).
#' @param coord "npc" (default) or "data" to interpret start/end/pos
#' @param margin_pt Base margin to add on the side (default 60)
#' @param ... Passed to `bracket_grob()` (e.g., label_size, fontface, family, cap, label_offset, lwd)
#' @export
# UPDATED: safely vectorize ... per bracket
add_brackets_simple <- function(p,
                                side = c("top","bottom","left","right"),
                                labels, start, end, pos,
                                coord = c("npc","data"),
                                margin_pt = 60, ...) {
  side  <- match.arg(side)
  coord <- match.arg(coord)

  # Base vectors (recycle)
  len <- max(length(labels), length(start), length(end), length(pos))
  labels <- rep(labels, length.out = len)
  start  <- rep(start,  length.out = len)
  end    <- rep(end,    length.out = len)
  pos    <- rep(pos,    length.out = len)

  # Convert from data -> NPC if requested
  if (coord == "data") {
    mapped <- .to_npc_coords(p, side, start, end, pos)
    start  <- mapped$start
    end    <- mapped$end
    pos    <- mapped$pos
  }

  # One-time margin bump on that side
  if (side == "top")    p <- .bump_margin(p, top    = margin_pt)
  if (side == "bottom") p <- .bump_margin(p, bottom = margin_pt)
  if (side == "left")   p <- .bump_margin(p, left   = margin_pt)
  if (side == "right")  p <- .bump_margin(p, right  = margin_pt)

  # Capture extra args and validate lengths
  dots <- list(...)
  if (length(dots)) {
    for (nm in names(dots)) {
      v <- dots[[nm]]
      if (!(length(v) %in% c(1L, len))) {
        stop(sprintf("Argument `%s` must be length 1 or %d (got %d).", nm, len, length(v)))
      }
    }
  }

  # Add all brackets, indexing ... per i
  for (i in seq_len(len)) {
    dots_i <- lapply(dots, function(v) if (length(v) == 1L) v else v[i])
    args_i <- c(
      list(side = side, start = start[i], end = end[i], pos = pos[i], label = labels[i]),
      dots_i
    )
    p <- add_bracket(p, do.call(bracket_grob, args_i))
  }
  p
}



# ------------------------
# OPTIONAL: minimal aliases (kept for discoverability)
# Comment out if you want the absolute-lean surface.
# ------------------------

#' @describeIn add_brackets_simple Convenience alias for top horizontal brackets
#' @export
add_horizontal_top  <- function(p, label, xstart, xend, y, margin_pt=60, ...) {
  add_brackets_simple(p, "top",    labels = label, start = xstart, end = xend, pos = y, margin_pt = margin_pt, ...)
}

#' @describeIn add_brackets_simple Convenience alias for bottom horizontal brackets
#' @export
add_horizontal_bot  <- function(p, label, xstart, xend, y, margin_pt=60, ...) {
  add_brackets_simple(p, "bottom", labels = label, start = xstart, end = xend, pos = y, margin_pt = margin_pt, ...)
}

#' @describeIn add_brackets_simple Convenience alias for right vertical brackets
#' @export
add_vertical_right  <- function(p, label, ystart, yend, x, margin_pt=60, ...) {
  add_brackets_simple(p, "right",  labels = label, start = ystart, end = yend, pos = x, margin_pt = margin_pt, ...)
}

#' @describeIn add_brackets_simple Convenience alias for left vertical brackets
#' @export
add_vertical_left   <- function(p, label, ystart, yend, x, margin_pt=60, ...) {
  add_brackets_simple(p, "left",   labels = label, start = ystart, end = yend, pos = x, margin_pt = margin_pt, ...)
}

# ------------------------
# Theme helpers (unchanged)
# ------------------------

#' Minimal print-friendly theme (base_family NULL by default)
#' @export
theme_pmplot <- function(base_family = NULL, base_size = 10, base_face = "plain") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(text = ggplot2::element_text(family = base_family, size = base_size, face = base_face))
}

#' Apply pmplot theme globally
#' @export
set_pmplot_theme <- function(base_family = NULL, base_size = 10, base_face = "plain") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  old <- ggplot2::theme_set(theme_pmplot(base_family, base_size, base_face))
  invisible(old)
}
