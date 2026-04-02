library(openair)
library(ggplot2)
library(tidyverse)

# Taylor Diagram ----------------------------------------------------------

load(url(
  "https://github.com/openair-project/book/raw/refs/heads/main/assets/data/modelData.RData"
))

geom_segment_straight <- function(...) {
  layer <- geom_segment(...)
  new_layer <- ggproto(NULL, layer)
  old_geom <- new_layer$geom
  geom <- ggproto(
    NULL,
    old_geom,
    draw_panel = function(
      data,
      panel_params,
      coord,
      arrow = NULL,
      arrow.fill = NULL,
      lineend = "butt",
      linejoin = "round",
      na.rm = FALSE
    ) {
      data <- ggplot2:::remove_missing(
        data,
        na.rm = na.rm,
        c("x", "y", "xend", "yend", "linetype", "size", "shape")
      )
      if (ggplot2:::empty(data)) {
        return(zeroGrob())
      }
      coords <- coord$transform(data, panel_params)
      # xend and yend need to be transformed separately, as coord doesn't understand
      ends <- transform(data, x = xend, y = yend)
      ends <- coord$transform(ends, panel_params)

      arrow.fill <- if (!is.null(arrow.fill)) arrow.fill else coords$colour
      return(grid::segmentsGrob(
        coords$x,
        coords$y,
        ends$x,
        ends$y,
        default.units = "native",
        gp = grid::gpar(
          col = alpha(coords$colour, coords$alpha),
          fill = alpha(arrow.fill, coords$alpha),
          lwd = 2 * .pt,
          lty = coords$linetype,
          lineend = lineend,
          linejoin = linejoin
        ),
        arrow = arrow
      ))
    }
  )
  new_layer$geom <- geom
  return(new_layer)
}

LH <- dplyr::filter(modTest, site == "Lullington.Heath")

td_sd <-
  TaylorDiagram(
    LH,
    obs = "o3",
    mod = "mod",
    group = "group",
    key.position = "none",
    plot = FALSE,
    cols = "greyscale",
    rms.col = "transparent"
  )$plot +
  annotate(
    geom = "segment",
    x = 0.68,
    y = 0,
    xend = 0.68,
    yend = 25.5,
    color = "red",
    arrow = arrow(),
    linewidth = 2
  ) +
  theme(panel.grid.major.x = element_blank())

ggsave(
  "assets/media/TaylorExplainSD.png",
  td_sd,
  width = 5,
  height = 5,
  dpi = 300
)

td_r <-
  TaylorDiagram(
    LH,
    obs = "o3",
    mod = "mod",
    group = "group",
    key.position = "none",
    plot = FALSE,
    cols = "greyscale",
    rms.col = "transparent"
  )$plot +
  annotate(
    geom = "segment",
    y = 26.5,
    yend = 26.5,
    x = 0,
    xend = 0.65,
    color = "red",
    arrow = arrow(),
    linewidth = 2
  ) +
  theme(panel.grid.major.y = element_blank())

ggsave(
  "assets/media/TaylorExplainR.png",
  td_r,
  width = 5,
  height = 5,
  dpi = 300
)

td_rms <-
  TaylorDiagram(
    LH,
    obs = "o3",
    mod = "mod",
    group = "group",
    key.position = "none",
    plot = FALSE,
    cols = "greyscale"
  )$plot +
  geom_segment_straight(
    inherit.aes = FALSE,
    data = data.frame(x = 28.5, xend = 26.5, y = 1, yend = 0.71),
    mapping = aes(x = y, y = x, xend = yend, yend = xend),
    color = "red",
    arrow = arrow()
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(
  "assets/media/TaylorExplainRMS.png",
  td_rms,
  width = 5,
  height = 5,
  dpi = 300
)
