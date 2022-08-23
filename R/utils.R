fig_colors <- c(
  "main1"      = "#003366",
  "main2"      = "#008EC6",
  "main3"      = "#66CC33",
  "main4"      = "#666699",
  "accent1"    = "#C3BCB2",
  "accent2"    = "#d9bc74",
  "accent3"    = "#CC9933",
  "accent4"  = "#f4ecdb",
  "lightgrey"  = "#CCCCCC",
  "midgrey"    = "#7F7F7F",
  "darkgrey"   = "#4C4C4C"
)
# for visualizing the vector
# plot(NULL, xlim=c(0,length(COL)), ylim=c(0,1),
#      xlab="", ylab="", xaxt="n", yaxt="n")
# rect(0:(length(COL)-1), 0, 1:length(COL), 1, col=COL)



save_png <- function(path, fig, fig_width = 6){
  ragg::agg_png(path, width = fig_width, height = fig_width * 0.618, units = "in", res = 144)
  print(fig)
  invisible(dev.off())
}



theme_fsa <- function(frame_color = fig_colors["darkgrey"],
                      frame_font_color = fig_colors["darkgrey"],
                      grid_color = fig_colors["lightgrey"]) {
  theme_bw() +
    theme(
      legend.position = "bottom",
      legend.spacing.x = unit(2, "pt"),
      strip.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        color = grid_color,
        linetype = "dotted"
      ),
      panel.border = element_blank(),
      axis.line = element_line(color = frame_color),
      axis.line.y = element_blank(),
      axis.text = element_text(color = frame_font_color),
      axis.title = element_text(color = frame_font_color),
      strip.text = element_text(color = frame_font_color),
      legend.title = element_text(color = frame_font_color),
      legend.text = element_text(color = frame_font_color),
      axis.ticks = element_line(color = frame_color),
      axis.ticks.y = element_line(color = grid_color),
      plot.caption = element_text(size = 7, color = frame_font_color)
    )
}
theme_set(theme_fsa())

ggplot2::update_geom_defaults("line", list(color = fig_colors[["main1"]]))
ggplot2::update_geom_defaults("point", list(color = fig_colors[["main1"]]))
