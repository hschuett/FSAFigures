tiu_colors <- c("blue"       = "#003366",
                "gold"       = "#CC9933",
                "tan"        = "#d9bc74",
                "lightblue"  = "#008EC6",
                "lightgold"  = "#C3BCB2",
                "yellow"     = "#D9BC75",
                "green"      = "#339900",
                "lightgreen" = "#66CC33",
                "lightgrey"  = "#CCCCCC",
                "midgrey"    = "#7F7F7F",
                "darkgrey"   = "#4C4C4C",
                "purple"     = "#666699",
                "lightgold2"  = "#f4ecdb")


theme_fsa <- function(frame_color = tiu_colors["darkgrey"],
                      frame_font_color = tiu_colors["darkgrey"],
                      grid_color = tiu_colors["lightgrey"]){
  theme_bw() +
    theme(legend.position = "bottom",
          legend.spacing.x = unit(2, "pt"),
          strip.background=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.x=element_blank(),
          panel.grid.major.y=element_line(color=grid_color,
                                          linetype="dotted"),
          panel.border     = element_blank(),
          axis.line = element_line(color=frame_color),
          axis.line.y = element_blank(),
          axis.text        = element_text(color = frame_font_color),
          axis.title       = element_text(color = frame_font_color),
          strip.text       = element_text(color = frame_font_color),
          legend.title     = element_text(color = frame_font_color),
          legend.text      = element_text(color = frame_font_color),
          axis.ticks = element_line(color = frame_color),
          axis.ticks.y = element_line(color = grid_color),
          plot.caption=element_text(size=7, color=frame_font_color)
    )
}
theme_set(theme_fsa())

ggplot2::update_geom_defaults("line", list(color=tiu_colors['blue']))
ggplot2::update_geom_defaults("point", list(color=tiu_colors['blue']))

