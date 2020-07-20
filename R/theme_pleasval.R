#' Make Plots Styled With PVCSD's Guidelines
#'
#' @description Make ggplot plots render with themes that are consistent with the design of PVCSD
#'
#' @export
theme_pleasval <-  function(){
  theme_minimal(base_size = 16, base_family = "") %+replace%
    theme(
      axis.text = element_text(size=rel(0.8),color = "#565656"),
      axis.ticks = element_line(color = "#D6D6D6"),
      legend.key = element_rect(colour = "White"),
      panel.grid.major = element_line(colour = "#C6C6C6", size = 0.2, lineend = "butt"),
      panel.grid.minor = element_line(colour = "#E6E6E6", size = 0.05, linetype = "longdash")
    )
}

