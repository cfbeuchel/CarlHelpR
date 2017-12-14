#' A custom ggplot2 theme.
#'
#' This is a ggplot2 theme for making minimalistic plots with.
#'
#' A small adjustment made to the theme_minimal() as was suggested here:
#' https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/
#'
#' @export

theme_carl <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(),
        text = ggplot2::element_text(family = 'Palatino'),
        legend.position = 'right'
        )
}
