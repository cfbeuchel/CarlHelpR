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
        # panel.grid.major = ggplot2::element_line(colour = "grey50"),
        # panel.grid.minor = ggplot2::element_line(colour = "grey50"),
        axis.line = ggplot2::element_line(),
        # text = ggplot2::element_text(family = 'Palatino'),
        legend.position = 'right',
        text = element_text(size=15),
        panel.border = element_blank(),
        axis.line = element_blank()
        )
}
