#' ggEvalue
#'
#' plot E value curve using ggplot2
#' @param estimate estimate value, for example, RR, OR and HR. class = numerical, length = 1.
#' @param lo lower 95\% CI of estimate value,  class = numerical, length = 1.
#' @param hi upper 95\% CI of estimate value,  class = numerical, length = 1.
#' @param xlim,ylim limitation for x axis with 2 numbers,
#' the first is the minimum value and the second is the maximum value. class = numerical, length = 2.
#' @param point.col,text.col,line.col colors for point, text and line. the first color is the CI and the second color is the E value.

#' @importFrom ggnewscale new_scale_color
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @import dplyr
#' @return a ggplot object.
#' @export
#'
ggEvalue <- function(estimate, lo, hi,
                     xlim = c(0,7),
                     ylim = c(0,7),
                     point.col = c('#90353b', '#1a476f'),
                     text.col = c('#90353b', '#1a476f'),
                     line.col = c('#0d680d', '#ff4602')){

  options(warn=-1)

  if (estimate < 1) estimate = 1/estimate
  if (lo < 1) lo = 1/lo
  if (hi < 1) hi = 1/hi

  xmax = xlim[2]
  ymax = ylim[2]

  x.est = seq(estimate, xmax, by = 0.01)
  y.est = estimate * (estimate - 1)/(x.est - estimate) + estimate
  e.est = estimate+sqrt(estimate*(estimate-1))

  x.ci = seq(lo, xmax, by = 0.01)
  y.ci = lo * (lo - 1)/(x.ci - lo) + lo
  e.ci = lo+sqrt(lo*(lo-1))

  n <- max(length(x.est), length(y.est), length(e.est),
           length(x.ci), length(y.ci), length(e.ci))

  length(x.est) <- n
  length(y.est) <- n
  length(e.est) <- n
  length(x.ci) <- n
  length(y.ci) <- n
  length(e.ci) <- n

  df1 =
    cbind(x.est, y.est, e.est, x.ci, y.ci, e.ci) %>%
    as_tibble()

  df2 =
    df1 %>%
    pivot_longer(everything(), names_to = c('.value', 'type'),
                 names_sep = '\\.') %>%
    mutate(text = case_when(type == 'est' ~ glue::glue("Evalue: ({round(e, 2)}, {round(e, 2)})"),
                            type == 'ci'  ~ glue::glue("Evalue (CI): ({round(e, 2)}, {round(e, 2)})")))

  p1 =
    df2 %>%
    ggplot() +

    geom_line(aes(x = x, y = y, color = type)) +
    scale_color_manual(values = line.col)+

    ggnewscale::new_scale_color() +
    geom_point(aes(x = e, y = e, color = type))+
    scale_color_manual(values = point.col)+

    ggnewscale::new_scale_color() +
    geom_text(
      aes(x = e, y = e, label = text, color = type),
      hjust = -0.1,
      vjust =  0.1) +
    scale_color_manual(values = text.col)+

    scale_y_continuous(limits = xlim) +
    scale_x_continuous(limits = ylim) +

    xlab('exposure-confounder relationship') +
    ylab('confounder-outcome relationship') +

    theme_classic() +
    theme(axis.title = element_text(face = 'bold'),
          axis.text = element_text(color = 'black'),
          axis.ticks = element_line(color = 'black'),
          legend.position = 'none',
          panel.border = element_rect(color = 'black', fill = NA))
  p1
}
