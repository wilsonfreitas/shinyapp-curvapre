
library(glue)
library(xml2)
library(stringr)
library(httr)
library(ggplot2)
library(dplyr)
library(bizdays)
library(forcats)
library(rb3)
library(fixedincome)

get_di1_curve <- function(refdate) {
  fut <- futures_get(refdate)
  yc <- yc_get(refdate)
  df <- yc_superset(yc, fut)

  df_curve <- bind_rows(
    df |> slice(1) |> select(biz_days, r_252),
    df |> filter(!is.na(symbol)) |> select(biz_days, r_252)
  ) |>
    filter(!duplicated(biz_days))

  spotratecurve(
    df_curve$r_252, df_curve$biz_days, "discrete", "business/252", "Brazil/ANBIMA",
    refdate = refdate
  )
}

plot_curve <- function(curve, copom_dates = NULL, show_forward = TRUE,
                       base_size = 20) {
  curve_fwd <- forwardrate(curve)
  curve_fwd <- spotratecurve(
    as.numeric(curve_fwd),
    cumsum(curve_fwd@terms),
    refdate = curve@refdate,
    .copyfrom = curve
  ) |> as.data.frame()
  curve_spt <- as.data.frame(curve)

  .dash <- "#4f7f81"
  .colors <- c("#e05305", "#fbb407")
  .names <- c("Curva Zero", "Curva Forward")
  .nidx <- 1
  names(.colors) <- .names

  g <- ggplot()

  if (!is.null(copom_dates)) {
    g <- g + geom_vline(
      xintercept = copom_dates, colour = .dash,
      linetype = "dashed", size = 1
    )
  }

  g <- g +
    geom_line(
      data = curve_spt,
      mapping = aes(x = dates, y = rates, colour = .names[1]),
      size = 1
    ) +
    geom_point(
      data = curve_spt,
      mapping = aes(x = dates, y = rates, colour = .names[1]),
      size = 2
    )

  if (show_forward) {
    g <- g +
      geom_step(
        data = curve_fwd,
        mapping = aes(x = dates, y = rates, colour = .names[2]),
        size = 1,
        direction = "vh"
      ) +
      geom_point(
        data = curve_fwd,
        mapping = aes(x = dates, y = rates, colour = .names[2]),
        size = 2
      )
    .nidx <- seq_along(.names)
  }

  g <- g +
    scale_colour_manual("", breaks = .names[.nidx], values = .colors[.nidx])

  .title <- glue("Curva de Juros Prefixados DI1 - {refdate}",
    refdate = format(curve@refdate)
  )

  .subtitle <- if (is.null(copom_dates)) {
    NULL
  } else {
    "As linhas cinza tracejadas representam as datas do COPOM"
  }

  g <- g +
    labs(
      x = "Data",
      y = NULL,
      title = .title,
      subtitle = .subtitle,
      caption = "Desenvolvido por wilsonfreitas (com dados da B3)"
    ) +
    theme_wf(base_size = base_size) +
    scale_y_continuous(labels = scales::percent)
  g
}

plot_copom_curve <- function(curve, copom_dates, base_size = 16) {
  curve_spt <- as.data.frame(curve)
  curve_fwd <- forwardrate(curve)
  curve_fwd <- spotratecurve(
    as.numeric(curve_fwd),
    cumsum(curve_fwd@terms),
    refdate = curve@refdate,
    .copyfrom = curve
  ) |> as.data.frame()
  copom_curve_fwd <- curve[[seq_len(max(curve@terms))]] |>
    forwardrate()
  copom_curve <- spotratecurve(
    as.numeric(copom_curve_fwd),
    cumsum(copom_curve_fwd@terms),
    refdate = curve@refdate,
    .copyfrom = curve
  ) |> as.data.frame()

  .dash <- "#4f7f81"
  .colors <- c("#e05305", "#fbb407")
  .names <- c("COPOM Forward", "DI1 Forward")
  names(.colors) <- .names

  g <- ggplot() +
    geom_vline(
      xintercept = copom_dates,
      colour = .dash,
      linetype = "dashed", size = 1
    ) +
    geom_step(
      data = curve_fwd,
      mapping = aes(x = dates, y = rates, colour = .names[2]),
      size = 1,
      direction = "vh"
    ) +
    geom_point(
      data = curve_fwd,
      mapping = aes(x = dates, y = rates, colour = .names[2]),
      size = 2
    ) +
    geom_step(
      data = copom_curve,
      mapping = aes(x = dates, y = rates, colour = .names[1]),
      size = 1,
      alpha = 0.75,
      direction = "hv"
    )

  g <- g +
    scale_colour_manual("", breaks = .names, values = .colors)

  .title <- glue("Curva a Termo de Juros Prefixados DI1 - {refdate}",
    refdate = format(curve@refdate)
  )
  g <- g +
    labs(
      x = "Data",
      y = NULL,
      title = .title,
      subtitle = "As linhas cinza tracejadas representam as datas do COPOM",
      caption = "Desenvolvido por wilsonfreitas (com dados da B3)"
    ) +
    theme_wf(base_size = base_size) +
    scale_y_continuous(labels = scales::percent)

  g
}

# Paleta de cores
# https://icolorpalette.com/e05305_4f7f81_fbb407_5d91a2_432608
theme_wf <- function(base_size = 12,
                     base_family = "mono",
                     base_line_size = base_size / 22,
                     base_rect_size = base_size / 22) {
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      legend.position = "top",
      text = element_text(family = base_family, size = base_size),
      plot.background = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank(),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      title = element_text(
        family = base_family,
        colour = "black",
        face = "bold"
      ),
      axis.line = element_line(colour = "grey92", size = 1),
      axis.title.y = element_text(colour = "black", face = "bold"),
      axis.title.x = element_text(colour = "black", face = "bold"),
      legend.key = element_rect(fill = "white", colour = NA),
      complete = TRUE
    )
}
