
# quando a data da taxa estiver diferente da data da curva, fazer o quê?
# na interpolação do primeiro vencimento, o que fazer quando tiver um forward
# após a próxima reunião.


source("copom-functions.R")

curve <- get_curve_from_web()
refdate <- curve$refdate[1]
rng <- range(curve$maturity_date)
copom_dates <- add.bizdays(copom_dates, 1, "Brazil/ANBIMA")
ix <- copom_dates >= rng[1] & copom_dates <= rng[2]
.copom_dates <- copom_dates[ix][1:4]
parts <- split_curve_into_copom_dates(curve, .copom_dates)
copom_curve <- copom_calc(parts, 1, conflicts = "forward")

# ----

plot_curve <- function(curve, copom_dates) {
  
  .dash <- "#4f7f81"
  .colors <- c("#e05305", "#fbb407")
  .names <- c("Curva Zero", "Curva Forward")
  names(.colors) <- .names
  
  g <- ggplot() +
    geom_vline(xintercept = copom_dates, colour = .dash,
               linetype = "dashed", size = 1) +
    geom_line(
      data = curve,
      mapping = aes(x = maturity_date, y = adjusted_tax, colour = .names[1]),
      size = 1
    ) +
    geom_point(
      data = curve,
      mapping = aes(x = maturity_date, y = adjusted_tax, colour = .names[1]),
      size = 2
    ) +
    geom_step(
      data = curve,
      mapping = aes(x = maturity_date, y = forward_tax, colour = .names[2]),
      size = 1,
      direction = "vh"
    ) +
    geom_point(
      data = curve,
      mapping = aes(x = maturity_date, y = forward_tax, colour = .names[2]),
      size = 2
    )

  g <- g +
    scale_colour_manual("", breaks = .names, values = .colors)

  .title <- glue("Curva de Juros Prefixados DI1 - {refdate}",
                 refdate = format(curve$refdate[1]))
  g <- g +
    labs(x = "Data",
         y = "%",
         title = .title,
         subtitle = "As linhas cinza tracejadas representam as datas do COPOM",
         caption = "Desenvolvido por wilsonfreitas (com dados da B3)") +
    theme_wf()
  g
}

plot_curve(curve |> filter(business_days < 504), .copom_dates)
