
# quando a data da taxa estiver diferente da data da curva, fazer o quê?
# na interpolação do primeiro vencimento, o que fazer quando tiver um forward
# após a próxima reunião.


source("utils-functions.R")
library(flatforwardCOPOM)

curve <- get_curve_from_web()
refdate <- curve@refdate
.copom_dates <- get_copom_dates(refdate, 4)
interpolation(curve) <- interp_flatforwardcopom(.copom_dates, "second")

curve[[seq_len(252)]] |> forwardrate()

interpolation(curve)@moves

interpolation_error(curve)

# ----

plot_curve(fixedincome::first(curve, "2 years"), .copom_dates)
plot_copom_curve(fixedincome::first(curve, "2 years"), .copom_dates)

copom_dates <- .copom_dates
curve <- fixedincome::first(curve, "1 years")
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
    colour = "grey",
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
  geom_line(
    data = copom_curve,
    mapping = aes(x = dates, y = rates, colour = .names[1]),
    size = 1
  )

g <- g +
  scale_colour_manual("", breaks = .names, values = .colors)

.title <- glue("Curva a Termo de Juros Prefixados DI1 - {refdate}",
  refdate = format(curve@refdate)
)
g <- g +
  labs(
    x = "Data",
    y = "%",
    title = .title,
    subtitle = "As linhas cinza tracejadas representam as datas do COPOM",
    caption = "Desenvolvido por wilsonfreitas (com dados da B3)"
  ) +
  theme_wf(base_size = 20) +
  scale_y_continuous(labels = scales::percent)

g