
library(glue)
library(xml2)
library(stringr)
library(httr)
library(ggplot2)
library(dplyr)
library(bizdays)
library(forcats)
library(rbcb)


copom_dates <- c("02/02/2022", "16/03/2022", "04/05/2022", "15/06/2022",
                 "03/08/2022", "21/09/2022", "26/10/2022", "07/12/2022",
                 "20/01/2021", "17/03/2021", "05/05/2021", "16/06/2021",
                 "04/08/2021", "22/09/2021", "27/10/2021", "08/12/2021",
                 "05/02/2020", "18/03/2020", "06/05/2020", "17/06/2020",
                 "05/08/2020", "16/09/2020", "28/10/2020", "09/12/2020",
                 "06/02/2019", "20/03/2019", "08/05/2019", "19/06/2019",
                 "31/07/2019", "18/09/2019", "30/10/2019", "11/12/2019",
                 "07/02/2018", "21/03/2018", "16/05/2018", "20/06/2018",
                 "01/08/2018", "19/09/2018", "31/10/2018", "12/12/2018",
                 "11/01/2017", "22/02/2017", "12/04/2017", "31/05/2017",
                 "26/07/2017", "06/09/2017", "25/10/2017", "06/12/2017",
                 "20/01/2016", "02/03/2016", "27/04/2016", "08/06/2016",
                 "20/07/2016", "31/08/2016", "19/10/2016", "29/11/2016",
                 "21/01/2015", "04/03/2015", "29/04/2015", "03/06/2015",
                 "29/07/2015", "02/09/2015", "21/10/2015", "25/11/2015") |>
  as.Date("%d/%m/%Y") |> 
  sort()

# functions ----

split_curve_into_copom_dates <- function(curve, copom_dates) {
  lapply(seq_along(copom_dates), function(x) {
    if (x == 1) {
      p1 <- curve[1,]
      p2 <- curve |>
        filter(maturity_date >= copom_dates[x])
      list(
        copom_date = copom_dates[x],
        futures = rbind(p1, p2[1,])
      )
    } else if (!is.na(copom_dates[x+1])) {
      p1 <- curve |>
        filter(
          maturity_date >= copom_dates[x],
          maturity_date <= copom_dates[x+1],
        )
      list(
        copom_date = copom_dates[x],
        futures = p1
      )
    } else {
      p1 <- curve |>
        filter(maturity_date >= copom_dates[x])
      list(
        copom_date = copom_dates[x],
        futures = p1[1,]
      )
    }
  })
}

copom_curve_term <- function(refdate,
                             du_copom,
                             copom_date,
                             zero,
                             fact_zero,
                             copom_forward,
                             move) {
  data.frame(
    refdate = refdate,
    du_copom = du_copom,
    copom_date = copom_date,
    zero = zero,
    fact_zero = fact_zero,
    copom_forward = copom_forward,
    move = move
  )
}

calc_first_term <- function(parts, x, results) {
  copom_date <- parts[[x]]$copom_date
  futs <- parts[[x]]$futures
  refdate <- futs$refdate[1]
  du_copom <- bizdays(refdate, copom_date, "Brazil/ANBIMA")
  
  du_fut <- futs$business_days
  fact_fut <- (1 + futs$adjusted_tax[2]/100) ^ (du_fut[2]/252)
  fact_cdi <- (1 + futs$adjusted_tax[1]/100) ^ (du_copom/252)
  du_rem <- du_fut[2] - du_copom
  
  if (du_rem > 0) {
    fwd <- (fact_fut / fact_cdi) ^ (252/du_rem) - 1
    copom_curve_term(
      refdate = refdate,
      du_copom = du_copom,
      copom_date = copom_date,
      zero = futs$adjusted_tax[1],
      fact_zero = (1 + futs$adjusted_tax[1]/100) ^ (du_copom/252),
      copom_forward = 100 * fwd,
      move = 1e4 * (fwd - futs$adjusted_tax[1]/100)
    )
  } else {
    fwd <- 10.65/100
    copom_curve_term(
      refdate = refdate,
      du_copom = du_copom,
      copom_date = copom_date,
      zero = futs$adjusted_tax[1],
      fact_zero = (1 + futs$adjusted_tax[1]/100) ^ (du_copom/252),
      copom_forward = 100 * fwd,
      move = 1e4 * (fwd - futs$adjusted_tax[1]/100)
    )
  }
}

calc_with_forward <- function(parts, x, results) {
  copom_date <- parts[[x]]$copom_date
  futs <- parts[[x]]$futures
  refdate <- futs$refdate[1]
  du_copom <- bizdays(refdate, copom_date, "Brazil/ANBIMA")
  
  last_result <- results[[length(results)]]
  
  zero <- (last_result$fact_zero * (1 + last_result$copom_forward/100) ^ ((du_copom - last_result$du_copom)/252)) ^ (252/du_copom) - 1
  fwd <- futs$forward_tax[2]
  copom_curve_term(
    refdate = refdate,
    du_copom = du_copom,
    copom_date = copom_date,
    zero = 100 * zero,
    fact_zero = (1 + zero) ^ (du_copom/252),
    copom_forward = fwd,
    move = 1e2 * (fwd - last_result$copom_forward)
  )
}

calc_with_optim <- function(parts, x, results) {
  copom_date <- parts[[x]]$copom_date
  futs <- parts[[x]]$futures
  refdate <- futs$refdate[1]
  du_copom <- bizdays(refdate, copom_date, "Brazil/ANBIMA")
  
  last_result <- results[[length(results)]]
  
  zero <- (last_result$fact_zero * (1 + last_result$copom_forward/100) ^ ((du_copom - last_result$du_copom)/252)) ^ (252/du_copom) - 1
  
  fact_fut <- (1 + futs$adjusted_tax/100) ^ (futs$business_days/252)
  fact_copom <- (1 + zero) ^ (du_copom/252)
  du_rem <- futs$business_days - du_copom
  f_obj <- function(x) {
    fact_obj <- fact_copom * (1 + x) ^ (du_rem/252)
    sum(fact_fut - fact_obj) ^ 2
  }
  res <- optim(futs$forward_tax[2], f_obj, method = "Brent", lower = 0, upper = 1)
  fwd <- 100*res$par
  
  copom_curve_term(
    refdate = refdate,
    du_copom = du_copom,
    copom_date = copom_date,
    zero = 100 * zero,
    fact_zero = (1 + zero) ^ (du_copom/252),
    copom_forward = fwd,
    move = 1e2 * (fwd - last_result$copom_forward)
  )
}

calc_with_first_future <- function(parts, x, results) {
  copom_date <- parts[[x]]$copom_date
  futs <- parts[[x]]$futures
  refdate <- futs$refdate[1]
  du_copom <- bizdays(refdate, copom_date, "Brazil/ANBIMA")
  
  last_result <- results[[length(results)]]
  
  zero <- (last_result$fact_zero * (1 + last_result$copom_forward/100) ^ ((du_copom - last_result$du_copom)/252)) ^ (252/du_copom) - 1
  
  fact_fut <- (1 + futs$adjusted_tax[1]/100) ^ (futs$business_days[1]/252)
  fact_copom <- (1 + zero) ^ (du_copom/252)
  du_rem <- futs$business_days[1] - du_copom
  fwd <- (fact_fut / fact_copom) ^ (252/du_rem) - 1
  
  copom_curve_term(
    refdate = refdate,
    du_copom = du_copom,
    copom_date = copom_date,
    zero = 100 * zero,
    fact_zero = fact_copom,
    copom_forward = 100 * fwd,
    move = 1e4 * (fwd - last_result$copom_forward/100)
  )
}

copom_calc <- function(parts, x, results = NULL, conflicts = c("forward", "first", "optimize")) {
  
  if (x > length(parts)) {
    df <- do.call(rbind, results) |> 
      rename(maturity_date = copom_date,
             adjusted_tax = zero,
             forward_tax = copom_forward,
             business_days = du_copom) |> 
      select(maturity_date, refdate, adjusted_tax, business_days, forward_tax, move)
    return(df)
  }
  
  conflicts <- match.arg(conflicts)
  futs <- parts[[x]]$futures
  if (x == 1) {
    result <- calc_first_term(parts, x, results)
    results[[length(results) + 1]] <- result
    copom_calc(parts, x+1, results, conflicts)
  } else {
    result <- if (nrow(futs) == 2) {
      if (conflicts == "forward") {
        calc_with_forward(parts, x, results)
      } else if (conflicts == "first") {
        calc_with_first_future(parts, x, results)
      } else if (conflicts == "optimize") {
        calc_with_optim(parts, x, results)
      }
    } else if (nrow(futs) == 0) {
      NULL
    } else {
      calc_with_first_future(parts, x, results)
    }
    results[[length(results) + 1]] <- result
    copom_calc(parts, x+1, results, conflicts)
  }
}

cdi_rate_from_web <- function(refdate = NULL) {
  if (is.null(refdate)) {
    url <- "https://www2.cetip.com.br/ConsultarTaxaDi/ConsultarTaxaDICetip.aspx"
    
    res <- GET(url)
    .json <- content(res, as = "text") |> 
      jsonlite::fromJSON()
    
    refdate <- as.Date(.json$dataTaxa, "%d/%m/%Y")
    
    # if (refdate == as.Date("2022-02-02")) {
    #   data.frame(
    #     refdate = refdate,
    #     CDI = 10.65
    #   )  
    # } else {
    #   data.frame(
    #     refdate = refdate,
    #     CDI = .json$taxa |> 
    #       str_replace(",", ".") |> 
    #       as.numeric()
    #   )  
    # }
    data.frame(
      refdate = refdate,
      CDI = .json$taxa |> 
        str_replace(",", ".") |> 
        as.numeric()
    )  
  } else {
      df <- get_series(c(CDI = 4389),
                       start_date = refdate,
                       end_date = refdate)
      data.frame(
        refdate = df$date,
        CDI = df$CDI
      )
  }
}

forward_curve <- function(curve) {
  curve |>
    mutate(
      DUB = business_days - lag(business_days),
      FatorTaxa = (1 + adjusted_tax / 100) ^ (business_days/252),
      FatorTaxa_b = lag(FatorTaxa),
      forward_tax = ((FatorTaxa / FatorTaxa_b) ^ (252/DUB) - 1) * 100
    ) |>
    select(-DUB, -FatorTaxa, -FatorTaxa_b)
}

plot_curve <- function(curve, copom_dates, base_size = 20) {
  
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
    theme_wf(base_size = 16)
  g
}

plot_copom_curve <- function(curve, copom_curve, copom_dates, base_size = 16) {
  .dash <- "#4f7f81"
  .colors <- c("#e05305", "#fbb407")
  .names <- c("COPOM Forward", "DI1 Forward")
  names(.colors) <- .names
  
  curve <- curve |>
    filter(maturity_date <= add.bizdays(refdate, 252, "Brazil/ANBIMA"))
  
  g <- ggplot() +
    geom_vline(
      xintercept = copom_dates,
      colour = .dash,
      linetype = "dashed", size = 1
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
    ) +
    geom_step(
      data = copom_curve,
      mapping = aes(x = maturity_date, y = forward_tax, colour = .names[1]),
      size = 1,
      direction = "hv"
    ) +
    geom_point(
      data = copom_curve,
      mapping = aes(x = maturity_date, y = forward_tax, colour = .names[1]),
      size = 2
    )
  
  g <- g +
    scale_colour_manual("", breaks = .names, values = .colors)
  
  .title <- glue("Curva a Termo de Juros Prefixados DI1 - {refdate}",
                 refdate = format(curve$refdate[1]))
  g <- g +
    labs(x = "Data",
         y = "%",
         title = .title,
         subtitle = "As linhas cinza tracejadas representam as datas do COPOM",
         caption = "Desenvolvido por wilsonfreitas (com dados da B3)") +
    theme_wf(base_size = base_size)

  g
}

flatten_names <- function(nx) {
  # nx <- txt[c(T,F,F,F,F,F)]
  for (ix in seq_along(nx)) {
    if (nx[ix] != "") {
      last_name <- nx[ix]
    }
    nx[ix] <- last_name
  }
  x <- nx |> str_match("^...")
  as.vector(x)
}

get_contracts <- function(refdate) {
  url <- "https://www2.bmf.com.br/pages/portal/bmfbovespa/lumis/lum-ajustes-do-pregao-ptBR.asp"
  
  if (is.null(refdate)) {
    res <- GET(url)
  } else {
    strdate <- format(as.Date(refdate), "%d/%m/%Y")
    res <- POST(url, body = list(dData1 = strdate), encode = "form")
  }
  
  html <- content(res, as = "text", encoding = "latin1")
  mtx <- str_match(html, "Atualizado em: (\\d{2}/\\d{2}/\\d{4})")
  refdate <- mtx[1,2] |> as.Date("%d/%m/%Y")
  doc <- read_html(html, encoding = "latin1")
  tbl <- xml_find_all(doc, "//table[contains(@id, 'tblDadosAjustes')]")
  
  if (length(tbl) == 0)
    return(NULL)
  
  txt <- tbl[[1]] |>
    xml_find_all("//td") |>
    xml_text() |>
    str_trim() |>
    str_replace("\\.", "") |>
    str_replace(",", ".")
  
  tibble(
    DataRef    = as.Date(refdate),
    Mercadoria = flatten_names(txt[c(T,F,F,F,F,F)]),
    Vencimento = txt[c(F,T,F,F,F,F)],
    PUAnterior = txt[c(F,F,T,F,F,F)] |> as.numeric(),
    PUAtual    = txt[c(F,F,F,T,F,F)] |> as.numeric(),
    Variacao   = txt[c(F,F,F,F,T,F)] |> as.numeric()
  )
}

contract_to_maturity <- function(x) {
  maturity_code <- str_sub(x, -3)
  
  year <- as.integer( str_extract(maturity_code, "\\d\\d$") ) + 2000
  
  m_ <- c(F = 1, G = 2, H = 3, J = 4, K = 5, M = 6, N = 7, Q = 8, U = 9, V = 10, X = 11, Z = 12)
  month_code <- str_extract(maturity_code, "^.")
  month <- m_[month_code] |>
    str_pad(2, pad = "0")
  
  glue("{year}-{month}-01") |> as.Date()
}


get_curve_from_web <- function(refdate = NULL) {
  contracts <- get_contracts(refdate)
  refdate <- contracts$DataRef[1] |> as.Date()
  di1 <- contracts |>
    filter(Mercadoria == "DI1") |>
    mutate(
      maturity_date = contract_to_maturity(Vencimento) |>
        following("Brazil/ANBIMA")
    ) |>
    mutate(
      business_days = bizdays(DataRef, maturity_date, "Brazil/ANBIMA"),
      adjusted_tax = 100 * ((100000 / PUAtual)^(252/business_days) - 1)
    ) |>
    rename(refdate = DataRef) |> 
    filter(business_days != 0, business_days != 1) |>
    select(maturity_date, refdate, adjusted_tax, business_days) |> 
    arrange(maturity_date)
  
  rates <- cdi_rate_from_web(refdate)
  v1 <- data.frame(
    maturity_date = add.bizdays(refdate, 1, "Brazil/ANBIMA"),
    refdate = refdate,
    adjusted_tax = rates$CDI,
    business_days = 1
  )
  
  di1 <- rbind(v1, di1)
  
  forward_curve(di1) |> 
    mutate(
      refdate = as.Date(refdate),
      maturity_date = as.Date(maturity_date)
    )
}

# Paleta de cores
# https://icolorpalette.com/e05305_4f7f81_fbb407_5d91a2_432608
theme_wf <- function(base_size = 12,
                     base_family = "mono",
                     base_line_size = base_size/22,
                     base_rect_size = base_size/22) {
  theme_grey(base_size = base_size,
             base_family = base_family, 
             base_line_size = base_line_size,
             base_rect_size = base_rect_size) %+replace%
    theme(
      legend.position = "top",
      text = element_text(family = base_family, size = base_size),
      plot.background = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank(),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      title = element_text(family = base_family,
                           colour = "black",
                           face = "bold"),
      axis.line = element_line(colour = "grey92", size = 1),
      axis.title.y = element_text(colour = "black", face = "bold"),
      axis.title.x = element_text(colour = "black", face = "bold"),
      legend.key = element_rect(fill = "white", colour = NA),
      complete = TRUE
    )
}
