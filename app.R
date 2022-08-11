
library(shiny)
library(formattable)
library(lubridate)
library(bslib)
library(readr)
library(DT)
library(copom)
library(ggplot2)

source("utils-functions.R")

CURVE <- get_di1_curve(getdate("last bizday", Sys.Date() - 1, "Brazil/ANBIMA"))

.theme <- bs_theme(
    fg = "#fff",
    bg = "#000"
)

ui <- fluidPage(
    theme = .theme,
    titlePanel("Curva de Juros Prefixados DI1"),
    fluidRow(
        column(
            2, div(span("1", style = "color: black;")),
            dateInput("refdate", "Data de referência", CURVE@refdate)
        ),
        column(
            2, div(span("1", style = "color: black;")),
            numericInput("num_meetings", "#Datas do COPOM", 6)
        )
    ),
    br(),
    tabsetPanel(
        tabPanel(
            "Curva Prefixada (DI1) 2 anos",
            div(span("1", style = "color: black;")),
            downloadButton("downloadCurvaPre",
                "CSV Curva Prefixada",
                icon = icon("download")
            ),
            div(span("1", style = "color: black;")),
            plotOutput("curvePre", height = "400px"),
        ),
        tabPanel(
            "Expectativas de Taxa DI",
            div(span("1", style = "color: black;")),
            fluidRow(
                column(2, downloadButton("downloadCopom",
                    "CSV Juros a Termo e Choques",
                    icon = icon("download")
                )),
                column(4, radioButtons("copomModels", "Solução de Conflitos", c(
                    "Primeiro Futuro" = "first",
                    "Segundo Futuro" = "second",
                    "Taxa Forward" = "forward",
                    "Otimização" = "optimize"
                ), "second", TRUE))
            ),
            div(span("1", style = "color: black;")),
            fluidRow(
                column(
                    plotOutput("curveCopom", height = "400px"),
                    width = 8
                ),
                column(
                    width = 4,
                    dataTableOutput("tableCopom"),
                )
            )
        ),
        tabPanel(
            "Curva Prefixada (DI1) Completa",
            div(span("1", style = "color: black;")),
            fluidRow(
                div(span("1", style = "color: black;")),
                column(2, checkboxInput("di1c_show_fwd", "Mostrar Curva a Termo")),
                column(3, dateInput("di1c_second_date", "Adicionar segunda curva", CURVE@refdate))
            ),
            div(span("1", style = "color: black;")),
            plotOutput("curvePreComplete", height = "400px"),
        ),
    )
)

server <- function(input, output, session) {
    curvePre <- reactive({
        validate(need(is.bizday(input$refdate, "Brazil/ANBIMA"), "Sem curva para data"))
        get_di1_curve(input$refdate)
    })

    refdate <- reactive({
        curve <- curvePre()
        curve@refdate
    })

    copomDates <- reactive({
        get_copom_dates(refdate(), input$num_meetings)
    })

    curveCopom <- reactive({
        curve <- curvePre()
        .copom_dates <- copomDates()
        interpolation(curve) <- interp_flatforwardcopom(.copom_dates, input$copomModels)
        curve
    })

    curveCopomFormatted <- reactive({
        curve <- curveCopom()
        moves <- interpolation(curve)@moves
        moves |>
            mutate(
                moves = moves * 1e4,
                forward_rates = 100 * as.numeric(forward_rates)
            ) |>
            rename(
                `Data do COPOM` = dates,
                `Taxa a termo` = forward_rates,
                `Var.` = moves
            )
    })

    output$curvePre <- renderPlot({
        curve <- curvePre()
        .copom_dates <- copomDates()
        interp_ <- interpolation(curve)
        curve_2y <- fixedincome::first(curve, "2 years")
        interpolation(curve_2y) <- interp_
        plot_curve(curve_2y, .copom_dates)
    })

    output$curvePreComplete <- renderPlot({
        curve <- curvePre()
        g <- ggspotratecurveplot(curve,
            title = "Curva de Juros Prefixados DI1",
            caption = "Desenvolvido por wilsonfreitas (com dados da B3)"
        )
        if (input$di1c_show_fwd) {
            g <- g + autolayer(forwardrate(curve), size = 1)
        }
        if (input$di1c_second_date != refdate()) {
            curve2 <- get_di1_curve(input$di1c_second_date)
            g <- g + autolayer(curve2, size = 1) +
                autolayer(curve2, size = 2, curve.geom = "point")
        }
        g
    })

    output$curveCopom <- renderPlot({
        curve <- curveCopom()
        .copom_dates <- copomDates()
        interp_ <- interpolation(curve)
        curve_1y <- fixedincome::first(curve, "1 years")
        interpolation(curve_1y) <- interp_
        plot_copom_curve(curve_1y, .copom_dates)
    })

    output$tableCopom <- renderDataTable({
        curveCopomFormatted() |>
            mutate(
                `Taxa a termo` = accounting(`Taxa a termo`, digits = 2),
                `Var.` = accounting(`Var.`, digits = 0)
            ) |>
            formattable() |>
            as.datatable(
                options = list(
                    pageLength = 10,
                    searchable = FALSE,
                    dom = "t"
                )
            )
    })

    output$downloadCopom <- downloadHandler(
        filename = function() {
            refdate() |>
                as.Date() |>
                format("VariacaoCOPOM_%Y%m%d.csv")
        },
        content = function(file) {
            df <- curveCopomFormatted()
            write_csv(df, file)
        }
    )

    output$downloadCurvaPre <- downloadHandler(
        filename = function() {
            refdate() |>
                as.Date() |>
                format("CurvaPrefixadaDI1_%Y%m%d.csv")
        },
        content = function(file) {
            df <- curvePre() |>
                as.data.frame() |>
                mutate(
                    terms = as.numeric(terms),
                    rates = as.numeric(rates)
                )
            write_csv(df, file)
        }
    )
}

shinyApp(ui = ui, server = server)
