
library(shiny)
library(formattable)
library(lubridate)
library(bslib)
library(readr)
library(DT)
library(flatforwardCOPOM)

source("utils-functions.R")

CURVE <- get_curve_from_web()

.theme <- bs_theme(
    fg = "#fff",
    bg = "#000"
)

ui <- fluidPage(
    theme = .theme,
    titlePanel("Curva de Juros Prefixados DI1"),
    fluidRow(
        div(span("1", style = "color: black;")),
        dateInput("refdate", "Data de referência", CURVE@refdate),
        div(span("1", style = "color: black;")),
        numericInput("num_meetings", "#Datas do COPOM", 4)
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
            downloadButton("downloadCopom",
                "CSV Juros a Termo e Choques",
                icon = icon("download")
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
                checkboxInput("di1c_show_fwd", "Mostrar Curva a Termo"),
                checkboxInput("di1c_show_copom", "Mostrar Datas do COPOM")
            ),
            div(span("1", style = "color: black;")),
            plotOutput("curvePreComplete", height = "400px"),
        ),
    )
)


server <- function(input, output, session) {
    curvePre <- reactive({
        get_curve_from_web(input$refdate)
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
        interpolation(curve) <- interp_flatforwardcopom(.copom_dates, "second")
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
        plot_curve(fixedincome::first(curve, "2 years"), .copom_dates)
    })

    output$curvePreComplete <- renderPlot({
        curve <- curvePre()
        .copom_dates <- if (input$di1c_show_copom) {
            copomDates()
        } else {
            NULL
        }
        plot_curve(curve, .copom_dates, show_forward = input$di1c_show_fwd)
    })

    output$curveCopom <- renderPlot({
        curve <- curveCopom()
        .copom_dates <- copomDates()
        plot_copom_curve(fixedincome::first(curve, "1 years"), .copom_dates)
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