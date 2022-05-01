
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

    titlePanel("Curva de Juros Prefixados DI1"),
    theme = .theme,
    br(),
    fluidRow(
        column(
            width = 3,
            dateInput("refdate", "Data de referência", CURVE@refdate),
            downloadButton("downloadCurvaPre",
                "Download .csv com os dados da curva Prefixada",
                icon = icon("download")
            ),
            div(span("1", style = "color: black;")),
            downloadButton("downloadCopom",
                "Download .csv com os dados das curvas a Termo",
                icon = icon("download")
            )
        ),
        column(
            plotOutput("curvePre", height = "320px"),
            width = 9
        )
    ),
    br(),
    fluidRow(
        column(
            plotOutput("curveCopom", height = "320px"),
            width = 8
        ),
        column(
            width = 4,
            dataTableOutput("tableCopom"),
        )
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
        get_copom_dates(refdate(), 4)
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