
library(shiny)
library(formattable)
library(lubridate)
library(bslib)
library(readr)
library(DT)

source("copom-functions.R")

CURVE <- get_curve_from_web()
REFDATE <- CURVE$refdate[1]

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
            dateInput("refdate", "Data de referência", REFDATE),
            downloadButton("downloadCurvaPre",
                           "Download .csv com os dados da curva Prefixada",
                           icon = icon("download")),
            div(span("1", style="color: black;")),
            downloadButton("downloadCopom",
                           "Download .csv com os dados das curvas a Termo",
                           icon = icon("download"))
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
        curve$refdate[1]
    })
    
    copomDates <- reactive({
        curve <- curvePre()
        rng <- range(curve$maturity_date)
        copom_dates <- add.bizdays(copom_dates, 1, "Brazil/ANBIMA")
        ix <- copom_dates >= rng[1] & copom_dates <= rng[2]
        copom_dates[ix][1:4]
    })
    
    curveCopom <- reactive({
        curve <- curvePre()
        .copom_dates <- copomDates()
        parts <- split_curve_into_copom_dates(curve, .copom_dates)
        copom_calc(parts, 1, conflicts = "forward")
    })
    
    curveCopomFormatted <- reactive({
        curveCopom() |>
            select(maturity_date, forward_tax, move) |>
            rename(`Data do COPOM` = maturity_date,
                   `Taxa a termo` = forward_tax,
                   `Var.` = move)
    })

    output$curvePre <- renderPlot({
        curve <- curvePre()
        .copom_dates <- copomDates()
        plot_curve(curve |> filter(business_days < 504), .copom_dates)
    })
    
    output$curveCopom <- renderPlot({
        curve <- curvePre()
        copom_curve <- curveCopom()
        .copom_dates <- copomDates()
        plot_copom_curve(curve, copom_curve, .copom_dates)
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
                ))
    })
    
    output$downloadCopom <- downloadHandler(
        filename = function() {
            refdate() |> as.Date() |> format("VariacaoCOPOM_%Y%m%d.csv")
        },
        content = function(file) {
            df <- curveCopomFormatted()
            write_csv(df, file)
        }
    )

    output$downloadCurvaPre <- downloadHandler(
        filename = function() {
            refdate() |> as.Date() |> format("CurvaPrefixadaDI1_%Y%m%d.csv")
        },
        content = function(file) {
            df <- curvePre()
            write_csv(df, file)
        }
    )
}

shinyApp(ui = ui, server = server)
