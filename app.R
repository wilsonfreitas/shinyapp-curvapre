#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(plotly)
library(bslib)

source("copom-functions.R")

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
            plotOutput("curvePre"),
            width = 12
        )
    ),
    
    br(),
    
    fluidRow(
        column(
            plotOutput("curveCopom"),
            width = 12
        )
    )
)

server <- function(input, output) {
    
    curvePre <- reactive({
        get_curve_from_web()
    })
    
    refdate <- reactive({
        curve <- curvePre()
        curve$refdate[1]
    })
    
    copomDates <- reactive({
        curve <- curvePre()
        rng <- range(curve$maturity_date)
        ix <- copom_dates >= rng[1] & copom_dates <= rng[2]
        add.bizdays(copom_dates[ix][1:4], 1, "Brazil/ANBIMA")
    })
    
    curveCopom <- reactive({
        curve <- curvePre()
        .copom_dates <- copomDates()
        parts <- split_curve_into_copom_dates(curve, .copom_dates)
        copom_curve <- copom_calc(parts, 1, conflicts = "forward")
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
}

shinyApp(ui = ui, server = server)
