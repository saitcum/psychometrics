#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
datasets <- c("economics", "faithfuld", "seals")

freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  df <- data.frame(x = c(x1, x2),
                   g = c(rep("x1", length(x1)), rep("x2", length(x2)))
  )
  ggplot(df, aes(x, colour = g)) +
    geom_freqpoly(binwidth = binwidth, size = 1) +
    coord_cartesian(xlim = xlim)
}

t_test <- function(x1, x2) {
  test <- t.test(x1, x2)
  # use sprintf() to format t.test() results compactly
  sprintf(
    "p value: %0.3f\n[%0.2f, %0.2f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}

x1 <- rnorm(100, mean = 0, sd = 0.5)
x2 <- rnorm(200, mean = 0.15, sd = 0.9)



ui<- fluidPage(
  theme = shinytheme("yeti"), 
  titlePanel( h1("Sait CUM Psychometrics Labs", align="center")),
  dashboardPage(
    dashboardHeader(title = "LABS"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("HISTOGRAM LAB", tabName = "k1", icon = icon("tree")),
        menuItem("IRT LAB", tabName = "k2", icon = icon("car")),
        menuItem("DATA LAB", icon = icon("table"),
                 menuSubItem("GRAPH", tabName = "sait"),
                 menuSubItem("TABLE", tabName = "murat"))
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem("k1",
                box(plotOutput("plot1", width=250,height = 250)),
                box(
                  title = "Slider",
                  sliderInput("slider", "Number of Observations:", 1, 100, 50))
        ),
        tabItem("k2",
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("paramb", h3("b parameter"),  
                                min=-3,max=3, step=0.1, value=0),    
                    sliderInput("parama", h3("a parameter"),  
                                min=-2.7,max=2.7, step=0.1, value=1) ),
                  mainPanel(plotOutput("plot")))  
        ),
        
        tabItem("sait",
                fluidPage(
                  fluidRow(
                    column(4,
                           "Distribution 1",
                           numericInput("n1", label = "n", value = 1000, min = 1),
                           numericInput("mean1", label = "μ", value = 0, step = 0.1),
                           numericInput("sd1", label = "σ", value = 0.5, min = 0.1, step = 0.1)
                    ),
                    column(4,
                           "Distribution 2",
                           numericInput("n2", label = "n", value = 1000, min = 1),
                           numericInput("mean2", label = "μ", value = 0, step = 0.1),
                           numericInput("sd2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
                    ),
                    column(4,
                           "Frequency polygon",
                           numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
                           sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
                    )
                  ),
                  fluidRow(
                    column(9, plotOutput("hist")),
                    column(3, verbatimTextOutput("ttest"))))
                
        ),
        tabItem("murat",
                tableOutput("static"),
                dataTableOutput("dynamic")
        )
      ),
    )
  )
)






