library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  
    titlePanel("How many socks in total in Karl Broman's laundry"),
    navbarPage("Team 6",
               tabPanel("Plots",
                        sidebarPanel(
                          numericInput("n_sims", "Number of simulations to run:", value=10000,min=1000,max=1e6),
                          hr(),
                          h4("Data:"),
                          sliderInput("n_Pairs", "Number of paried socks:", value=0,min=0,max=100),
                          sliderInput("n_Odds", "Number of singleton socks:", value=11,min=0,max=100),
                          hr(),
                          h4("Hyper parameter:"),
                          sliderInput("prior_mu", "prior mean", value=30,min=10,max=100),
                          sliderInput("prior_sd", "prior standard deviation", value=15,min=10,max=100),
                          sliderInput("prior_alpha", "shape1 alpha", value=15,min=1,max=100),
                          sliderInput("prior_beta", "shape2 beta", value=2, min=1,max=100)
                        ),
                        mainPanel(
                          useShinyjs(),
                          h4("Results:"),
                          actionButton("hideshow1", "Hide/show posterior distribution"),
                          checkboxGroupInput("checkGroup1", label = h5("posterior summary statistics"), 
                                             choices = list("mean", "median", "95% credible interval"="interval"),inline=TRUE),
                          plotOutput("plot1"),
                          actionButton("hideshow2", "Hide/show prior distribution"),
                          checkboxGroupInput("checkGroup2", label = h5("prior summary statistics"), 
                                             choices = list("mean", "median", "95% credible interval"="interval"),inline=TRUE),
                          plotOutput("plot2")
                        )),
               tabPanel("Summary Statistics",
                        actionButton("hideshow3", "Hide/show posterior summary statistics"),
                        fluidRow(column(3,
                                        tableOutput('table1')
                        )),
                        br(),
                        actionButton("hideshow4", "Hide/show prior summary statistics"),
                        fluidRow(column(3,
                                        tableOutput('table2')
                        )),
                        br(),
                        actionButton("show", "Show True Value"),
                        shinyjs::hidden(textOutput("true")
                        ))))
)



