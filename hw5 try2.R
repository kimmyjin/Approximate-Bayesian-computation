library(shiny)

shinyApp(
  ui = fluidPage(
    titlePanel("How many socks in total in Karl Broman's laundry"),
    sidebarPanel(
      numericInput("n_sims", "Number of simulations to run:", value=10000,min=1000,max=1e6),
      hr(),
      h4("Data:"),
      sliderInput("n_Pairs", "Number of paried socks:", value=0,min=0,max=100),
      sliderInput("n_Odds", "Number of singleton socks:", value=11,min=1,max=100),
      hr(),
      h4("Hyper parameter:"),
      sliderInput("prior_mu", "prior mean", value=30,min=10,max=100),
      sliderInput("prior_sd", "prior standard deviation", value=15,min=10,max=100),
      sliderInput("prior_alpha", "shape1 alpha", value=15,min=1,max=100),
      sliderInput("prior_beta", "shape2 beta", value=2, min=1,max=100)
    ),
    mainPanel(
      h4("Results:"),
      plotOutput("plot1"),
      br(),
      tableOutput("messages")
    )
  ),
  
  server = function(input, output){
    # number of socks picked from laundry
    # n_picked = reactive({
    #   n_picked = 2*input$n_Paris + input$n_Odds
    # })
    # Generate simulated socks
    sock_sim = reactive({
      # number of socks picked from laundry
      n_picked = 2*input$n_Paris + input$n_Odds
      replicate(input$n_sims,{
        # Generating a sample of the parameters from the priors
        prior_size = -input$prior_mu^2 / (input$prior_mu - input$prior_sd^2)
        n_socks = rnbinom(1, mu = input$prior_mu, size = prior_size)
        prop_pairs = rbeta(1, shape1 = input$prior_alpha, shape2 = input$prior_beta)
        n_pairs = round((floor(n_socks / 2)) * prop_pairs)
        n_odds = n_socks - n_pairs * 2
        
        # Simulating picking out n_picked socks
        socks = rep(seq_len(n_pairs + n_odds), rep(c(2, 1), c(n_pairs, n_odds)))
        picked_socks = sample(socks, size =  min(n_picked, n_socks))
        sock_counts = table(picked_socks)
        
        # Returning the parameters and counts of the number of matched 
        # and unique socks among those that were picked out.
        c(unique = sum(sock_counts == 1), pairs = sum(sock_counts == 2),
          n_socks = n_socks, n_pairs = n_pairs, n_odds = n_odds, prop_pairs = prop_pairs)
        
      })
    })
    
    post_samples = reactive({
      sock_sim()#[, (sock_sim()[1,] == input$n_Odds)
                #    &
                #   (sock_sim()[2,] == input$n_Pairs)
                # ]
    })

    
   #post_samples = sock_sim
    output$plot1 = renderPlot({
      hist(post_samples()[3,], probability =  TRUE, breaks = 20)
    })
    output$messages = renderTable(
      t(post_samples())
    )
  },
  options = list(width = 1000)
)
