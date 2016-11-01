library(shiny)
library(parallel)

shinyServer = function(input, output) {
  # Generate simulated socks
  sock_sim = reactive({
    # number of socks picked from laundry
    n_picked = 2*input$n_Pairs + input$n_Odds
    prior_mu = input$prior_mu
    prior_beta = input$prior_beta
    prior_alpha = input$prior_alpha
    prior_sd = input$prior_sd
    sample_generate = function(i){
      # Generating a sample of the parameters from the priors
      prior_size_param = -prior_mu^2 / (prior_mu - prior_sd^2)
      n_socks = rnbinom(1, mu = prior_mu, size = prior_size_param)
      prop_pairs = rbeta(1, shape1 = prior_alpha, shape2 = prior_beta)
      n_pairs = round((floor(n_socks / 2)) * prop_pairs)
      n_odds = n_socks - n_pairs * 2
      
      # Simulating picking out n_picked socks
      socks = rep(seq_len(n_pairs + n_odds), rep(c(2, 1), c(n_pairs, n_odds)))
      picked_socks = sample(socks, size =  min(n_picked, n_socks))
      sock_counts = table(picked_socks)
      
      # Returning the parameters and counts of the number of matched 
      # and unique socks among those that were picked out.
      c(n_socks = n_socks,prop_pairs = prop_pairs,n_pairs = n_pairs, n_odds = n_odds,unique = sum(sock_counts == 1), 
        pairs = sum(sock_counts == 2))
    }
    n =input$n_sims
    mcmapply(sample_generate,seq_len(n),mc.cores =8)
  })
  
  post_samples = reactive({
    sock_sim()[, (sock_sim()[5,] == input$n_Odds)
               &
                 (sock_sim()[6,] == input$n_Pairs)
               ]
  })
  
  #post_samples = sock_sim
  output$plot1 = renderPlot({
    x = c("n_socks","prop_pairs","n_pairs","n_odd")
    plot1name = paste("posterior on",x)
    par(mfrow=c(2,2))
    for(i in 1:4){
      hist(post_samples()[i,], freq = FALSE, breaks = 25, xlab=x[i],main= plot1name[i])
      lines(density(post_samples()[i,]))
      if (any(input$checkGroup1=="mean")){
        abline(v=mean(post_samples()[i,]),col="red")
      }
      if (any(input$checkGroup1=="median")){
        abline(v=median(post_samples()[i,]),col="blue")
      }
      if (any(input$checkGroup1=="interval")){
        abline(v=quantile(post_samples()[i,], c(0.025)),col="purple")
        abline(v=quantile(post_samples()[i,], c(0.975)),col="purple")
      }
    }
  })
  observeEvent(input$hideshow1, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("plot1")
  })
  output$plot2 = renderPlot({
    x = c("n_socks","prop_pairs","n_pairs","n_odd")
    plot2name = paste("prior on",x)
    par(mfrow=c(2,2))
    for(i in 1:4){
      hist(sock_sim()[i,], freq = FALSE, breaks = 25, xlab=x[i],main= plot2name[i])
      lines(density(sock_sim()[i,], adjust = 1.8))
      if (any(input$checkGroup2=="mean")){
        abline(v=mean(sock_sim()[i,]),col="red")
      }
      if (any(input$checkGroup2=="median")){
        abline(v=median(sock_sim()[i,]),col="blue")
      }
      if (any(input$checkGroup2=="interval")){
        abline(v=quantile(sock_sim()[i,], c(0.025)),col="purple")
        abline(v=quantile(sock_sim()[i,], c(0.975)),col="purple")
      }
    }
  })
  observeEvent(input$hideshow2, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("plot2")
  })
  
  output$table1 = renderTable({
    aa = c()
    bb = c()
    cc = c()
    dd = c()
    for(i in 1:4){
      x = c("n_socks","prop_pairs","n_pairs","n_odd")
      aa = c(aa,round(mean(post_samples()[i,]),2))
      bb = c(bb,round(median(post_samples()[i,]),2))
      cc = c(cc,round(quantile(post_samples()[i,], 0.025),2))
      dd = c(dd,round(quantile(post_samples()[i,], 0.975),2))
    }
    data.frame(x,"mean"=aa,"median"=bb,"0.025_CI"=cc,"0.975_CI"=dd)
  })
  observeEvent(input$hideshow3, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("table1")
  })
  
  output$table2 = renderTable({
    aa1 = c()
    bb1 = c()
    cc1 = c()
    dd1 = c()
    for(i in 1:4){
      x = c("n_socks","prop_pairs","n_pairs","n_odd")
      aa1 = c(aa1,round(mean(sock_sim()[i,]),2))
      bb1 = c(bb1,round(median(sock_sim()[i,]),2))
      cc1 = c(cc1,round(quantile(sock_sim()[i,], 0.025),2))
      dd1 = c(dd1,round(quantile(sock_sim()[i,], 0.975),2))
    }
    data.frame(x,"mean"=aa1,"median"=bb1,"0.025_CI"=cc1,"0.975_CI"=dd1)
  })
  
  observeEvent(input$hideshow4, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("table2")
  })
  
  output$true = renderText(
    paste("True values: There were 21 pairs and 3 singletons")
  )
  observeEvent(input$show, {
    toggle("true")
  })
}
#options = list(width = 1000)
