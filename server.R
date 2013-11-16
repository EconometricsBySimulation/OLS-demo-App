library(shiny)
input <- list(rseed=1)

# A function for generating the data.
data.maker <- function(nobs, 
                       expr, 
                       seed=1, 
                       sdx=1, 
                       sdu=3,
                       yscalar=FALSE) {
  set.seed(seed)
  x <- rnorm(500)*sdx+2
  u <- rnorm(500)*sdu
  
  x <- x[1:nobs]
  u <- u[1:nobs]
  
  x2 <- x^2
  x3 <- x^3
  x4 <- x^4
  x5 <- x^5
  expx <- exp(x)
  # This evuates the expression chosen by the user.
  y <- eval(parse(text=expr))
  if (yscalar) y <- -y
  data.frame(x=x,x2=x2,x3=x3,x4=x4,x5=x5
             ,expx=expx,u=u,y=y)
}

shinyServer(function(input, output) {
  
  # Hit counter
  output$counter <- 
    renderText({
      if (!file.exists("counter.Rdata")) counter <- 0
      if (file.exists("counter.Rdata")) load(file="counter.Rdata")
      counter <- counter + 1
      
      save(counter, file="counter.Rdata")     
      paste0("Hits: ", counter)
    })
  
  mydata <- reactive({
    data.maker(input$nobs, 
       input$dgp, 
       input$rseed,
       sdx=input$sdx,
       sdu=input$sdu,
       yscalar=input$yscalar)
  })

  output$datacaption <- 
    renderText(paste0("Data Generating Proccess: ",input$dgp))
  
  output$values  <- renderTable(mydata())
  
  lmResults <- reactive({
    regress.exp <- input$regression
    if (!input$constant) regress.exp <- paste(input$regression, "- 1")
    lm(regress.exp, data=mydata())
  })
  
  output$lmStats <- renderTable({
    results <- summary(lmResults())
    data.frame(R2=results$r.squared,
               adj.R2=results$adj.r.squared,
               DOF.model=results$df[1],
               DOF.available=results$df[2],
               DOF.total=sum(results$df[1:2]),
               f.value=results$fstatistic[1],
               f.denom=results$fstatistic[2],
               f.numer=results$fstatistic[3],
               p=1-pf(results$fstatistic[1],
                         results$fstatistic[2],
                         results$fstatistic[3]))
  })
  
  # Show coefficients
  output$lmResults <- renderTable(summary(lmResults()))
  
  # Show plot of points, regression line, residuals
  output$scatter <- renderPlot({
    data1 <- mydata()
    x <- data1$x
    y <- data1$y
    
    xcon <- seq(min(x)-.1, max(x)+.1, .025)
    x2 <- xcon^2
    x3 <- xcon^3
    x4 <- xcon^4
    x5 <- xcon^5
    expx <- exp(xcon)
    
    predictor <- data.frame(x=xcon,x2=x2,x3=x3,x4=x4,x5=x5
               ,expx=expx)
    
    yhat <- predict(lmResults())
    
    yline <- predict(lmResults(), predictor)
  
    plot(c(min(x),max(x)) 
         ,c(min(y,yline),max(y,yline)), 
         type="n",
         xlab="x",
         ylab="y",
         main=paste0("Regression Model: ", input$regression))
    
    if (input$predict) lines(xcon, yline, lwd=15, col=grey(.9))
    if (input$resid) for (j in 1:length(x)) 
      lines(rep(x[j],2), c(yhat[j],y[j]), col="red")
    if (input$showdata) points(x,y)
    if (input$predict) lines(xcon, yline, lwd=2, col="blue")
  })
  
})
