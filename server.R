# server.R

"quantmod" %in% rownames(installed.packages())
if("quantmod" %in% rownames(installed.packages()) == FALSE) {install.packages("quantmod")}
"randtests" %in% rownames(installed.packages())
if("randtests" %in% rownames(installed.packages()) == FALSE) {install.packages("randtests")}
"fractal" %in% rownames(installed.packages())
if("fractal" %in% rownames(installed.packages()) == FALSE) {install.packages("fractal")}
"tseries" %in% rownames(installed.packages())
if("tseries" %in% rownames(installed.packages()) == FALSE) {install.packages("tseries")}
"car" %in% rownames(installed.packages())
if("car" %in% rownames(installed.packages()) == FALSE) {install.packages("car")}
"shinyBS" %in% rownames(installed.packages())
if("shinyBS" %in% rownames(installed.packages()) == FALSE) {install.packages("shinyBS")}

library("quantmod")
library("randtests") 
library("fractal")
library("tseries")
library("car")
library("shinyBS")


shinyServer(function(input, output,session) {
  getSymbols.warning4.0=FALSE
  options("getSymbols.warning4.0"=FALSE)
  
  dataInput <- reactive({
    data<-tryCatch({
      #if there is a bsAlert, close it
      closeAlert(session, "alert")
      #try to get the symbols
      getSymbols(input$symb, src = "yahoo", 
                 from = input$dates[1],
                 to = input$dates[2],
                 auto.assign = FALSE)},
      #if there is an error
      error=function(cond) {
        #create the bsAlert
        createAlert(session, inputId = "alert_anchor",
                    alertId="alert",
                    message = "Please enter a valid symbol and data range",
                    type = "warning",
                    append="false",
                    dismiss = FALSE
        )
        #return an empty string
        return("")
      })
    data
  })
  lg.ret<-reactive({
    dailyReturn(dataInput(),type="log")
  
  })
  
  output$DateRange <- renderText({
    validate(
      need(input$dates[2] > input$dates[1], "end date is earlier than start date"
      )
    )
    
    paste("Your date range is", 
          difftime(input$dates[2], input$dates[1], units="days"),
          "days")
  })
  
  lg.ret.vec<-reactive({
    as.vector(dailyReturn(dataInput(),type="log"))

  })
  
  output$plot <- renderPlot({             
    chartSeries(dataInput(), name="Price changes",theme = chartTheme("white",bg.col='#E0F2F7'), 
      type = "line", TA = NULL,)
  })
  
  output$ret.graph<-renderPlot({
    
    chartSeries(lg.ret(),name="Log returns",type="line",theme=chartTheme("white",bg.col='#E0F2F7'))
    
  })
  
  output$qq.plot<-renderPlot({
  
    qqnorm(lg.ret(),main="QQ plot of log returns")
    qqline(lg.ret())    
  })
  output$histogram<-renderPlot({
   
      hist(lg.ret(),main="Histogram of log returns",xlab="Log returns")
  })
  
  output$qq.line<-renderPlot({
   
    index<-1:length(lg.ret.vec())
    model<-lm((lg.ret.vec())~ index) 
    plot(model,which=2,main="QQ plot of the residuals",xlab="Theoretical Quantiles")             
    qqline(rstandard(model),col="black")
  })
  
  output$logs <-renderTable({
    lg.day.ret.vec<-lg.ret.vec()
    
    FUN <- function(x) {
      x <- as.integer(x)
      div <- seq_len(abs(x))
      factors <- div[x %% div == 0L]
      factors <- list(neg = -factors, pos = factors)
      return(factors)
    }
    
    a<-length(lg.day.ret.vec)
    while(a>5){
      a<-FUN(a)$pos
      a<-ifelse('5'  %in% a,5,ifelse('4'  %in% a,4,ifelse('3'  %in% a,3,ifelse('2'  %in% a,2,length(lg.day.ret.vec)-1))))
    }
    
    
    for (i in 2:a){
      sa.1<-lg.day.ret.vec[1:(length(lg.day.ret.vec)/a)]
      nam <- paste("sa.", i, sep = "")
      assign(nam, lg.day.ret.vec[((i-1)*(length(lg.day.ret.vec)/a)):(i*(length(lg.day.ret.vec)/a))])
    }
    
    next.sa<-c()
    all.vec<-c()
    for (j in 1:a){
      g.nam<-get(paste("sa.", j, sep = ""))
      all.vec<-c(g.nam,all.vec)
      initial.sa<-c(rep(j,length(g.nam)))
      next.sa<-c(initial.sa,next.sa)
    }
    factors<-as.factor(next.sa)
    
    index<-1:length(lg.ret.vec())
    model<-lm((lg.ret.vec())~ index)
    
    lev.p<-sprintf("%.5f",leveneTest(all.vec,factors)$`Pr(>F)`[1])
    lev.s<-leveneTest(all.vec,factors)$`F value`[1]
    bart.p<-sprintf("%.5f",bartlett.test(all.vec,factors)$p.value)
    bart.s<-bartlett.test(all.vec,factors)$statistic
    turn.p<-sprintf("%.5f",turning.point.test(lg.day.ret.vec)$p.value)
    turn.s<-turning.point.test(lg.day.ret.vec)$statistic
    shap.p<-ifelse(length(lg.day.ret.vec) < 3 | length(lg.day.ret.vec) > 5000, NA, sprintf("%.5f",shapiro.test(lg.day.ret.vec)$p.value))
    shap.s<-ifelse(length(lg.day.ret.vec) < 3 | length(lg.day.ret.vec) > 5000, NA, sprintf("%.5f",shapiro.test(lg.day.ret.vec)$statistic))
    jar.s<-jarque.bera.test(lg.day.ret.vec)$statistic
    jar.p<-sprintf("%.5f",jarque.bera.test(lg.day.ret.vec)$`p.value`[1])
    linm.p<-sprintf("%.5f",summary(model)$coefficients[2, 4])
    linm.s<-summary(model)$fstatistic[1]
    
    P.Value<-c(lev.p,bart.p,turn.p,shap.p,jar.p,linm.p) 
    Test.Statistic<-c(lev.s,bart.s,turn.s,shap.s,jar.s,linm.s) 
    test.table<-data.frame(P.Value,Test.Statistic)
    rownames(test.table, do.NULL = TRUE, prefix = "row")
    rownames(test.table) <- c("Levene Test","Bartlett Test","Turning Point Test","Shapiro-Wilk Test","Jarque Bera Test","Linear Model Constant Drift Test")
    
    test.table
  })
})
