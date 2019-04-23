#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  output$boz <- renderPlot({
    obs <-  x.train.parallel[id_good[input$i],]
    par(mar=c(8,0,0,0))
    aa=seq(1,(ncol(x.new.sorted)-0.05),0.05)
    matplot(NA,type="l",lty=1, xlab = "", ylab = "", axes = FALSE, col="white",
            lwd=1,ylim=c(-0.2,1.2),xlim=range(aa))
    xlabels <- paste("x_{",1:ncol(x.new.sorted),"}",sep="")
    xlabels <- TeX(xlabels)
    axis(side = 1,at=1:ncol(x.new.sorted),labels=xlabels,font=1,cex.axis=1.3,padj=3)
    #draw_obs(obs,lty = 1,col = "grey70",lwd=2)
    sapply(id_good[1:input$i],function(xi) draw_obs(x.train.parallel[xi,],lty = 1,
                                                    col = "black",lwd=1))
    if (input$checkbox) {BOZ(x.new.sorted,lower.limit,upper.limit,limits = FALSE)
    limBOZ(x.new.sorted,lower.limit,upper.limit,e.l=input$e.l)}
    if (input$emptybox) EmptyZones(x.new,empty.zones.limits,e.s=input$e.s)
    if(input$bad>0)  sapply(id_bad[1:input$bad],function(xi) draw_obs(x.train.parallel[xi,],
                                                                       lty = 1,col = "red",lwd=1))
    

    
  })

  output$bozpar <- renderPlot({
    par(mar=c(8,0,0,0))
    aa=seq(1,(ncol(x.new.sorted)-0.05),0.05)
    matplot(NA,type="l",lty=1, xlab = "", ylab = "", axes = FALSE, col="white",
            lwd=1,ylim=c(-0.2,1.2),xlim=range(aa))
    BOZ(x.new.sorted,lower.limit,upper.limit,limits = FALSE)
    xlabels <- paste("x_{",1:ncol(x.new.sorted),"}",sep="")
    #xlabels <- TeX(xlabels)
    #sapply(1:length(xlabels),function(x) TeX(xlabels[x]))
    #TeX(xlabels)
    #axis(side = 1,at=1:ncol(x.new.sorted),labels=xlabels,font=1,cex.axis=1.3,padj=3)
    
    limBOZ(x.new.sorted,lower.limit,upper.limit,e.l=input$e.l)
    EmptyZones(x.new,empty.zones.limits,e.s=input$e.s)
  })

  observeEvent(input$anomaly1,
               output$anomaly <- renderPlot({
                 lengthsector <- ncol(x.new.sorted)/4
                 startsector <- lengthsector*3
                 n <- rbinom(1, 2,.3)+1
                 intruder0 <- sample(startsector:(startsector+lengthsector),n)
                 anomaly_example(intruder0)
               }) 
  )
  observeEvent(input$anomaly1,
               output$anomalyTime <- renderText({paste("Failure time:", Sys.time())}) 
  )
  
  
  observeEvent(input$anomaly2,
               output$anomaly <- renderPlot({
                 lengthsector <- ncol(x.new.sorted)/4
                 startsector <- lengthsector*2
                 n <- rbinom(1, 2,.3)+1
                 intruder0 <- sample(startsector:(startsector+lengthsector),n)
                 anomaly_example(intruder0)
                 }) 
               )
  observeEvent(input$anomaly2,
               output$anomalyTime <- renderText({paste("Failure time:", Sys.time())}) 
  )
  
  observeEvent(input$anomaly3,
               output$anomaly <- renderPlot({
                 lengthsector <- ncol(x.new.sorted)/4
                 startsector <- lengthsector*1
                 n <- rbinom(1, 2,.3)+1
                 intruder0 <- sample(startsector:(startsector+lengthsector),n)
                 anomaly_example(intruder0)
               }) 
  )
  observeEvent(input$anomaly3,
               output$anomalyTime <- renderText({paste("Failure time:", Sys.time())}) 
  )
  
  observeEvent(input$anomaly4,
               output$anomaly <- renderPlot({
                 lengthsector <- ncol(x.new.sorted)/4
                 startsector <- 1
                 n <- rbinom(1, 2,.3)+1
                 intruder0 <- sample(startsector:(startsector+lengthsector),n)
                 anomaly_example(intruder0)
               }) 
  )
  observeEvent(input$anomaly4,
               output$anomalyTime <- renderText({paste("Failure time:", Sys.time())}) 
  )
  
  
  output$acctable <- renderTable(acc_table(output.best,s = s,e.s = input$e.s,e.l = input$e.l),align="c")
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("Time:", Sys.time())
  })
  
  
  # image1 
  output$image1 <- renderImage({
    input$anomaly1
    # Return a list containing information about the image
    return(list(src = '/Users/andresmorales/Google_Drive_gmail/CIMARLAB/Open\ Up/Open-Up-Demo/Open-up-Demo-server/www/normalobs.gif',
         contentType = "image/png",
         width="80%",
         alt = "This is alternate text"))
    
  }, deleteFile = FALSE)
  
})
