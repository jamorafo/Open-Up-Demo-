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
  
  time.debounced <-  debounce(reactive(input$time),25)

  output$frameSlider <- renderUI( sliderInput("frame", label= h3("Animate"), 
                                              min=1, max = 5000, 
                                              value=1, step=1, sep=NULL,
                                              animate = animationOptions(interval = input$frameInterval, loop = 
                                                                           TRUE))
  )
  
  # This is to count the number of clicks on the anomaly button.
   observe({
    if(is.null(input$rnd)){
      runjs("
            var click = 0;
            Shiny.onInputChange('rnd', click)
            var outlier = document.getElementById('outlier')
            outlier.onclick = function() {click += 1; Shiny.onInputChange('rnd', click)};
            ")      
    }
    })
  
  output$clickCount <- renderText({
    paste('Anomaly generator Button Clicks =', input$rnd)
  })     
  

  output$openup <- renderText({"Open Up"})
  output$openup2 <- renderText({"Open Up"})
  
  # Here we show the animation of the BOZ without calling any gif.
  output$bozz <- output$bozz2 <- renderPlot({
    obs <-  x.train.parallel[id_good[time.debounced()],]
    par(mar=c(8,0,0,0))
    aa=seq(1,(ncol(x.new.sorted)-0.05),0.05)
    matplot(NA,type="l",lty=1, xlab = "", ylab = "", axes = FALSE, col="white",
            lwd=1,ylim=c(-0.2,1.2),xlim=range(aa))
    # xlabels <- paste("x_{",1:ncol(x.new.sorted),"}",sep="")
    #  xlabels <- TeX(xlabels)
    xlabels <- 1:ncol(x.new.sorted)#paste("x",1:ncol(x.new.sorted),sep="")
    if(r$outlier==0) axis(side = 1,at=1:ncol(x.new.sorted),labels=xlabels,font=1,cex.axis=1,padj=3)
    #draw_obs(obs,lty = 1,col = "grey70",lwd=2)
    draw_obs(obs,lty = 1, col = "black",lwd=1)
    if (input$checkbox) {BOZ(x.new.sorted,lower.limit,upper.limit,limits = FALSE)
      limBOZ(x.new.sorted,lower.limit,upper.limit,e.l=input$e.l)}
    if (input$emptybox) EmptyZones(x.new,empty.zones.limits,e.s=input$e.s)
    sector4 <- sector3 <- sector2 <- sector1 <- FALSE
    if(r$outlier==1){
     set.seed(input$rnd+100)
     bad <- sample(id_bad,1,replace = TRUE)
     ik.outlier <- bad
     bad.obs <- x.train.parallel[ik.outlier,]
     cross.bounds.matrix = ((lower.limit-input$e.l)>bad.obs)|((upper.limit+input$e.l)<bad.obs)
     where_cross = aa[which(cross.bounds.matrix)]
     floor_cross = floor(where_cross)
     ceiling_cross = ceiling(where_cross)
     kpi_id = union(floor_cross,ceiling_cross)
     draw_obs(bad.obs,lty = 2,col = "red",lwd=2)
     axis(side = 1,at=kpi_id,labels=xlabels[kpi_id],font=1,cex.axis=1,padj=3,col="red", lwd=2)
     sector4 <- any(1:10 %in% kpi_id)
     sector3 <- any(11:20 %in% kpi_id)
     sector2 <- any(21:30 %in% kpi_id)
     sector1 <- any(31:40 %in% kpi_id)
     sectors <- c(sector1,sector2,sector3,sector4)
     sectors.failt <- which(sectors)
     sectors.ok <- which(!sectors)
     
     if (length(sectors.ok)>0){
       idbutton.ok <- paste("button",sectors.ok,sep="")
       sapply(idbutton.ok, function(x) removeClass(x,"btn-primary"))
     }
     
     if (length(sectors.failt)>0){
       idbutton.f <- paste("button",sectors.failt,sep="")
       sapply(idbutton.f, function(x) addClass(x,"btn-primary"))
     }
     
   }
  })
  
  r <- reactiveValues(outlier = 0)
  
  observeEvent(input$reset, {
    r$outlier = 0
  })
  
  observeEvent(input$outlier, {
    r$outlier = 1
  })

    ####
  #output$res_bttn2 <- renderPrint(input$bttn2)
  outr <- reactiveValues(my_color = "green")
  observeEvent(input$reset, {
    removeClass("button4", "btn-primary")
    removeClass("button3", "btn-primary")
    removeClass("button2", "btn-primary")
    removeClass("button1", "btn-primary")
    outr$my_color <- "red"
  })
  
  observeEvent(input$outlier,
               output$anomalyTime2 <- renderText({paste("Failure time:", Sys.time())})
   )
  
  observeEvent(input$reset, {
    output$anomalyTime2 <- renderText(" ")
  })
  
  
  observeEvent(input$anomaly4, {
    addClass("anomaly4","btn-primary")
    removeClass("anomaly3", "btn-primary")
    removeClass("anomaly2", "btn-primary")
    removeClass("anomaly1", "btn-primary")
  })
  
  observeEvent(input$anomaly3, {
    addClass("anomaly3","btn-primary")
    removeClass("anomaly4", "btn-primary")
    removeClass("anomaly2", "btn-primary")
    removeClass("anomaly1", "btn-primary")
  })
  
  observeEvent(input$anomaly2, {
    addClass("anomaly2","btn-primary")
    removeClass("anomaly3", "btn-primary")
    removeClass("anomaly4", "btn-primary")
    removeClass("anomaly1", "btn-primary")
  })
  
  observeEvent(input$anomaly1, {
    addClass("anomaly1","btn-primary")
    removeClass("anomaly3", "btn-primary")
    removeClass("anomaly2", "btn-primary")
    removeClass("anomaly4", "btn-primary")
  })
  

  #################################
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
                 anomaly_example(intruder0,e.l=input$e.l,e.s=input$e.s,emptybox=input$emptybox)
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
                 anomaly_example(intruder0,e.l=input$e.l,e.s=input$e.s,emptybox=input$emptybox)
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
                 anomaly_example(intruder0,e.l=input$e.l,e.s=input$e.s,emptybox=input$emptybox)
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
                 anomaly_example(intruder0,e.l=input$e.l,e.s=input$e.s,emptybox=input$emptybox)
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
  
<<<<<<< HEAD
  output$currentTime2 <- renderText({
    invalidateLater(1000, session)
    paste("Time:", Sys.time())
  }) 

=======
  
  # image1 
  output$image1 <- renderImage({
    input$anomaly1
    # Return a list containing information about the image
    return(list(src = '/Users/andresmorales/Google_Drive_gmail/CIMARLAB/Open\ Up/Open-Up-Demo/Open-up-Demo-server/www/normalobs.gif',
         contentType = "image/png",
         width="80%",
         alt = "This is alternate text"))
    
  }, deleteFile = FALSE)
  
>>>>>>> a2a0241733a42dcfdaf5cf1e281fa07679e8fd67
})
