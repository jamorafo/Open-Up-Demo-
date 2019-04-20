################################################################################
# #  "Confidentiel / Propri?t? de PolyMtl" 
################################################################################
rm(list=ls(all=TRUE))
directory <- "/Users/andresmorales/Google_Drive_gmail/CIMARLAB/Open Up/Open-Up-Demo/src"
setwd(directory)
################################################################################
# #  Libraries
################################################################################

list.of.packages <- c("openxlsx","latex2exp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) require(new.packages)

library(openxlsx)
library(latex2exp)
# Functions
source("functions.R")
################################################################################
# #  Paths
################################################################################
srcPath      <- "../src/"
inPath       <- "../input/"
outPath      <- "../output/"
docPath      <- "../doc/"

################################################################################
# # Data
################################################################################
# # Trained Open Up
s  <- 0.48
e.s <- 0.10
e.l <-0.10
group <- 2
fileName   <- paste("r/output_group",group,"s",s,"e_s",e.s,".RData", sep = "")
pathFile   <- paste(outPath, fileName, sep = "")
load(pathFile)

# # New Observation
fileName   <- "data.xlsx"
pathFile   <- paste(inPath, fileName, sep = "")
x.new      <- read.xlsx(pathFile,sheet = "new")
x.training <- read.xlsx(pathFile,sheet = "input")
################################################################################
# #  Pre-processing
################################################################################

# Pre-processing
x.new.sorted <- preprocess_data(x.new)
x.train.sorted <- preprocess_data(x.training)

# #  Projections in the Parallel plane.
x.new.parallel <- parallel.plane(stitching,x.new.sorted)
x.train.parallel <- parallel.plane(stitching,x.train.sorted)

# # Good ones
id_good <- which(y==0)
# # Bad ones
id_bad <- which(y==1)

aa=seq(1,(ncol(x.new.sorted)-0.05),0.05)
################################################################################
# #  Plots
################################################################################
i=1
obs <-  x.new.parallel[i,]

par(mar=c(8,0,0,0))
aa=seq(1,(ncol(x.new.sorted)-0.05),0.05)
matplot(NA,type="l",lty=1, xlab = "", ylab = "", axes = FALSE, col="white", lwd=1,ylim=c(-0.2,1.2),xlim=range(aa))
BOZ(x.new.sorted,lower.limit,upper.limit,limits = FALSE)
xlabels <- paste("x_{",1:ncol(x.new.sorted),"}",sep="")
xlabels <- TeX(xlabels)
axis(side = 1,at=1:ncol(x.new.sorted),labels=xlabels,font=1,cex.axis=1.3,padj=3)

limBOZ(x.new.sorted,lower.limit,upper.limit,e.l=e.l)

###########
# Painting empty zones
EmptyZones(x.new,empty.zones.limits)

###########
# Draw a new observation
draw_obs(obs)

####
# # Good ones
set.seed(1)
id_good <- sample(id_good,50)
sapply(id_good,function(i) draw_obs(x.train.parallel[i,],lty = 1,col = "grey70",lwd=1))

# # Bad ones
sapply(id_bad,function(i) draw_obs(x.train.parallel[i,],lty = 1,col = "red",lwd=1))
################################################################################
# #  End!
################################################################################
