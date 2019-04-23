################################################################################
# #  "Confidentiel / Propriete de PolyMtl" 
################################################################################
################################################################################
#rm(list=ls(all=TRUE))
################################################################################
# #  Libraries
################################################################################
list.of.packages <- c("latex2exp","repmis","RCurl","httr","devtools",
                      "roxygen2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(latex2exp)
library(repmis)
library(RCurl)
library(httr)
library(devtools)
library(roxygen2)
# Functions
source_url("https://raw.githubusercontent.com/jamorafo/Open-Up-Demo-/master/src/functions.R")
################################################################################
# # Data
################################################################################
# # Trained Open Up
# You need to add "?raw=True" to your link address in order to read it.
train_info <- "https://github.com/jamorafo/Open-Up-Demo-/blob/master/output/r/output_group2s0.48e_s0.1.RData?raw=True"
source_data(train_info,rdata = T)

load("/Users/andresmorales/Google_Drive_gmail/CIMARLAB/Open Up/Open-Up-Demo/output/r/output.best.RData")


link.data <- getURL("https://raw.githubusercontent.com/jamorafo/Open-Up-Demo-/master/input/data.csv")
x.training <- read.csv(text = link.data)

# # New Observation

link.new<- getURL("https://raw.githubusercontent.com/jamorafo/Open-Up-Demo-/master/input/new.csv")
x.new <- read.csv(text = link.new)

################################################################################
# #  Pre-processing
################################################################################

# Pre-processing
x.new.sorted <- preprocess_data(x.new)
x.train.sorted <- preprocess_data(x.training)

# #  Projections in the Parallel plane.
x.new.parallel <- parallel.plane(stitching,x.new.sorted)
x.train.parallel <- parallel.plane(stitching,x.train.sorted)

# # Location of the good observations
id_good <- which(y==0)
# # Location of the bad observations
id_bad <- which(y==1)

aa=seq(1,(ncol(x.new.sorted)-0.05),0.05)

################################################################################
# #  End!
################################################################################
