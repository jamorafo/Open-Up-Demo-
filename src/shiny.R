################################################################################
# # Property of Polytechnique Montréal
# # Confidentiel / Propriete de PolyMtl 
################################################################################
options(warn=-1)
rm(list=ls(all=TRUE))
list.of.packages <- c("shiny")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(shiny)
runGitHub("Open-Up-Demo-", "jamorafo", subdir = "Open-up-Demo-server/")
################################################################################
