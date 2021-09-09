# Open-Up-Demo-
Demo for Open Up

Steps to Run Open Up Demo:

  1. Make sure that you have the latest R version installed in your computer. This code was updated with the R version 4.1.1 (2021-08-10). You can install or update 
     R from https://www.r-project.org/.
  2. Once R is installed or updated, you have two options: Option 1 - Run the online version which will call the github repository and Option 2 - Download the whole  Open-Up-Demo folder and run it in your computer. 

Option 1

If you choose option 1 you will just need to file shiny.R located in OpenUpDemo/src/ folder. You will need to accept every library update and voilá! you can enjoy the demo. 

Option 2

If you choose option 2 you will require more steps but you would not need internet connection once everything is installed. You will need to go to Open-up-Demo-server/ui.R, open this file and change the directory for the one in your own computer. For example: "setwd("/Users/andresmorales/Dropbox/CIMAR-LAB/13-Software\ tutorials/OpenUpDemo/src/")" It is located in around the line 22 or 23 of this ui.R file. Then you can run the file server.R located in the same folder. You will need to accept every library update and voilá! you can enjoy the demo. 



In both cases, the software would take some seconds to show up the plots, this is because some computations are required.

