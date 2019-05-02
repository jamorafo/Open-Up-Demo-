

png(file="openup%02d.png", width = 1424, height = 400)
id_good_sample <- sample(id_good,20)
for (i in id_good_sample){
  obs_gif <-  x.train.parallel[i,]
  OpenUp(obs_gif)
}
dev.off()

# Converting .png files in one .gif image using ImageMagick
system("convert -delay 80 *.png normalobs.gif")
# Remove .png files from working directory
#file.remove(list.files(pattern=".png"))
file.copy("normalobs.gif", "../Open-up-Demo-server/www/",overwrite = TRUE)
#file.copy("*.png", "../Open-up-Demo-server/www/")


