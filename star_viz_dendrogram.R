#Run catanalysis.R first
#Then run this script

#Install/load library for export graphics
#install.packages("Cairo")
library("Cairo")

#Deinfe the starting angle (in degree) with due north as 0 degree and increment (in degree)
starting_angle <- 0
increment <- 360/n_icons

#Define the name of the exported file
filename <- "star_viz_dendrogram.png"

#Deinfe max/min cluster solution
min <- 2
max <- 8

##Define 16 distinct colors
colors <- c("firebrick2", "dodgerblue4", "darkgreen", "darkorange2",
            "chocolate4", "black", "deeppink3", "darkmagenta",
            "blueviolet", "darkslategrey", "saddlebrown", "indianred1")

#Define the layout of the output figure (nr * nc >= total number of participants)
nr <- max - min + 1
nc <- 3

#Read in the osm
osm <- read.csv(paste(path,"osm.csv",sep=""),header = F)

#Read in the corrected icon order from Jinlong's Dropbox
icon_order <- read.csv(file = "http://dl.dropboxusercontent.com/u/15662467/birdseye_order_fix.csv", 
                       header = F, stringsAsFactors = F)

#Extract the icon index from the icon names
for(i in 1:nrow(icon_order)){
  icon_order[i, 3] <- substr(icon_order[i, 1], 5, nchar(icon_order[i, 1]))
}

#Rename the icon names in osm
osm[ ,1] <- as.numeric(icon_order[,3])

#Create the osm for cluster analysis with corrected icon names
dm <- as.matrix(osm[, -1])
dimnames(dm) <- list(osm[, 1],osm[, 1])

#Cluster anlaysis
ave <- hclust(method = "average", as.dist(np - dm))
comp <- hclust(method = "complete", as.dist(np - dm))
ward <- hclust(method = "ward", as.dist(np - dm))

#Create a function to convert degrees to redians
#NOTE: Authored by Fabio Marroni
#URL: http://fabiomarroni.wordpress.com/2010/12/23/r-function-to-convert-degrees-to-radians/
degrees.to.radians<-function(degrees=45,minutes=30){
  if(!is.numeric(minutes)) stop("Please enter a numeric value for minutes!\n")
  if(!is.numeric(degrees)) stop("Please enter a numeric value for degrees!\n")
  decimal<-minutes/60
  c.num<-degrees+decimal
  radians<-c.num*pi/180
  return(radians)
}

#Start a new canvas with Cairo(function)
#The width/height of the graphic can be changed via "width" and "height" parameter
Cairo(file = paste(path, filename, sep = ""), 
      type = "png", units = "px", 
      width = nc * 300 + 200, height = nr * 300 + 700, 
      dpi = 1200)

#Partition the canvas using nr and nc defined previously
par(mfrow = c(nr, nc))

#Draw the star plot for all three clustering methods
for(k in min: max){
  #Get the icon membership from dendrogram ave and cut it into k clusters
  dend_ave <- as.data.frame(cutree(ave, k))
  
  #Get the icon index and add the index as the 2nd column to dend_ave
  dend_ave[ ,2] <- as.numeric(gsub("\\D", "", rownames(dend_ave)))
  
  #Name the columns
  colnames(dend_ave) <- c("group", "icon")
  
  #Plot a starplot
  plot(0, 0, type = "n", xaxt = "n", yaxt = "n", 
       xlim = c(-5, 5), ylim =  c(-5, 5), 
       bty="n", xlab = "", ylab = "", 
       main = paste("Average Linkage:", k, "clusters", sep = " "), cex.main = 3)
  
  #Draw the segments(lines) one by one
  for(i in 1:nrow(dend_ave)){
    x <- -5*sin(degrees.to.radians(increment*dend_ave[i, 2] + starting_angle - increment))
    y <- 5*cos(degrees.to.radians(increment*dend_ave[i, 2] + starting_angle - increment))
    segments(0, 0, x, y, col = colors[dend_ave[i, 1]])
  }
  
  #Draw the starplot for complete linkage
  #Please refer to the comments for avearge linkage
  dend_comp <- as.data.frame(cutree(comp, k))
  dend_comp[ ,2] <- as.numeric(gsub("\\D", "", rownames(dend_comp)))
  colnames(dend_comp) <- c("group", "icon")
  
  plot(0, 0, type = "n", xaxt = "n", yaxt = "n", 
       xlim = c(-5, 5), ylim =  c(-5, 5), 
       bty="n", xlab = "", ylab = "", 
       main = paste("Complete Linkage:", k, "clusters", sep = " "), cex.main = 3)
  
  for(i in 1:nrow(dend_comp)){
    x <- -5*sin(degrees.to.radians(increment*dend_ave[i, 2] + starting_angle - increment))
    y <- 5*cos(degrees.to.radians(increment*dend_ave[i, 2] + starting_angle - increment))
    segments(0, 0, x, y, col = colors[dend_comp[i, 1]])
  }
  
  #Draw the starplot for Ward's method
  #Please refer to the comments for avearge linkage
  dend_ward <- as.data.frame(cutree(ward, k))
  dend_ward[ ,2] <- as.numeric(gsub("\\D", "", rownames(dend_ward)))
  colnames(dend_ward) <- c("group", "icon")
  
  plot(0, 0, type = "n", xaxt = "n", yaxt = "n", 
       xlim = c(-5, 5), ylim =  c(-5, 5), 
       bty="n", xlab = "", ylab = "", 
       main = paste("Ward's Method:", k, "clusters", sep = " "), cex.main = 3)
  
  for(i in 1:nrow(dend_ward)){
    x <- -5*sin(degrees.to.radians(increment*dend_ave[i, 2] + starting_angle - increment))
    y <- 5*cos(degrees.to.radians(increment*dend_ave[i, 2] + starting_angle - increment))
    segments(0, 0, x, y, col = colors[dend_ward[i, 1]])
  }
}

#Turn off the drawing device
dev.off()