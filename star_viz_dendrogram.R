#Run catanalysis.R first
#Then run this script

#Install/load library for export graphics
#install.packages("Cairo")
library("Cairo")

#Deinfe max/min cluster solution
min = 4
max = 4

#Define the layout
rows = 1
columns = 3

#Define variables for the exported grpahic
#Change the width and height according to the layout
Cairo(file = paste(path, "star_viz_dendrogram_black.png", sep = ""), type = "png", 
      units = "px", width = 2250, height = 900)

#Partition the drawing device
par(mfrow = c(rows, columns))

#Read in the osm
osm <- read.csv(paste(path,"osm.csv",sep=""),header = F)

#Correct the order of the icons
icon_order <- read.csv(file = "http://dl.dropboxusercontent.com/u/15662467/birdseye_order_fix.csv", 
                       header = F, stringsAsFactors = F)
for(i in 1:nrow(icon_order)){
  icon_order[i, 3] <- substr(icon_order[i, 1], 5, nchar(icon_order[i, 1]))
}

#Rename all icons
osm[ ,1] <- as.numeric(icon_order[,3])

#Create the osm for cluster analysis
dm <- as.matrix(osm[,-1])
dimnames(dm) <- list(osm[,1],osm[,1])

#Cluster anlaysis
ave <- hclust(method = "average", as.dist(np - dm))
comp <- hclust(method = "complete", as.dist(np - dm))
ward <- hclust(method = "ward", as.dist(np - dm))

##Define 16 distinct colors
colors <- c("firebrick2", "dodgerblue4", "darkgreen", "darkorange2",
            "chocolate4", "black", "deeppink3", "darkmagenta",
            "blueviolet", "darkslategrey", "saddlebrown", "indianred1")

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

#Draw the star plot for all three clustering methods
for(k in min: max){
  dend_ave <- as.data.frame(cutree(ave, k))
  dend_ave[ ,2] <- as.numeric(rownames(dend_ave))
  colnames(dend_ave) <- c("group", "icon")
  
  plot(0, 0, type = "n", xaxt = "n", yaxt = "n", 
       xlim = c(-5, 5), ylim =  c(-5, 5), 
       bty="n", xlab = "", ylab = "", 
       main = paste("Average Linkage:", k, "cluster", sep = " "), cex.main = 4)
  for(i in 1:nrow(dend_ave)){
    x <- -5*sin(degrees.to.radians(5*dend_ave[i, 2]))
    y <- 5*cos(degrees.to.radians(5*dend_ave[i, 2]))
    segments(0, 0, x, y, col = colors[dend_ave[i,1]])
  }
  
  dend_comp <- as.data.frame(cutree(comp, k))
  dend_comp[ ,2] <- as.numeric(rownames(dend_comp))
  colnames(dend_comp) <- c("group", "icon")
  
  plot(0, 0, type = "n", xaxt = "n", yaxt = "n", 
       xlim = c(-5, 5), ylim =  c(-5, 5), 
       bty="n", xlab = "", ylab = "", 
       main = paste("Complete Linkage:", k, "cluster", sep = " "), cex.main = 4)
  for(i in 1:nrow(dend_comp)){
    x <- -5*sin(degrees.to.radians(5*dend_comp[i, 2]))
    y <- 5*cos(degrees.to.radians(5*dend_comp[i, 2]))
    segments(0, 0, x, y, col = colors[dend_comp[i,1]])
  }
  
  
  dend_ward <- as.data.frame(cutree(ward, k))
  dend_ward[ ,2] <- as.numeric(rownames(dend_ward))
  colnames(dend_ward) <- c("group", "icon")
  
  plot(0, 0, type = "n", xaxt = "n", yaxt = "n", 
       xlim = c(-5, 5), ylim =  c(-5, 5), 
       bty="n", xlab = "", ylab = "", 
       main = paste("Ward's Method:", k, "cluster", sep = " "), cex.main = 4)
  for(i in 1:nrow(dend_ward)){
    x <- -5*sin(degrees.to.radians(5*dend_ward[i, 2]))
    y <- 5*cos(degrees.to.radians(5*dend_ward[i, 2]))
    segments(0, 0, x, y, col = colors[dend_ward[i,1]])
  }
}

dev.off()