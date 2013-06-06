##Housing clearing
rm(list=ls())

##Set path and layout of the output figure (nr * nc >= total number of participants)
path <- "D:/Desktop/planes_sideview/"
nr <- 4
nc <- 5

##Install and load package(s)
#install.packages("Cairo")
library("Cairo")


##Read in files
zip_path <- paste(path, "zip/", sep="")
files <- list.files(zip_path)

#Cairo(file = paste("star_viz", p, ".png", sep = ""), type = "png", units = "px", width = 1250, height = 1000)
Cairo(file = paste(path, "star_viz_all.png", sep = ""), type = "png", units = "px", width = nc * 300 + 200, height = nr * 300 + 300, dpi = 1200)
par(mfrow = c(nr, nc))

for(p in files){
  p <- files[9]
  participant <- unzip(paste(zip_path, p, sep=""))
  check <- participant[6]
  if(substr(check, nchar(check) - 13, nchar(check)) != "assignment.csv"){
    check <- participant[4]
  }
  d <- read.delim(check, header=FALSE, sep=",",stringsAsFactors=F)
  d <- d[order(d[,3]),] 
  icon_order <- read.csv(file = "https://dl.dropboxusercontent.com/u/15662467/birdseye_order_fix.csv", 
                         header = F, stringsAsFactors = F)
  for(i in 1:nrow(icon_order)){
    icon_order[i, 3] <- substr(icon_order[i, 1], 5, nchar(icon_order[i, 1]))
  }
  d <- cbind(d, as.numeric(icon_order[,3]))
  colnames(d) <- c("participant", "group", "fake_order", "order")
  
  ##Define 16 colors
  colors <- c("firebrick2", "dodgerblue4", "darkgreen", "darkorange2",
              "chocolate4", "black", "deeppink3", "darkmagenta",
              "blueviolet", "darkslategrey", "saddlebrown", "indianred1")
  
  
  ##Convert degrees to radians
  degrees.to.radians<-function(degrees=45,minutes=30)
  {
    if(!is.numeric(minutes)) stop("Please enter a numeric value for minutes!\n")
    if(!is.numeric(degrees)) stop("Please enter a numeric value for degrees!\n")
    decimal<-minutes/60
    c.num<-degrees+decimal
    radians<-c.num*pi/180
    return(radians)
  }
  
  
  #Have fun with plotting :-)
  plot(0, 0, type = "n", xaxt = "n", yaxt = "n", xlim = c(-5, 5), ylim =  c(-5, 5), bty="n", xlab = "", ylab = "", main = substr(p, 1, nchar(p)-4), cex.main = 3.5)
  for(i in 1:nrow(d)){
    x <- -5*sin(degrees.to.radians(5*d[i, 4]))
    y <- 5*cos(degrees.to.radians(5*d[i, 4]))
    segments(0, 0, x, y, col = colors[d[i,2]+1])
  }
}

dev.off()


Cairo(file = paste(path, "star_viz_dendrogram.png", sep = ""), type = "png", units = "px", width = 2*1250, height = 2*3000)
par(mfrow = c(7, 3))

##Cluster analysis
osm <- read.csv(paste(path,"osm.csv",sep=""),header=FALSE)

icon_order <- read.csv(file = "https://dl.dropboxusercontent.com/u/15662467/birdseye_order_fix.csv", 
                       header = F, stringsAsFactors = F)
for(i in 1:nrow(icon_order)){
  icon_order[i, 3] <- substr(icon_order[i, 1], 5, nchar(icon_order[i, 1]))
}

osm[ ,1] <- as.numeric(icon_order[,3])

dm <- as.matrix(osm[,-1])
dimnames(dm) <- list(osm[,1],osm[,1])

ave <- hclust(method = "average", as.dist(20 - dm))
comp <- hclust(method = "complete", as.dist(20 - dm))
ward <- hclust(method = "ward", as.dist(20 - dm))



##Define 16 colors
colors <- c("firebrick2", "dodgerblue4", "darkgreen", "darkorange2",
            "chocolate4", "black", "deeppink3", "darkmagenta",
            "blueviolet", "darkslategrey", "saddlebrown", "indianred1")


degrees.to.radians<-function(degrees=45,minutes=30)
{
  if(!is.numeric(minutes)) stop("Please enter a numeric value for minutes!\n")
  if(!is.numeric(degrees)) stop("Please enter a numeric value for degrees!\n")
  decimal<-minutes/60
  c.num<-degrees+decimal
  radians<-c.num*pi/180
  return(radians)
}

for(k in 2:8){
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












