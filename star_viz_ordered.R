

##Set path and layout of the output figure (nr * nc >= total number of participants)
participant_cluster <- 3
nr <- 5
nc <- 6

##Install and load package(s)
#install.packages("Cairo")
library("Cairo")
library("RColorBrewer")


##Read in files
zip_path <- paste(path, "zip/", sep="")
files <- list.files(zip_path)

#Participant Similarity Analysis
isms <- list.files(paste(path, "ism/", sep = ""))
all_isms <- list()

participants <- c()
for (i in 1:length(isms)){
  aism <- read.delim(paste(paste(path, "ism/", sep = ""),isms[i],sep=""),header=FALSE, sep=" ",stringsAsFactors=F)
  #assign(paste("p",i,sep=""),ism)
  all_isms <- c(all_isms, list(aism))
}

dm <- matrix(0, ncol = np, nrow = np)
for (i in 1:np){
  for (j in 1:np){
    dm[i,j] <- sum(abs(all_isms[[i]] - all_isms[[j]]))
  }
}

names <- c()
for (i in 1:length(isms)){
  name <- isms[i]
  names <- append(names, substr(name, 12, nchar(name) - 5))
}

colnames(dm) <- names
rownames(dm) <- names

cluster <- hclust(method = "ward", as.dist(dm))
dend <- as.dendrogram(cluster)
group_membership <- cutree(cluster, participant_cluster)
group_membership <- as.data.frame(group_membership)


#Reordering participants based on participant similarity analysis (psa)
overall_container <- list()

for(p in files){
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
  d$membership <- rep(group_membership[rownames(group_membership) == d[1, 1], ], nrow(d))
  overall_container <- c(overall_container, list(d))
}

ordered_container <- list()
i <- 1
while(i <= np){
  for(alist in overall_container){
    if(alist[1,5] == i){
     ordered_container <- c(ordered_container, list(alist))

    }
  }
  i <- i + 1
}

#Cairo(file = paste("star_viz", p, ".png", sep = ""), type = "png", units = "px", width = 1250, height = 1000)
Cairo(file = paste(path, "star_viz_all.png", sep = ""), type = "png", units = "px", width = nc * 300 + 200, height = nr * 300 + 300, dpi = 1200)
par(mfrow = c(nr, nc))
    
for(d in ordered_container){
  
  ##Define colors
  colors <- c((rgb(102, 194, 165,  maxColorValue=255)), (rgb(252, 141, 98,  maxColorValue=255)), (rgb(166, 216, 84,  maxColorValue=255)), (rgb(228, 26, 28,  maxColorValue=255)), (rgb(231, 138, 165,  maxColorValue=255)), (rgb(141, 160, 203,  maxColorValue=255)), (rgb(55, 126, 184,  maxColorValue=255)), (rgb(152, 78, 163,  maxColorValue=255)), (rgb(77, 175, 74,  maxColorValue=255)), (rgb(255, 127, 0,  maxColorValue=255)), (rgb(255, 255, 51,  maxColorValue=255)), (rgb(166, 86, 40,  maxColorValue=255)), (rgb(229, 196, 148,  maxColorValue=255)), (rgb(50, 50, 50,  maxColorValue=255)), (rgb(110, 110, 110,  maxColorValue=255)), (rgb(170, 170, 170,  maxColorValue=255)), (rgb(255, 255, 153,  maxColorValue=255)))
  colors <- append(colors, brewer.pal(12, "Paired"))
  colors <- append(colors, brewer.pal(11, "Set3"))
  
  
  
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
  plot(0, 0, type = "n", xaxt = "n", yaxt = "n", xlim = c(-5, 5), ylim =  c(-5, 5), bty="n", xlab = "", ylab = "", main = d[1, 1], cex.main = 3.5)
  for(i in 1:nrow(d)){
    x <- -5*sin(degrees.to.radians(5*d[i, 4]))
    y <- 5*cos(degrees.to.radians(5*d[i, 4]))
    segments(0, 0, x, y, col = colors[d[i,2]+1], lwd = 1.5)
    background_colors <- c("red", "black", "yellow", "blue", "pink", "green", "brown", "grey", "violet")
    background_color <- d[1, 5]
    segments(-5, -5, -5, 5, col = background_color, lwd = 2)
    segments(-5, 5, 5, 5, col = background_color, lwd = 2)
    segments(5, 5, 5, -5, col = background_color, lwd = 2)
    segments(5, -5, -5, -5, col = background_color, lwd = 2)
    
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

ave <- hclust(method = "average", as.dist(np - dm))
comp <- hclust(method = "complete", as.dist(np - dm))
ward <- hclust(method = "ward", as.dist(np - dm))



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
    segments(0, 0, x, y, col = colors[dend_ave[i,1]], lwd = 2.5)
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
    segments(0, 0, x, y, col = colors[dend_comp[i,1]], lwd = 2.5)
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
    segments(0, 0, x, y, col = colors[dend_ward[i,1]], lwd = 2.5)
  }
}

dev.off()











