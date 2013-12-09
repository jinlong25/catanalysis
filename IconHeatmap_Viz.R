#change "folder_location" to your desired path to store data, do not add "/" at the end
#run the script ONLY UNTIL ###"PLACE ZIP FILES INTO "zip" SUBFOLDER..."  
#place all the zip files in the "zip" subfolder created
#run the rest of the script


#install(gplots)
library(gplots)
#install(RColorBrewer)
library(RColorBrewer)
#install.packages("Cairo")
library("Cairo")


####Automated Folder Creation####

folder_location <- "C:/Users/Sparks/Desktop/test"
a <- paste(folder_location, "side_view_iconviz", sep="/")
dir.create(a)
b <- paste(a, "zip", sep="/")
dir.create(b)
c <- paste(a, "ism", sep="/")
dir.create(c)
d <- paste(a, "matrices", sep="/")
dir.create(d)

path <- folder_location

if(substr(path, nchar(path), nchar(path)) != "/"){
  path <- paste(path, "/", sep = "")
}

##PLACE ZIP FILES INTO "zip" SUBFOLDER INSIDE THE "icon_viz" FOLDER


####IconGroup_Viz####

##Read in files
zip_path <- paste(path, "zip/", sep="")
files <- list.files(zip_path)

for(p in files){
  #p <- "21010020.zip"
  participant <- unzip(paste(zip_path, p, sep=""))
  check <- participant[6]
  if(substr(check, nchar(check) - 13, nchar(check)) != "assignment.csv"){
    check <- participant[4]
  }
  
  d <- read.delim(check, header=FALSE, sep=",",stringsAsFactors=F)
  
  d <- d[order(d[,3]),] 
  #icon_order <- read.csv(file = "C:/Users/Sparks/Desktop/test/order_fix.csv", 
                         #header = F, stringsAsFactors = F)
  #icon_order <- icon_order[1:98,]
  #icon_order <- d
  #for(i in 1:nrow(icon_order)){
    #icon_order[i, 3] <- substr(icon_order[i, 1], 5, nchar(icon_order[i, 1]))
  #}
  #d <- cbind(d, as.numeric(icon_order[,3]))
  #colnames(d) <- c("participant", "group", "fake_order", "order")
  
  
  batch <- unzip(paste(zip_path, p, sep=""))
  check <- batch[6]
  if(substr(check, nchar(check) - 13, nchar(check)) != "batch.csv"){
    check <- batch[5]
  }
  
  batch_file <- read.delim(check, header=FALSE, sep=",",stringsAsFactors=F)
  linguistic_info <- batch_file[,2:3]
  
  ##outputs the unique integers in column 2 of the csv
  groups <- unique(d[,2]) 
  
  ##creates an empty list called "all_groups"
  all_groups <- list()
  
  ##fills in the created "all_groups" list with the groups the participant created 
  for(group in groups){
    all_groups[[group+1]] <- d[d[,2] == group,] 
  }
  
  ##creates an empty data frame
  df <- data.frame() 

  
  ##matches all integers in specific group to other integers within that group
  for(k in 1:length(all_groups)){ 
    for(i in 1:length(all_groups[[k]][,3])){
      for(j in 1:length(all_groups[[k]][,3])){
        combo <- c(all_groups[[k]][i, 3], all_groups[[k]][j, 3], k-1)
        df <- rbind(df, combo)
        # print(combo)
      }
    }
  }
  
  ####MATRIX####
  
  ##creates empty matrix
  m <- matrix(data = NA, nrow = nrow(d), ncol = nrow(d))
  
  ##fills in matrix from combo values of data frame
  for(i in 1:nrow(df)){  
    x <- df[i, 1]+1
    y <- df[i, 2]+1
    m[x, y] <- df[i, 3]
  }  
  
  ngroups <- length(groups)
  myBreaks <- -1:ngroups 
  
  myColors <- c((rgb(102, 194, 165,  maxColorValue=255)), (rgb(252, 141, 98,  maxColorValue=255)), (rgb(166, 216, 84,  maxColorValue=255)), (rgb(228, 26, 28,  maxColorValue=255)), (rgb(231, 138, 165,  maxColorValue=255)), (rgb(141, 160, 203,  maxColorValue=255)), (rgb(55, 126, 184,  maxColorValue=255)), (rgb(152, 78, 163,  maxColorValue=255)), (rgb(77, 175, 74,  maxColorValue=255)), (rgb(255, 127, 0,  maxColorValue=255)), (rgb(255, 255, 51,  maxColorValue=255)), (rgb(166, 86, 40,  maxColorValue=255)), (rgb(229, 196, 148,  maxColorValue=255)), (rgb(50, 50, 50,  maxColorValue=255)), (rgb(110, 110, 110,  maxColorValue=255)), (rgb(170, 170, 170,  maxColorValue=255)), (rgb(255, 255, 153,  maxColorValue=255)))
  myColors <- append(myColors, brewer.pal(12, "Paired"))
  myColors <- append(myColors, brewer.pal(11, "Set3"))
  myColors <- myColors[(1:(ngroups+1))]
  
  p <- substr(p, 1, nchar(p) - 4 )
  
  jpeg(filename = paste(path, p, "heatmap.jpeg", sep = ""), 
       quality=400, width = 500, height = 500, pointsize = 20)
  heatmap.2(m, Rowv = NA, Colv = NA, trace = "none", col = myColors, 
            breaks = myBreaks, key = "FALSE", na.color = "black")
  legend("topleft",legend=(linguistic_info[,2]), fill=myColors)
  
  dev.off()
  
}