##Jinlong's test editing


##Instruction##
##1. Create a folder with the name of the experiment;
##2. In the experiment folder, create three subfolder named "zip", "matrices", 
##and "ism" respectively;
##3. Change the PATH & SCENARIO NAME at the beginning of the script;
##4. Run the entire script;
##5. To create dendrograms at different solutions, manually change the number in the last line of the script.
##6. Go find the result in the experiment folder


rm(list=ls())
path <- "D:/Desktop/sideview"
scenario_name <- "Planes Sideview"
##Define the max number of clusters
max_cluster <- 8


if(substr(path, nchar(path), nchar(path)) != "/"){
  path <- paste(path, "/", sep = "")
}



#install.packages("gplots")
library("gplots")
#install.packages("vegan")
library("vegan")
#install.packages("Rcmdr")
#library("Rcmdr")

############DATA PROCESSING FUNCTIONS###############################

##########Icon Counter##########
icon_counter <- function(path){
  zip_path <- paste(path, "zip/", sep="")
  files <- list.files(zip_path)
  first_p <- unzip(paste(zip_path,files[1],sep=""))
  icons <- read.csv(first_p[7],header=F)
  n_icons <- nrow(icons)
  return(n_icons)
}

##########Icon List Getter######
icon_list_getter <- function(path){
  zip_path <- paste(path, "zip/", sep="")
  files <- list.files(zip_path)
  first_p <- unzip(paste(zip_path,files[1],sep=""))
  icons <- read.csv(first_p[7],header=F,stringsAsFactors=F)
  icon_list <- icons[,2]
  for(i in 1:length(icon_list)){
######################FIX LATER!!###################################
    icon_list[i] <- substr(icon_list[i],13,nchar(icon_list[i])-4)
  }
  icon_list = sort(icon_list)
  return(icon_list)
}

######Participant Counter#######
participant_counter <- function(path){
  zip_path <- paste(path, "zip/", sep="")
  files <- list.files(zip_path)
  np <- length(files)
  return(np)
}


#####OSM and ISM Generator#####

#Set the path for experiment folder
osm_ism_generator <- function(path){
  #Creat path for zips
  zip_path <- paste(path, "zip/", sep="")
  files <- list.files(zip_path)
  
  #Initialize osm with the first individual similarity matrice
  participant1 <- unzip(paste(zip_path,files[1],sep=""))
  matrix1 <- read.delim(participant1[2],header=FALSE, sep=" ",stringsAsFactors=F)
  matrix1 <- data.matrix(matrix1[1:icon_counter(path),])
  
  #Export the first matrix
  write.table(matrix1,file=paste(path, "ism/", "participant", substr(files[1], 1, nchar(files[1])-4),  ".mtrx",sep=""), sep=" ", row.names=F, col.names = F)
  write.table(matrix1,file=paste(path, "matrices/", "participant", substr(files[1], 1, nchar(files[1])-4),  ".mtrx",sep=""), sep=" ", row.names=F, col.names = F)
  #Add all ism to osm and export each individual ism
  osm <- matrix1
  for(i in 2:length(files)){
    participant_i <- unzip(paste(zip_path,files[i],sep=""))
    matrix_i <- read.delim(participant_i[2],header=FALSE, sep=" ",stringsAsFactors=F)
    matrix_i <- data.matrix(matrix_i[1:icon_counter(path),])
    write.table(matrix_i,file=paste(path, "ism/", "participant", substr(files[i], 1, nchar(files[1])-4),  ".mtrx",sep=""), sep=" ", row.names=F, col.names = F)
    write.table(matrix_i,file=paste(path, "matrices/", "participant", substr(files[i], 1, nchar(files[i])-4),  ".mtrx",sep=""), sep=" ", row.names=F, col.names = F)
    osm <- osm + matrix_i
  }
  
  #Uncomment this line if export data for KlipArt
  write.table(osm, file=paste(path, "matrices/", "total.mtrx", sep=""), sep=" ", row.names=F,  col.names = F)
  ##Icon naming convention matters here!!!
  osm <- cbind(sort(as.numeric(icon_list_getter(path))), osm)
  write.table(osm, file=paste(path, "osm.csv", sep=""), sep=",", row.names=F,  col.names = F)
}



#################ANALYSIS FUNCTIONS##############################
#Participant info
participant_info <- function(path){
  zip_path <- paste(path, "zip/", sep="")
  files <- list.files(zip_path)
  
  participant1 <- unzip(paste(zip_path,files[1],sep=""))
  demo1 <- read.delim(participant1[4],header=FALSE, sep=",",stringsAsFactors=F)
  while(length(demo1) > 13){
    demo1[7] <- paste(demo1[7], demo1[8], sep = ",")
    demo1 <- demo1[-8]
  }
  colnames(demo1) <- 1:13

  demographic <- demo1
  
  for(i in 2:length(files)){
    participant_i <- unzip(paste(zip_path,files[i],sep=""))
    
    #Temp solution, fix later#
  check <-  participant_i[4] 
  if(substr(check, nchar(check) - 14, nchar(check)) != "participant.csv"){
    check <- participant_i[7]
  }
    demo <- read.delim(check,header=FALSE, sep=",",stringsAsFactors=F)
    while(length(demo) > 13){
      demo[7] <- paste(demo[7], demo[8], sep = ",")
      demo <- demo[-8]
    }
    colnames(demo) <- 1:13
    demographic <- rbind(demographic, demo)
  }
  
  groups_created <- c()
  time_spent <- c()
  
  for(i in 1:length(files)){
    participant_i <- unzip(paste(zip_path,files[i],sep=""))
    #Temp solution
    check <-  participant_i[6] 
    if(substr(check, nchar(check) - 13, nchar(check)) != "assignment.csv"){
      check <- participant_i[4]
    }
    groups <- read.delim(check, header=FALSE, sep=",",stringsAsFactors=F)
    groups <- groups[nrow(groups),2]+1
    groups_created <- append(groups_created, groups)
  }
  
  
  for(i in 1:length(files)){
    participant_i <- unzip(paste(zip_path,files[i],sep=""))
    #Temp solution
    check <-  participant_i[6] 
    if(substr(check, nchar(check) - 15, nchar(check)) != "assignment.csv"){
      check <- participant_i[4]
    }
    log <- read.delim(participant_i[3],header=FALSE, sep=",",stringsAsFactors=F)
    time <- log[nrow(log),]
    time <- substr(time, 33,nchar(time))
    time_spent <- append(time_spent, time)
  }
  
  demographic <- cbind(demographic, groups_created)
  demographic <- cbind(demographic, time_spent)
  
  write.table(demographic, file=paste(path, "participant.csv", sep=""), sep=",", row.names=F,  col.names = F)
}
  

description_getter <- function(path){
  zip_path <- paste(path, "zip/", sep="")
  files <- list.files(zip_path)
  
  participant1 <- unzip(paste(zip_path,files[1],sep=""))
  description1 <- read.csv(participant1[5],header=FALSE, stringsAsFactors=F)
  while(length(description1) > 4){
    description1[,4] <- paste(description1[,4], description1[,5], sep = ",")
    description1 <- description1[-5]
  }
  colnames(description1) <- 1:4
  
  description <- description1
  
  for(i in 2:length(files)){
    participant_i <- unzip(paste(zip_path,files[i],sep=""))
    description_i <- read.csv(participant_i[5],header=FALSE, stringsAsFactors=F)
    while(length(description_i) > 4){
      description_i[4] <- paste(description_i[,4], description_i[,5], sep = ",")
      description_i <- description_i[-5]
    }
    colnames(description_i) <- 1:4
    description <- rbind(description, description_i)
  }
  write.table(description, file=paste(path, "description.csv", sep=""), sep=",", row.names=F,  col.names = F)
}
  


#Heatmap
heat_map <- function(path){
  d = read.csv(paste(path,"osm.csv",sep=""),header=FALSE)
  dm = as.matrix(d[,-1])
  dimnames(dm) = list(d[,1],d[,1])
  
  tiff(filename = paste(path, "heat_map.tiff", sep=""),width = 2000,height=2000,units="px",pointsize=5,compression="none",bg="white",res=600)
  heatmap.2(as.matrix(participant_counter(path)-dm),Rowv=FALSE, Colv="Rowv",dendrogram="none",margin = c(3,3),cexRow =0.6,cexCol=0.6,revC=F,trace="none",key=F)
  dev.off()
}

#Heatmap with dendrogram
heat_map_w_dend <- function(path){
  d = read.csv(paste(path,"osm.csv",sep=""),header=FALSE)
  dm = as.matrix(d[,-1])
  dimnames(dm) = list(d[,1],d[,1])
  
  cluster = hclust(method = "ward", as.dist(participant_counter(path)-dm))
  dend = as.dendrogram(cluster)
  
  tiff(filename = paste(path, "heat_map_w_dend.tiff", sep=""),width = 2000,height=2000,units="px",pointsize=5,compression="none",bg="white",res=600)
  heatmap.2(as.matrix(participant_counter(path)-dm),Rowv=dend,Colv=dend,margin = c(3,3),cexRow =0.6,cexCol=0.6,dendrogram="both",revC=TRUE,trace="none",key=TRUE)
  dev.off()
}


#Cluster analysis
cluster_analysis <- function(path, k, title=""){
  d = read.csv(paste(path,"osm.csv",sep=""),header=FALSE)
  dm = as.matrix(d[,-1])
  dimnames(dm) = list(d[,1],d[,1])
  
  ave = hclust(method = "average", as.dist(participant_counter(path)-dm))
  comp = hclust(method = "complete", as.dist(participant_counter(path)-dm))
  ward = hclust(method = "ward", as.dist(participant_counter(path)-dm))
  
  # load code of A2R function
  source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
  
  pre_colors <- c("firebrick2","dodgerblue4","indianred1","darkgreen","darkorange2",
"darkmagenta","deeppink3","chocolate4", "blueviolet", "black", "darkslategrey", "saddlebrown")
  colors <- pre_colors[1:k]
  
  
  pdf(file = paste(path, "dendrograms_", k, "_cluster.pdf", sep=""),width = 6, height =2.5, bg = "white", pointsize = 1)
  A2Rplot(ave, k=k, boxes = FALSE,col.up = "gray50", col.down = colors, main = paste(title, " Average Linkage ", k, " clusters", sep=""))
  A2Rplot(comp, k=k, boxes = FALSE,col.up = "gray50", col.down = colors, main = paste(title, " Complete Linkage ", k, " clusters", sep=""))
  A2Rplot(ward, k=k, boxes = FALSE,col.up = "gray50", col.down = colors, main = paste(title, " Ward's Method ", k, " clusters", sep=""))
  dev.off()
  
}

if(F){
#Cluster validation
cluster_validation <- function(path, k, title=""){

  
  ism <- list.files(paste(path,"ism/",sep=""))
  r <- sample(1:100, size=participant_counter(path), replace=TRUE)
  ism_list <- data.frame(ism,r)
  ism_list <- ism_list[order(r),]
  
  if(participant_counter(path)%%2 == 0){
    split <- participant_counter(path)/2
  }else{
    split <-(participant_counter(path)-1)/2
  }
  
  #Split the participants
  group1=ism_list[1:split,1]
  group2=ism_list[(split+1):participant_counter(path),1]
  
  #read in group1 matrix
  matrix1=read.delim(paste(path,"ism/",group1[1],sep=""),header=F, sep=" ",stringsAsFactors=F)
  osm1=data.matrix(matrix1)
  
  for (i in 2:length(group1)){
    matrix_i<-read.delim(paste(path,"ism/",group1[i],sep=""),header=F, sep=" ",stringsAsFactors=F)
    matrix_i<-data.matrix(matrix_i)
    osm1<-osm1 + matrix_i
  }
  
  #read in group2 matrix
  matrix2=read.delim(paste(path,"ism/",group2[1],sep=""),header=F, sep=" ",stringsAsFactors=F)
  osm2=data.matrix(matrix2)
  
  for (i in 2:length(group2)){
    matrix_i<-read.delim(paste(path,"ism/",group2[i],sep=""),header=F, sep=" ",stringsAsFactors=F)
    matrix_i<-data.matrix(matrix_i)
    osm2<-osm2 + matrix_i
  }
  
  d1=data.frame(icon_list_getter(path),osm1)
  d1m = as.matrix(d1[,-1])
  dimnames(d1m) = list(d1[,1],d1[,1])
  
  d2=data.frame(icon_list_getter(path),osm2)
  d2m = as.matrix(d2[,-1])
  dimnames(d2m) = list(d2[,1],d2[,1])
  
  ave1 = hclust(method = "average", as.dist(participant_counter(path)-d1m))
  ave2 = hclust(method = "average", as.dist(participant_counter(path)-d2m))
  
  comp1 = hclust(method = "complete", as.dist(participant_counter(path)-d1m))
  comp2 = hclust(method = "complete", as.dist(participant_counter(path)-d2m))
  
  ward1 = hclust(method = "ward", as.dist(participant_counter(path)-d1m))
  ward2 = hclust(method = "ward", as.dist(participant_counter(path)-d2m))
  
  #load code of A2R function
  source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
  
  #Define colors
  pre_colors <- c("firebrick2","dodgerblue4","indianred1","darkgreen","darkorange2",
                  "darkmagenta","deeppink3","chocolate4", "blueviolet", "black", "darkslategrey", "saddlebrown")
  colors <- pre_colors[1:k]
  
  #colored dendrograms
  pdf(file= paste(path, "cluster_validation.pdf", sep=""),onefile=T,width=12, height=4)
  A2Rplot(ave1 , k=k, boxes = FALSE,col.up = "gray50", col.down = colors,main=paste(title," Group 1 Average Linkage",sep=""))
  A2Rplot(ave2 , k=k, boxes = FALSE,col.up = "gray50", col.down = colors,main=paste(title," Group 2 Average Linkage",sep=""))
  
  A2Rplot(comp1 , k=k, boxes = FALSE,col.up = "gray50", col.down = colors,main=paste(title," Group 1 Complete Linkage",sep=""))
  A2Rplot(comp2 , k=k, boxes = FALSE,col.up = "gray50", col.down = colors,main=paste(title," Group 2 Complete Linkage",sep=""))
  
  A2Rplot(ward1, k=k, boxes = FALSE,col.up = "gray50", col.down = colors,main=paste(title," Group 1 Ward's Method",sep=""))
  A2Rplot(ward2, k=k, boxes = FALSE,col.up = "gray50", col.down = colors,main=paste(title," Group 2 Ward's Method",sep=""))
  
  dev.off()
}
}


#MDS
if(F){
###Define a list using Excel..
mds_list <- read.csv(paste(path,"mds_list.csv", sep = ""), header=F)

mds <- function(path, list){
  d = read.csv(paste(path,"osm.csv",sep=""),header=FALSE)
  dm = as.matrix(d[,-1])
  dimnames(dm) = list(d[,1],d[,1])
  
  dm_dist <- dist(dm,method="euclidean")
  mds <- cmdscale(dm_dist)
  col <- c("firebrick2","dodgerblue4","darkgreen","chocolate4","darkorange2",
           "darkmagenta","deeppink3","indianred1", "blueviolet", "black", "darkslategrey", "saddlebrown")
  tiff(filename = paste(path, "mds.tiff", sep=""),width = 3, height =3, units = "in", pointsize=5, compression="none", bg = "white",res=600)
  plot(min(mds[,1],mds[,2]):max(mds[,1],mds[,2]), min(mds[,1],mds[,2]):max(mds[,1],mds[,2]), type = "n", xlab="",ylab="", main="Multidimensional Scaling")
  legend
  for(i in 1:nrow(mds)){
    points(mds[i,1],mds[i,2],pch=list[i,2], col=col[list[i,2]],type="p",cex=1.5)
  }
  ####FIX LATER!!!###########
  legend("topright", col = col[1:3], legend = c("Group 1", "Group 2", "Group 3"), pch=c(1,2,3), title="Legend")
  dev.off()  
}
}

##Overview
#set the scenario here and file name

overview_getter <- function(path){
  output <- paste(scenario_name, "_overview.pdf", sep = "")
  data <- read.csv(paste(path,"participant.csv", sep = ""), header=F, stringsAsFactors = F)
  
  male=0
  female=0
  for (i in 1:nrow(data)){
    if (data[i,3]== "male"){
      male=male+1
    } else {
      female=female+1
    }
  }
  
  aveage <- round(mean(data[,2]),2)
  max <- max(data[,2])
  min <- min(data[,2])
  
  
  pdf(file= paste(path, output, sep = "") ,onefile=T,width=10, height=25)
  
  layout(matrix(c(1,1,2,2,3,3,4,5), 4, 2, byrow = TRUE))
  
  plot.new()
  title(paste("Total participants: ", np ,";",sep=""),line=-18, cex=20)
  title(paste("Male: ",male, ", Female: ", female, sep=""),line=-20, cex=20)
  title(paste("Average age: ", aveage, " (max: ", max, ", min: ", min, ")", sep=""),line=-22,cex=20)
  boxplot(data[,14],horizontal=T, main = "Groups Created")
  boxplot(data[,15],horizontal=T, main = "Grouping Time")
  
  groupscount=data.frame(table(data[,14]))
  
  a=groupscount$Var1
  b=c()
  for (i in 1:length(a)){
    b[i]=toString(a[i])
  }
  
  groupmean=mean(data[,14])
  groupsd=round(sd(data[,14]),2)
  
  barplot(groupscount$Freq, names.arg = b, 
          main = paste("Groups Created (mean = ", groupmean,", ","sd = ", groupsd, ")",sep=""),
          xlab="Number of groups created", ylab="Frequency")
  
  hist(data[,15], col="grey",main = paste("Grouping Time", " (mean = ", round(mean(data[,15]),2), "s", "," ," sd = ", round(sd(data[,15]),2),  "s", ")",sep=""),xlab="Time spent on grouping in second")
  title(scenario_name,outer=T,line=-2,cex.main = 2,col.main="blue")
  
  dev.off()
}

##Participant similarity analysis
participant_similarity <- function(path){
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

  tiff(filename = paste(path, "participant_simiarlity.tiff", sep=""),width = 2000,height=2000,units="px",pointsize=5,compression="none",bg="white",res=600)
  plot(dend)
  dev.off()
}

#exe
n_icons <- icon_counter(path)

all_icons <- sort(as.numeric(icon_list_getter(path)))

np <- participant_counter(path)

osm_ism_generator(path)

heat_map(path)

heat_map_w_dend(path)

participant_info(path)

overview_getter(path)

description_getter(path)

participant_similarity(path)

###Change the number here to create colored-dendrograms at different solutions
for(i in 2: max_cluster){
  cluster_analysis(path, i, scenario_name)
}
