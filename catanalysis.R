#Instruction
#1. Create a folder with the name of the experiment;
#2. In the experiment folder, create three subfolder named "zip", "matrices", 
#and "ism" respectively;
#3. Change the PATH & SCENARIO NAME at the beginning of the script;
#4. Run the entire script;
#5. To create dendrograms at different solutions, manually change the number in the last line of the script.
#6. Go find the result in the experiment folder


#Clear the workspace
rm(list=ls())

#Define the path to the experiment folder (with a closing "/" or "\")
#Note that the path delimiter in Windows is "\" while the path delimiter in Mac in "/"
#path <- "E:/My Documents/Dropbox/qstr_collaboration/Catscan experiments/Experiments/2100 mturk landscape test"
#path <- "/Users/jinlong/Dropbox/Catscan experiments/Experiments/2100 mturk landscape test/"
#path <- "/Users/jinlong/Dropbox/ACM_SIGSPATIAL2013/analysis_jinlong/sideview/red/"
#path <- "/Users/jinlong/Dropbox/ACM_SIGSPATIAL2013/analysis_jinlong/sideview/green/"
path <- "/Users/jinlong/Dropbox/ACM_SIGSPATIAL2013/analysis_jinlong/sideview/black/"

#Define the name of the experiment
scenario_name <- "birdseye"

#Define the max number of clusters
max_cluster <- 5

#Uncomment the install.package() functions if you haven't installed these packages
#install.packages("gplots")
library("gplots")
#install.packages("vegan")
library("vegan")

#############DATA PROCESSING FUNCTIONS###############################
#Icon counter: count the number of icons(items) used in the experiment
icon_counter <- function(path){
  #Construct the zip folder path and list all the zip files
  zip_path <- paste(path, "zip/", sep = "")
  files <- list.files(zip_path)
  
  #Get the participant number for the first participant
  first_p_number <- substring(files[1], 1, nchar(files[1]) - 4)
  
  #Unzip the file
  first_p <- unzip(paste(zip_path, files[1], sep = ""))
  
  #Construct the full file name for icons.csv
  icons_csv <- paste("./", first_p_number, "/", first_p_number, "icons.csv", sep = "")
  
  #Read in icons.csv
  icons <- read.csv(icons_csv, header = F)
  
  #Get the number of icons used in the experiment
  n_icons <- nrow(icons)
  
  #Return the number of icons
  return(n_icons)
}

#Icon list getter: get a list of icon names
icon_list_getter <- function(path){
  
  #Construct the zip folder path and list all the zip files 
  zip_path <- paste(path, "zip/", sep = "")
  files <- list.files(zip_path)
  
  #Unzip the zip file from the 1st participant
  first_p <- unzip(paste(zip_path, files[1], sep = ""))
  
  #Get the participant number for the first participant
  first_p_number <- substring(files[1], 1, nchar(files[1]) - 4)
  
  #Construct the full file name for icons.csv
  icons_csv <- paste("./", first_p_number, "/", first_p_number, "icons.csv", sep = "")
  
  #Read in icons.csv
  icons <- read.csv(icons_csv, header = F, stringsAsFactors = F)
  
  #Extract the icon names from the table (excluding the path name and file extensions)
  icon_list <- icons[, 2]
  for(i in 1:length(icon_list)){
    end <- regexpr("\\.[^\\.]*$", icon_list[i])[1]
    icon_list[i] <- substr(icon_list[i], 9, end - 1)
  }
  # Why is this necessary?
  #Jinlong: The old script can only handle icon files with a three-character extension
  #such as gif, jpg, bmp, png. It is okay so far but I rewrote it using regular expression 
  #to auto-locate the extension and then extract the 
  #icon name (without the path or the extension), so it will also work with tiff or jpeg files
  
  #Get and sort the icon names alphabetically in ascending order
  icon_list = sort(icon_list)
  
  #Return the icon list as a vector
  return(icon_list)
}


#Participant counter: count the number of participants
participant_counter <- function(path){
  
  #Construct the zip folder path and list all zip files
  zip_path <- paste(path, "zip/", sep = "")
  files <- list.files(zip_path)
  
  #Get the total number of participants (zip files)
  np <- length(files)
  
  #Return the total number of participants as an integer
  return(np)
}


#OSM and ISM Generator: extract all individual similarity matrices (ISMs) 
#and generate the overall similarity matrix(OSM) by summing up all ISMs
osm_ism_generator <- function(path){
  #Construct the zip folder path and list all zip files
  zip_path <- paste(path, "zip/", sep = "")
  files <- list.files(zip_path)
  
  #Initialize osm with the 1st ISM
  participant1 <- unzip(paste(zip_path, files[1], sep = ""))
  
  #Get the participant number for the first participant
  first_p_number <- substring(files[1], 1, nchar(files[1]) - 4)
  
  #Construct the full file name for ISM file (mtrx file)
  first_ism <- paste("./", first_p_number, "/", first_p_number, ".mtrx", sep = "")
  
  #Read in the ISM from the 1st participant and exclude the non-ism info from the .mtrx file
  first_matrix <- read.delim(first_ism, header = FALSE, sep = " ", stringsAsFactors = F)
  first_matrix <- data.matrix(first_matrix[1:icon_counter(path), ])
  
  #Export the first ISM
  write.table(first_matrix,file = paste(path, "ism/", "participant", 
                                        substr(files[1], 1, nchar(files[1]) - 4),
                                        ".mtrx",sep = ""), sep = " ",
                                        row.names = F, col.names = F)
  
  write.table(first_matrix,file = paste(path, "matrices/", "participant", 
                                        substr(files[1], 1, nchar(files[1]) - 4),
                                        ".mtrx",sep = ""), sep = " ", 
                                        row.names = F, col.names = F)
  
  #Summing up all ISMs for OSM and export each ISM
  osm <- first_matrix
  
  #Process the ISMs of the rest of participants
  for(i in 2:length(files)){
    #Unzip the participant's zip file
    participant_i <- unzip(paste(zip_path, files[i], sep = ""))
    
    #Get the participant number
    participant_number <- substring(files[i], 1, nchar(files[i]) - 4)
    
    #Construct the full file name for .mtrx file
    matrix_i_name <- paste("./", participant_number, "/", participant_number, ".mtrx", sep = "")
    
    #Read in the ISM from a participant and exclude the non-ism info from the .mtrx file
    matrix_i <- read.delim(matrix_i_name, header = F, sep = " ", stringsAsFactors = F)
    matrix_i <- data.matrix(matrix_i[1:icon_counter(path), ])
    
    #Export the ISM as .mtrx for KlipArt and .csv for catanalysis
    write.table(matrix_i, file = paste(path, "ism/", "participant", 
                                       substr(files[i], 1, nchar(files[i]) - 4),
                                       ".mtrx", sep = ""), sep = " ", 
                                       row.names = F, col.names = F)
    
    write.table(matrix_i, file = paste(path, "matrices/", "participant", 
                                       substr(files[i], 1, nchar(files[i]) - 4), 
                                       ".mtrx", sep = ""), sep = " ",
                                       row.names = F, col.names = F)
    
    #Add the ISM to OSM
    osm <- osm + matrix_i
  }
  
  #Export OSM
  #Uncomment the line below if export data for KlipArt
  write.table(osm, file = paste(path, "matrices/", "total.mtrx", sep = ""), 
              sep = " ", row.names = F,  col.names = F)
  
  osm <- cbind(icon_list_getter(path), osm)
  write.table(osm, file = paste(path, "osm.csv", sep = ""), 
              sep = ",", row.names = F,  col.names = F)
}



#################ANALYSIS FUNCTIONS##############################
#Participant info: collect demographic info and basic experiment info (# of groups created
#and time spent in seconds)
participant_info <- function(path){
  
  #Read in the zip file
  zip_path <- paste(path, "zip/", sep = "")
  files <- list.files(zip_path)
  
  #Read in to demographic info for the 1st participant
  participant1 <- unzip(paste(zip_path,files[1],sep = ""))
  
  #Get the participant number for the first participant
  first_p_number <- substring(files[1], 1, nchar(files[1]) - 4)
  
  #Construct the full file name for the participant.csv file for the 1st participant
  first_demo <- paste("./", first_p_number, "/", first_p_number, "participant.csv", sep = "")
  
  #Read in the participant.csv for the 1st participant
  demo1 <- read.delim(first_demo, header = F, sep = ",",stringsAsFactors = F)
  
  #Aggregate eduction background for participant who use comma(s) in their eduction 
  #background (e.g., geography, education, business)
  while(length(demo1) > 13){
    demo1[7] <- paste(demo1[7], demo1[8], sep = ",")
    demo1 <- demo1[-8]
  }
  colnames(demo1) <- 1:13
  
  #Initialize the dataframe for demographic info
  demographic <- demo1
  
  #Add demographic info from the rest of participants to the dataframe "demographic"
  for(i in 2:length(files)){
    participant_i <- unzip(paste(zip_path,files[i],sep=""))
    
    #Get the participant number for the first participant
    participant_number <- substring(files[i], 1, nchar(files[i]) - 4)
    
    #Construct the full file name for participant.csv file
    participant_demo <- paste("./", participant_number, "/", participant_number, 
                              "participant.csv", sep = "")
    
    #Read in the participant.csv
    demo <- read.delim(participant_demo, header = F, sep = ",", stringsAsFactors = F)
    while(length(demo) > 13){
      demo[7] <- paste(demo[7], demo[8], sep = ",")
      demo <- demo[-8]
    }
    colnames(demo) <- 1:13
    demographic <- rbind(demographic, demo)
  }
  
  #Create two vectors to store the # of groups created and time spent (in seconds)
  groups_created <- c()
  time_spent <- c()
  
  for(i in 1:length(files)){
    #Read in the assignment.csv file
    participant_i <- unzip(paste(zip_path, files[i], sep = ""))
    
    #Get the participant number for the first participant
    participant_number <- substring(files[i], 1, nchar(files[i]) - 4)
    
    #Construct the full file name for assignment.csv file
    participant_assignment <- paste("./", participant_number, "/", participant_number, 
                                    "assignment.csv", sep = "")
    
    groups <- read.delim(participant_assignment, header = F, sep = ",", stringsAsFactors = F)
    
    #Get the maxim group index and convert it to the # of groups created
    groups <- groups[nrow(groups), 2] + 1
    
    #Append the # of groups created to the vector "groups_created"
    groups_created <- append(groups_created, groups)
  }
  
  
  for(i in 1:length(files)){
    #Read in the log file
    participant_i <- unzip(paste(zip_path,files[i],sep=""))
    
    #Get the participant number for the first participant
    participant_number <- substring(files[i], 1, nchar(files[i]) - 4)
    
    #Construct the full file name for .log file
    participant_log <- paste("./", participant_number, "/", participant_number, ".log", sep = "")
    
    #Read in the log file
    log <- read.delim(participant_log, header = F, sep=",", stringsAsFactors = F)
    
    #Get the time spent
    time <- log[nrow(log), ]
    time <- substr(time, 33, nchar(time))
    
    #Append the time spent to the vector "time_spent"
    time_spent <- append(time_spent, time)
  }
  
  #Append two vectors (i.e., two columns) - groups_created and time_spent to the demographic dataframe
  demographic <- cbind(demographic, groups_created)
  demographic <- cbind(demographic, time_spent)
  
  #Export the demographic dataframe as a csv file
  write.table(demographic, file = paste(path, "participant.csv", sep = ""),
              sep = ",", row.names = F,  col.names = F)
}
  
#description_getter: extract the linguistic labels (both long and short) from all participants and store in a single csv file
description_getter <- function(path){
  
  #Construct the path for the zip folder and list all the zip files
  zip_path <- paste(path, "zip/", sep = "")
  files <- list.files(zip_path)
  
  #Unzip the zip file from the 1st participant
  participant1 <- unzip(paste(zip_path, files[1], sep =""))
  
  #Get the participant number for the first participant
  participant_number <- substring(files[1], 1, nchar(files[1]) - 4)
  
  #Construct the full file name for the batch.csv file
  batch <- paste("./", participant_number, "/", participant_number, "batch.csv", sep = "")
  
  description1 <- read.csv(batch, header = F, stringsAsFactors = F)
  
  #Aggregate participants' long descriptions when they use comma in the descriptions.
  while(length(description1) > 4){
    description1[, 4] <- paste(description1[, 4], description1[,5], sep = ",")
    description1 <- description1[-5]
  }
  
  #Create dummy column names for the dataframe (will not be included when exported)
  colnames(description1) <- 1:4
  
  #Initialize a dataframe for all descriptions
  description <- description1
  
  #Read in the batch.csv for the rest of participants and extract the descriptions
  for(i in 2:length(files)){
    
    #Read in the zip files
    participant_i <- unzip(paste(zip_path, files[i], sep = ""))
    description_i <- read.csv(sort(participant_i)[5],header = F, stringsAsFactors = F)
    
    #Aggregate participants' long descriptions when they use comma in the descriptions.
    while(length(description_i) > 4){
      description_i[4] <- paste(description_i[, 4], description_i[, 5], sep = ",")
      description_i <- description_i[-5]
    }
    
    #Create dummy column names for the dataframe (will not be included when exported)
    colnames(description_i) <- 1:4
    
    #Combine descriptions from all participant into a dataframe (row-bind)
    description <- rbind(description, description_i)
  }
  
  #Export the description dataframe as a csv file
  write.table(description, file = paste(path, "description.csv", sep = ""), 
              sep = ",", row.names = F,  col.names = F)
}
  


#heatmap: generates a heatmap based on the OSM.
#No dendrograms are generated and the icons are in alphabetical order
#Jinlong: It is intended to be a raw heat map without dendrograms. 
#The cluster heatmap function is right below this function
heatmap <- function(path){
  
  #Read in the osm.csv file and format the row/column names
  d = read.csv(paste(path, "osm.csv", sep = ""),header = F)
  dm = as.matrix(d[, -1])
  dimnames(dm) = list(d[, 1],d[, 1])
  
  #The export of the heatmap is realized as a tiff file. Other options are ....??
  #Jinlong: other options includes jpg, bmp, png, etc. but each has its own function with
  #slightly different arguments and different default values for arguments
  #Drawing the heatmap and export as a tiff file
  tiff(filename = paste(path, "heat_map.tiff", sep = ""),width = 2000, height = 2000, units = "px",
       pointsize = 5,compression = "none", bg = "white", res = 600)
  heatmap.2(as.matrix(participant_counter(path) - dm), Rowv = F, Colv = "Rowv", dendrogram = "none", 
            margin = c(3, 3), cexRow = 0.6, cexCol = 0.6, revC = F, trace = "none", key = F)
  dev.off()
}

#cluster_heatmap: generates a cluster heatmap based on the OSM
cluster_heatmap <- function(path){
  
  #Read in the osm.csv file and format the row/column names
  d = read.csv(paste(path, "osm.csv", sep = ""), header = F)
  dm = as.matrix(d[, -1])
  dimnames(dm) = list(d[, 1],d[, 1])
  
  #Generate the dendrogram using wards method
  cluster = hclust(method = "ward", as.dist(participant_counter(path) - dm))
  dend = as.dendrogram(cluster)
  
  #Drawing the cluster heatmap and export as a tiff file
  tiff(filename = paste(path, "cluster_heatmap.tiff", sep = ""), width = 2000, height = 2000, units = "px",
       pointsize = 5, compression = "none", bg = "white", res = 600)
  heatmap.2(as.matrix(participant_counter(path) - dm), Rowv = dend, Colv = dend, 
            margin = c(3,3), cexRow = 0.6, cexCol = 0.6, dendrogram = "both", 
            revC = T, trace = "none", key = T)
  dev.off()
}


#Cluster analysis
cluster_analysis <- function(path, k, title = ""){
  d <- read.csv(paste(path, "osm.csv", sep = ""), header = F)
  dm <- as.matrix(d[, -1])
  dimnames(dm) <- list(d[, 1],d[, 1])
  #Old code: dm = as.matrix(d)
  #Jinlong: I'm pretty sure the code above won't work for this function
  
  ave = hclust(method = "average", as.dist(participant_counter(path) - dm))
  comp = hclust(method = "complete", as.dist(participant_counter(path) - dm))
  ward = hclust(method = "ward", as.dist(participant_counter(path) - dm))
  
  # load code of A2R function
  # Explain what this function is doing!
  source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
  
  #Create a color scheme from rainbow color scheme
  pre_colors <- rainbow(k)
  colors <- pre_colors[1: k]
  
  pdf(file = paste(path, "dendrograms_", k, "_cluster.pdf", sep=""),
      width = 6, height = 2.5, 
      bg = "white", pointsize = 0.5)
  
  A2Rplot(ave, k = k, boxes = F, col.up = "gray50", col.down = colors, 
          main = paste(title, " Average Linkage ", k, " clusters", sep = ""))
  A2Rplot(comp, k = k, boxes = F, col.up = "gray50", col.down = colors, 
          main = paste(title, " Complete Linkage ", k, " clusters", sep = ""))
  A2Rplot(ward, k = k, boxes = F, col.up = "gray50", col.down = colors, 
          main = paste(title, " Ward's Method ", k, " clusters", sep = ""))
  
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
  pre_colors <- rainbow(k)
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
  boxplot(data[,14],
          horizontal=TRUE, 
          notch = TRUE,  # Notches for CI for median
          col = "slategray3",
          boxwex = 0.5,  # Width of box as proportion of original
          whisklty = 1,  # Whisker line type; 1 = solid line
          staplelty = 0,  # Staple (line at end) type; 0 = none
          outpch = 16,  # Symbols for outliers; 16 = filled circle
          outcol = "slategray3",  # Color for outliers
          main = "Groups Created")
  boxplot(data[,15],
          horizontal=TRUE, 
          notch = TRUE,  # Notches for CI for median
          col = "slategray3",
          boxwex = 0.5,  # Width of box as proportion of original
          whisklty = 1,  # Whisker line type; 1 = solid line
          staplelty = 0,  # Staple (line at end) type; 0 = none
          outpch = 16,  # Symbols for outliers; 16 = filled circle
          outcol = "slategray3",  # Color for outliers
          main = "Grouping Time")
  
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

all_icons <- sort(icon_list_getter(path))

np <- participant_counter(path)

osm_ism_generator(path)

heatmap(path)

cluster_heatmap(path)

participant_info(path)

overview_getter(path)

description_getter(path)

participant_similarity(path)

###Change the number here to create colored-dendrograms at different solutions
for(i in 2: max_cluster){
  cluster_analysis(path, i, scenario_name)
}
