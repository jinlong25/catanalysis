### CatAnalysis Extension ###

##################################################################################

# Numerical cluster validation
# Author: Alexander Klippel
# cluster validation is accomplished by comparing cluster membership
# for a certain number of clusters across different clustering methods (ave, comp, ward)
# Parameters: path of experiment and k (maximum number of clusters)
# There could be an issue as cluster membership is a number that may not be the same
# across cluster method!!!
#
numClusVal <- function(path, k){
  #read in matrix and column/row names
  d <- read.csv(paste(path, "osm.csv", sep = ""), header = F)
  dm <- as.matrix(d[, -1])
  dimnames(dm) <- list(d[, 1],d[, 1])
  
  ###Participants minus osm generates dissimilarity###
  ave = hclust(method = "average", as.dist(participant_counter(path) - dm))
  comp = hclust(method = "complete", as.dist(participant_counter(path) - dm))
  ward = hclust(method = "ward", as.dist(participant_counter(path) - dm))
  #cluster validation
  cut.Results = data.frame() #create empty data frame
  for (i in 2:k){
    cut.ave <- as.data.frame(cutree(ave, i))
    cut.comp <- as.data.frame(cutree(comp, i))
    cut.ward <- as.data.frame(cutree(ward, i))
    cut.Results <- as.data.frame(cbind(cut.ave[,1], cut.comp[,1], cut.ward[,1]))
    colnames(cut.Results) <- c(paste("ave", sep=""), paste("comp", sep=""), paste("ward", sep=""))
    cut.Results$Equal3[cut.Results$ave == cut.Results$comp & cut.Results$comp == cut.Results$ward] <- "Equal"
    cut.Results$Equal3[cut.Results$ave != cut.Results$comp | cut.Results$comp != cut.Results$ward] <- "Dif"
    cut.Results$Equal2[cut.Results$ave == cut.Results$comp | cut.Results$comp == cut.Results$ward
                       | cut.Results$ave == cut.Results$ward] <- "Equal2"
    rownames(cut.Results) <- rownames(cut.ave)
    write.csv(cut.Results, file=paste(path, "cluVal", i, ".csv", sep = ""))
  } 
}


###################################################################
# print standard dendrograms
# Author: Alexander Klippel
# input variable: path
# OSM needs to be present
stanDen <- function(path)
  {
  d <- read.csv(paste(path, "osm.csv", sep = ""), header = F)
  dm <- as.matrix(d[, -1])
  dimnames(dm) <- list(d[, 1],d[, 1])
  clu.meth = c("ave", "comp", "ward.D")
  for (i in clu.meth)
    {
    ##ALTERNATIVE 1
      dummy = hclust(method = i, as.dist(participant_counter(path) - dm))
      png(file = paste(path, "dendro", i, ".png", sep=""), width = 1200, height = 1200, pointsize = 12)
      plot(dummy)
    ##ALTERNATIVE 2
#     dummy = as.dendrogram(hclust(method = i, as.dist(participant_counter(path) - dm)))
#     png(file = paste(path, "dendro", i, ".png", sep=""), width = 1400, height = 1200)
#     plot(dummy, type = "triangle", nodePar = list(pch = 10:1, cex = .5*4:1, col = 2:3),
#          edgePar = list(col = 1:2, lty = 2:3), 
#          horiz = TRUE, 
#          #center = FALSE, dLeaf = -2, edge.root = FALSE
#     )
    myTitle = paste(scenario_name, i, sep="//")
    title(main = myTitle)
    dev.off()
    }
  }
stanDen(path)

################################################33
# ploting individual "heatmaps" as black/white images
# Author: Alexander Klippel
# input: path
# output: results for each participant are stored in folder 'indISM'
# required package:
visIndISM <- function(path)
  {
  #read in all ISMs and store as a list
  indISM <- as.list(list.files(paste(path,"ism/",sep="")))
  dir.create(paste(path, "indISM/", sep=""))
  indISM.Path <- paste(path, "indISM/", sep="")
  #iterate through the list and plot each matrix using 'grid.raster'
  #individusal images are stored as png files
  for (i in indISM) 
    {
    indISM.matrix <- read.delim(paste(path, "ism/", i, sep = ""), header = FALSE, sep = " ", stringsAsFactors = F)
    indISM.matrix <- data.matrix(indISM.matrix)
    png(paste(indISM.Path, i, ".png", sep = ""), width = 480, height = 480)
    grid.raster(as.raster(indISM.matrix), interpolate = FALSE)
    dev.off()
    }
  }
visIndISM(path)

#### Not finished
## ploting reordered individual "heatmaps"
#read in all ISMs and store as a list
indISM <- as.list(list.files(paste(path,"ism/",sep="")))
dir.create(paste(path, "indISM-reordered/", sep=""))
indISM.Path <- paste(path, "indISM-reordered/", sep="")
#read in file names with new order
my.names <- read.csv((paste(path, "newNameOrder.csv", sep = "")), header = TRUE)
#iterate through the list and plot each matrix using 'grid.raster'
#individusal images are stored as png files
for (i in indISM) {
  indISM.matrix <- read.delim(paste(path, "ism/", i, sep = ""), header = FALSE, sep = " ", stringsAsFactors = F)
  indISM.matrix <- data.matrix(indISM.matrix)
  colnames(indISM.matrix) <- my.names$new
  rownames(indISM.matrix) <- my.names$new
  new.indISM.matrix <- indISM.matrix[sort(rownames(indISM.matrix)),sort(colnames(indISM.matrix)), drop = F]
  png(paste(indISM.Path, i, ".png", sep = ""), width = 480, height = 480)
  grid.raster(as.raster(new.indISM.matrix), interpolate = FALSE)
  dev.off()
}


#############################################################################################

# Visualizing participant similarities by groups
# Author: Alexander Klippel
# Input: path, number of participants (np), number of clusters (k)
# TODO: np needs to be set manually at the moment!
part.sim.group.vis <- function(path, np, k){
  #List all ISMs
  isms <- list.files(paste(path, "ism/", sep = ""))
  all_isms <- list()
  
  #Read in all ISMs and store them in a list named all_isms
  for (i in 1:length(isms)){
    aism <- read.delim(paste(paste(path, "ism/", sep = ""), isms[i], sep = ""),
                       header = F, sep = " ", stringsAsFactors = F)
    all_isms <- c(all_isms, list(aism))
  }
  
  #Calculate participant similarity matrix (dm) of all pairs of partcipants based on the hamming distance of their ISMs
  dm <- matrix(0, ncol = np, nrow = np)
  for (i in 1: np){
    for (j in 1: np){
      dm[i,j] <- sum(abs(all_isms[[i]] - all_isms[[j]]))
    }
  }
  
  #Extract the participant number of all participants and store them in a vector named names
  names <- c()
  for (i in 1: length(isms)){
    name <- isms[i]
    names <- append(names, substr(name, 12, nchar(name) - 5))
  }
  
  #Assign participants numbers as the row&column names of the participant similarity matrix (dm)
  colnames(dm) <- names
  rownames(dm) <- names
  
  #Perform cluster analysis based on participant similarity matrix using Ward's method and construct a dendrogram
  ward.P <- hclust(method = "ward", as.dist(dm))
  clus.mem <- cutree(ward.P, k)
  clus.mem <- as.data.frame(clus.mem)
  
  #store images of clusters in corresponding html files
  for (i in 1:k) {
    #select one cluster and store all the members in ClusterMembers
    clusterMembers <- subset(clus.mem, (clus.mem %in% c(i))) 
    #Store all the row names (image names) of that cluster in iconNames
    partNames <- rownames(clusterMembers)
    #define output file using the cluster number as a name variable
    output <- paste(k, "_partClus", i, ".html", sep = "")
    HTMLoutput=file.path(path, output)
    #specify where the icons/images are located at
    iconPath <- paste(path, "indISM/", sep = "")
    #write all the images/icons of one cluster into the html file
    #MyHTMLInsertGraph is necessary as there is no parameter to switch off the line break
    for (i in partNames) {
      MyHTMLInsertGraph(paste(iconPath, "participant", i, ".mtrx", ".png", sep = ""),file=HTMLoutput,caption=i)
    }
  }
  
}

##################################################################################################

# Comparing results from 2 experiments / 2 OSMs
# Here: Substracting two OSMs from one another and visualizing the difference
# Author: Alexander Klippel
dif2Osm <- function(path1,path2)
  {
  # load first OSM
  d1 <- read.csv(paste(path1, "osm.csv", sep = ""), header = F)
  dm1 <- as.matrix(d1[, -1])
  
  # load second OSM
  d2 <- read.csv(paste(path2, "osm.csv", sep = ""), header = F)
  dm2 <- as.matrix(d2[, -1])
  
  # substract the two OSMs
  dm.diff <- dm1 - dm2
  dimnames(dm.diff) <- list(d1[, 1],d1[, 1])
  # Output results
  tiff(filename = paste(path1, "heatDif.tiff", sep = ""),width = 2000, height = 2000, units = "px",
       pointsize = 5,compression = "none", bg = "white", res = 600)
  heatmap.2(as.matrix(dm.diff), Rowv = F, Colv = "Rowv", dendrogram = "none", 
            margin = c(3, 3), cexRow = 0.6, cexCol = 0.6, revC = F, trace = "none", key = TRUE)
  dev.off()
  min(dm.diff)
  #max(dm.diff)
  }

path1 <- "E:/My Documents/Dropbox/qstr_collaboration/Catscan experiments/Experiments/1209 mturk directions 3D mugs final 225deg/"
path2 <- path <- "E:/My Documents/Dropbox/qstr_collaboration/Catscan experiments/Experiments/1208 mturk directions 3D mugs final 0deg/"
dif2Osm(path1,path2)

