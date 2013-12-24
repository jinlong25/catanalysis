#Visualizing cluster membership as HTML files
#Cluster membership is derived from cluster analysis
#Input: k - the number of clusters (that is, where to cut the tree)

#Clear the workspace
rm(list=ls())

#libraries for HTML output
library(R2HTML)


#Experiment folder / test
path <- "E:/My Documents/Dropbox/qstr_collaboration/Catscan experiments/Experiments/2202 mturk landscape dmark 1/"

#Number of clusters, that is, where to cut the tree
k = 8

#Participant counter: count the number of participants
#This is taking from CatAnalysis
participant_counter <- function(path){
  
  #Construct the zip folder path and list all zip files
  zip_path <- paste(path, "zip/", sep = "")
  files <- list.files(zip_path)
  
  #Get the total number of participants (zip files)
  np <- length(files)
  
  #Return the total number of participants as an integer
  return(np)
}

#Modified HTMLInsertGraph function without line breaks and smaller default width
#HTMLInsertGraph is part of R2HTML
MyHTMLInsertGraph <- function (GraphFileName = "", Caption = "", GraphBorder = 1, 
          Align = "center", WidthHTML = 200, HeightHTML = NULL, file = get(".HTML.file"), 
          append = TRUE, ...) 
  {
    cat("\n", file = file, append = append, ...)
    cat(paste("<align=", Align, "><img src='", GraphFileName, 
              "' border=", GraphBorder, if (!is.null(WidthHTML)) 
                paste(" width=", WidthHTML, sep = "")
              else "", if (!is.null(HeightHTML)) 
                paste(" height=", HeightHTML, sep = "")
              else "", ">", sep = "", collapse = ""), file = file, 
        append = TRUE, sep = "")
    if (Caption != "") 
      cat(paste("<br><i class=caption>", Caption, "</i>"), 
          file = file, append = TRUE, sep = "")
    invisible(return(TRUE))
}

#Cluster analysis to obtain cluster membership
#This could and should be integrated with other cluster analysis code
d <- read.csv(paste(path, "osm.csv", sep = ""), header = F)
dm <- as.matrix(d[, -1])
dimnames(dm) <- list(d[, 1],d[, 1])

###Participants minus osm generates dissimilarity###
#ave = hclust(method = "average", as.dist(participant_counter(path) - dm))
#comp = hclust(method = "complete", as.dist(participant_counter(path) - dm))
ward = hclust(method = "ward", as.dist(participant_counter(path) - dm))

#obtain cluster membership from Ward's method using cutree and store as data frame
#k is the number of clusters (set at the beginning)
clus.mem <- cutree(ward, k)
clus.mem <- as.data.frame(clus.mem)

#store images of clusters in corresponding html files
for (i in 1:k) {
  #select one cluster and store all the members in ClusterMembers
  clusterMembers <- subset(clus.mem, (clus.mem %in% c(i))) 
  #Store all the row names (image names) of that cluster in iconNames
  iconNames <- rownames(clusterMembers)
  #define output file using the cluster number as a name variable
  output <- paste(k, "_clusWard", i, ".html", sep = "")
  HTMLoutput=file.path(path, output)
  #specify where the icons/images are located at
  iconPath <- paste("icons/", sep = "")
  #write all the images/icons of one cluster into the html file
  #MyHTMLInsertGraph is necessary as there is no parameter to switch off the line break
  for (i in iconNames) {
    MyHTMLInsertGraph(paste(iconPath, i, ".jpg", sep = ""),file=HTMLoutput,caption=i)
  }
}

