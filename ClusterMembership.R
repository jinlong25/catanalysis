
rm(list=ls())




##IMPORTANT, Cluster Membership requires "participant_counter" function to be defined


######################################################################


###Participant counter function: count the number of participants###

participant_counter <- function(path){
  
  #Construct the zip folder path and list all zip files
  zip_path <- paste(path, "zip/", sep = "")
  files <- list.files(zip_path)
  
  #Get the total number of participants (zip files)
  np <- length(files)
  
  #Return the total number of participants as an integer
  return(np)
}



######################################################################










##Cluster Membership

#install(R2HTML)
library(R2HTML)

#Below requires user entries
##############################################################

#Assign the location of the icons 
#this is also where the output html file will be saved to
iconlocation<-'C:/Users/Sparks/Dropbox/Catscan experiments/Experiments/2100 mturk landscape test/archive/icons'


##Define path to where the osm is located
path <- "C:/Users/Sparks/Desktop/Landscape/"
##Read in the osm
osm <- read.csv(paste(path,"osm.csv",sep=""),header=FALSE)


#Define the max number of clusters, BUT DEFINE BETWEEN 2-8
max_cluster <- 4

##############################################################

#analysis begins, run from here

##Names of the icons must be removed as the first column and replaced as 
##a column/row name. There is a difference between first column and column name
dm <- as.matrix(osm[,-1])
dimnames(dm) <- list(osm[,1],osm[,1])

###Participants minus osm generates dissimilarity###
ave = hclust(method = "average", as.dist(participant_counter(path) - dm))
comp = hclust(method = "complete", as.dist(participant_counter(path) - dm))
ward = hclust(method = "ward", as.dist(participant_counter(path) - dm))


##Starts main loop
for(k in 2:max_cluster){
  
  
  #Average Linkage Begins
  dend_ave <- as.data.frame(cutree(ave, k))  
  dend_ave[ ,2] <- rownames(dend_ave)
  colnames(dend_ave) <- c("group", "icon")

  ###Creates a list of icon names in cluster 1###
  #Creates empty list
  aveCluster1 <- list()
  #Defines a counting variable to cycle through rows
  l<-1
  #loops through the 'group' column in cluster analysis
  for (i in dend_ave[,1]){
    #only gathers entries of '1'
    if (i==1){
      #adds the icon name to the cluster list
      aveCluster1=append(aveCluster1, dend_ave[l,2])
    }
    #redefines counter variable to go to the next row in dend_ave
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 2###
  aveCluster2 <- list()
  l<-1
  for (i in dend_ave[,1]){
    if (i==2){
      aveCluster2=append(aveCluster2, dend_ave[l,2])
    }
    l=l+1
  }

  ###Creates a list of icon names in cluster 3###
  aveCluster3 <- list()
  l<-1
  for (i in dend_ave[,1]){
    if (i==3){
      aveCluster3=append(aveCluster3, dend_ave[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 4###
  aveCluster4 <- list()
  l<-1
  for (i in dend_ave[,1]){
    if (i==4){
      aveCluster4=append(aveCluster4, dend_ave[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 5###
  aveCluster5 <- list()
  l<-1
  for (i in dend_ave[,1]){
    if (i==5){
      aveCluster5=append(aveCluster5, dend_ave[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 6###
  aveCluster6 <- list()
  l<-1
  for (i in dend_ave[,1]){
    if (i==6){
      aveCluster6=append(aveCluster6, dend_ave[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 7###
  aveCluster7 <- list()
  l<-1
  for (i in dend_ave[,1]){
    if (i==7){
      aveCluster7=append(aveCluster7, dend_ave[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 8###
  aveCluster8 <- list()
  l<-1
  for (i in dend_ave[,1]){
    if (i==8){
      aveCluster8=append(aveCluster8, dend_ave[l,2])
    }
    l=l+1
  }
  
  #Creates a master list that holds nested lists of the created cluster groups
  masterAveCluster<-list()
  #Assigns the first cluster groupof type list as the first entry in the master list 
  masterAveCluster[1]=list(aveCluster1) 
  masterAveCluster[2]=list(aveCluster2)
  masterAveCluster[3]=list(aveCluster3)
  masterAveCluster[4]=list(aveCluster4)
  masterAveCluster[5]=list(aveCluster5)
  masterAveCluster[6]=list(aveCluster6)
  masterAveCluster[7]=list(aveCluster7)
  masterAveCluster[8]=list(aveCluster8)
  

  #First for loop that will cycle through the number of cluster groups NEED TO CHANGE K, PROBABLY WRONG
  for (j in 1:k){
    #Defines which cluster group we are looking at
    clustergroup<-masterAveCluster[j]
    #Starts the second for loop that cycles through each image in the cluster group and adds to the html
    for (i in clustergroup){
      HTMLoutput=file.path(iconlocation,paste(k, "GroupsAverage.html", sep=''))
      image1 = paste(i, ".jpg", sep="") #you have to put this image into your working directory
      cat("<table border=0><td width=50%>",file=HTMLoutput, append=TRUE)
      HTMLInsertGraph(image1,file=HTMLoutput,caption=paste("Cluster group", j),
                      Align='left',WidthHTML=150)
    }
  }
  
  
  
  
  #Complete Linkage Begins
  dend_comp <- as.data.frame(cutree(comp, k))
  dend_comp[ ,2] <- rownames(dend_ave)
  colnames(dend_comp) <- c("group", "icon")
  
  
  ###Creates a list of icon names in cluster 1###
  compCluster1 <- list()
  l<-1
  for (i in dend_comp[,1]){
    if (i==1){
      compCluster1=append(compCluster1, dend_comp[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 2###
  compCluster2 <- list()
  l<-1
  for (i in dend_comp[,1]){
    if (i==2){
      compCluster2=append(compCluster2, dend_comp[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 3###
  compCluster3 <- list()
  l<-1
  for (i in dend_comp[,1]){
    if (i==3){
      compCluster3=append(compCluster3, dend_comp[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 4###
  compCluster4 <- list()
  l<-1
  for (i in dend_comp[,1]){
    if (i==4){
      compCluster4=append(compCluster4, dend_comp[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 5###
  compCluster5 <- list()
  l<-1
  for (i in dend_comp[,1]){
    if (i==5){
      compCluster5=append(compCluster5, dend_comp[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 6###
  compCluster6 <- list()
  l<-1
  for (i in dend_comp[,1]){
    if (i==6){
      compCluster6=append(compCluster6, dend_comp[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 7###
  compCluster7 <- list()
  l<-1
  for (i in dend_comp[,1]){
    if (i==7){
      compCluster1=append(compCluster7, dend_comp[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 8###
  compCluster8 <- list()
  l<-1
  for (i in dend_comp[,1]){
    if (i==8){
      compCluster8=append(compCluster8, dend_comp[l,2])
    }
    l=l+1
  }
  
  
  #Creates a master list that holds nested lists of the created cluster groups
  masterCompCluster<-list()
  #Assigns the first cluster groupof type list as the first entry in the master list 
  masterCompCluster[1]=list(compCluster1) 
  masterCompCluster[2]=list(compCluster2)
  masterCompCluster[3]=list(compCluster3)
  masterCompCluster[4]=list(compCluster4)
  masterCompCluster[5]=list(compCluster5)
  masterCompCluster[6]=list(compCluster6)
  masterCompCluster[7]=list(compCluster7)
  masterCompCluster[8]=list(compCluster8)
  

  #First for loop that will cycle through the number of cluster groups NEED TO CHANGE K, PROBABLY WRONG
  for (j in 1:k){
    #Defines which cluster group we are looking at
    clustergroup<-masterCompCluster[j]
    #Starts the second for loop that cycles through each image in the cluster group and adds to the html
    for (i in clustergroup){
      HTMLoutput=file.path(iconlocation,paste(k, "GroupsComplete.html", sep=''))
      image1 = paste(i, ".jpg", sep="") #you have to put this image into your working directory
      cat("<table border=0><td width=50%>",file=HTMLoutput, append=TRUE)
      HTMLInsertGraph(image1,file=HTMLoutput,caption=paste("Cluster group", j),
                      Align='left',WidthHTML=150)
    }
  }
  
  
  
  
  #Ward's Method Begins
  dend_ward <- as.data.frame(cutree(ward, k))
  dend_ward[ ,2] <- rownames(dend_ave)
  colnames(dend_ward) <- c("group", "icon")
  
  ###Creates a list of icon names in cluster 1###
  wardCluster1 <- list()
  l<-1
  for (i in dend_ward[,1]){
    if (i==1){
      wardCluster1=append(wardCluster1, dend_ward[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 2###
  wardCluster2 <- list()
  l<-1
  for (i in dend_ward[,1]){
    if (i==2){
      wardCluster2=append(wardCluster2, dend_ward[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 3###
  wardCluster3 <- list()
  l<-1
  for (i in dend_ward[,1]){
    if (i==3){
      wardCluster3=append(wardCluster3, dend_ward[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 4###
  wardCluster4 <- list()
  l<-1
  for (i in dend_ward[,1]){
    if (i==4){
      wardCluster4=append(wardCluster4, dend_ward[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 5###
  wardCluster5 <- list()
  l<-1
  for (i in dend_ward[,1]){
    if (i==5){
      wardCluster5=append(wardCluster5, dend_ward[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 6###
  wardCluster6 <- list()
  l<-1
  for (i in dend_ward[,1]){
    if (i==6){
      wardCluster6=append(wardCluster6, dend_ward[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 7###
  wardCluster7 <- list()
  l<-1
  for (i in dend_ward[,1]){
    if (i==7){
      wardCluster7=append(wardCluster7, dend_ward[l,2])
    }
    l=l+1
  }
  
  ###Creates a list of icon names in cluster 8###
  wardCluster8 <- list()
  l<-1
  for (i in dend_ward[,1]){
    if (i==8){
      wardCluster8=append(wardCluster8, dend_ward[l,2])
    }
    l=l+1
  }
  
  #Creates a master list that holds nested lists of the created cluster groups
  masterWardCluster<-list()
  #Assigns the first cluster groupof type list as the first entry in the master list 
  masterWardCluster[1]=list(wardCluster1) 
  masterWardCluster[2]=list(wardCluster2)
  masterWardCluster[3]=list(wardCluster3)
  masterWardCluster[4]=list(wardCluster4)
  masterWardCluster[5]=list(wardCluster5)
  masterWardCluster[6]=list(wardCluster6)
  masterWardCluster[7]=list(wardCluster7)
  masterWardCluster[8]=list(wardCluster8)

  
  #First for loop that will cycle through the number of cluster groups NEED TO CHANGE K, PROBABLY WRONG
  for (j in 1:k){
    #Defines which cluster group we are looking at
    clustergroup<-masterWardCluster[j]
    #Starts the second for loop that cycles through each image in the cluster group and adds to the html
    for (i in clustergroup){
      HTMLoutput=file.path(iconlocation,paste(k, "GroupsWard.html", sep=''))
      image1 = paste(i, ".jpg", sep="") #you have to put this image into your working directory
      cat("<table border=0><td width=50%>",file=HTMLoutput, append=TRUE)
      HTMLInsertGraph(image1,file=HTMLoutput,caption=paste("Cluster group", j),
                      Align='left',WidthHTML=150)
    }
  } 
  
}














