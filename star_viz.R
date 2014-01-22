#Instruction
#1. Run CatAnalysis.R;
#2. Set nr and nc (nr * nc = total number of participants) (Improve later..);
#3. Run the script;

#Set the layout of the output figure (nr * nc >= total number of participants)
nr <- 6
nc <- 5

#Set the number of participant groups resulting from cluster analysis
participant_cluster <- 3

#Define the starting angle (in degree) with due north as 0 degree and increment (in degree)
starting_angle <- 0
increment <- 360/n_icons

#Define the name of the exported file
filename <- "star_viz_dendrogram.png"

##Define 16 colors
colors <- c("firebrick2", "dodgerblue4", "darkgreen", "darkorange2",
            "chocolate4", "black", "deeppink3", "darkmagenta",
            "blueviolet", "darkslategrey", "saddlebrown", "indianred1")

##Define a function that converts degrees to radians
#NOTE: Authored by Fabio Marroni
#URL: http://fabiomarroni.wordpress.com/2010/12/23/r-function-to-convert-degrees-to-radians/
degrees.to.radians<-function(degrees=45,minutes=30)
{
  if(!is.numeric(minutes)) stop("Please enter a numeric value for minutes!\n")
  if(!is.numeric(degrees)) stop("Please enter a numeric value for degrees!\n")
  decimal<-minutes/60
  c.num<-degrees+decimal
  radians<-c.num*pi/180
  return(radians)
}

#Install and load package(s)
#install.packages("Cairo")
library("Cairo")

#Read in files
zip_path <- paste(path, "zip/", sep="")
files <- list.files(zip_path)

#Calculate participant similarity
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
cluster <- hclust(method = "ward", as.dist(dm))
dend <- as.dendrogram(cluster)

#Cut the dendrogram and save the result in group_membership
group_membership <- cutree(cluster, participant_cluster)
group_membership <- as.data.frame(group_membership)

#Create a list to store all assignment.csv from participants
overall_container <- list()

for(i in 1:length(files)){
  
  #Get the participant number
  participant_number <- substring(files[i], 1, nchar(files[i]) - 4)
  
  #Construct the full file name for assignment.csv file
  assignment <- paste("./", participant_number, "/", participant_number, "assignment.csv", sep = "")
  
  #Read in the assignment.csv as d
  d <- read.delim(assignment, header=FALSE, sep=",",stringsAsFactors=F)
  
  #Order the assignment by the icon index
  d <- d[order(d[,3]),]
  
  #Read in the corrected icon order from Jinlong's Dropbox
  icon_order <- read.csv(file = "http://dl.dropboxusercontent.com/u/15662467/birdseye_order_fix.csv", 
                         header = F, stringsAsFactors = F)
  
  #Extract the icon index from the icon names
  for(i in 1:nrow(icon_order)){
    icon_order[i, 3] <- substr(icon_order[i, 1], 5, nchar(icon_order[i, 1]))
  }
  
  #Bind the assignment with the corrected icon order
  d <- cbind(d, as.numeric(icon_order[,3]))
  
  #At this point, the 4th column indicates the corrected_order of all icons
  colnames(d) <- c("participant", "group", "fake_order", "corrected_order")
  
  #Add the participant's membership from cluster analysis to d(assignment.csv)
  d$membership <- rep(group_membership[rownames(group_membership) == d[1, 1], ], nrow(d))
  
  #Add the assignment to the overall_container
  overall_container <- c(overall_container, list(d))
}

#Reorder all assignment.csv by their membership from cluster analysis
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

#Start a new canvas with Cairo(function)
#The width/height of the graphic can be changed via "width" and "height" parameter
Cairo(file = paste(path, filename, sep = ""), type = "png", units = "px", width = nc * 300 + 200, height = nr * 300 + 300, dpi = 1200)

#Partition the canvas using nr and nc defined previously
par(mfrow = c(nr, nc))

#Start to draw the starplots
for(d in ordered_container){
  
  #Have fun with plotting individual starplot:-)
  plot(0, 0, type = "n", xaxt = "n", yaxt = "n", 
       xlim = c(-5, 5), ylim =  c(-5, 5), 
       bty="n", xlab = "", ylab = "", 
       main = d[1, 1], cex.main = 3.5)
  
  for(i in 1:nrow(d)){
    #Determin the x and y for each segment(line)
    x <- -5*sin(degrees.to.radians(5*d[i, 4]))
    y <- 5*cos(degrees.to.radians(5*d[i, 4]))
    
    #Draw the segment
    segments(0, 0, x, y, col = colors[d[i,2]+1], lwd = 1.5)
    
    #Define the color for the bounding box
    bbox_colors <- c("black", "red", "green", "blue", "pink", "yellow", "brown", "grey", "violet")
    
    #Get the boudning box color by the participant membership from cluster analysis
    bbox_color <- bbox_colors[d[1, 5]]
    
    #Draw the bounding box
    segments(-5, -5, -5, 5, col = bbox_color, lwd = 2)
    segments(-5, 5, 5, 5, col = bbox_color, lwd = 2)
    segments(5, 5, 5, -5, col = bbox_color, lwd = 2)
    segments(5, -5, -5, -5, col = bbox_color, lwd = 2)
  }
}

#Turn off the drawing device
dev.off()










