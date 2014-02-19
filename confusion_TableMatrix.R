##Confusion Matrix
##Kevin Sparks
##2/18/14

##Packages needed
#install(gplots)
require(gplots)
#install(RColorBrewer)
require(RColorBrewer)
#install.packages("Cairo")
require(Cairo)

##Define path, making sure there is a folder named 'zip' that 
##holds the zipped files in the last folder in the path
path <- 'C:/Users/Sparks/Desktop/alex/Projects/CLP/catscan/participant_results/nonfree/2_17'


#####END user input#####

##Checks if "/" exists after path. If not, one is added
if(substr(path, nchar(path), nchar(path)) != "/"){
  path <- paste(path, "/", sep = "")
}

##Creates a folder "icon" within the path to save the icon names csv to 
klipart_path <- paste(path, "icon/", sep = "")
dir.create(klipart_path)


###----------Define Functions----------###


#Icon list getter: get a list of icon names
#It also saves the icon.csv needed for KlipArt
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
  
  #Reorder icon names by icon index
  icons <- icons[order(icons[, 1]),]
  
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
  
  #Extract the icon names with file type (e.g. .jpg) for KlipArt
  icon_list_klipart <- icons
  for(j in 1:nrow(icon_list_klipart)){
    icon_list_klipart[j, 2] <- substr(icon_list_klipart[j, 2], 9, nchar(icon_list_klipart[j, 2]))
  }
  colnames(icon_list_klipart) <- c("index", "icon_names")
  
  #Sort the icon list by index
  icon_list_klipart <- icon_list_klipart[order(icon_list_klipart$index) , ]
  
  #Export the list as a csv file
  write.table(icon_list_klipart, file = paste(klipart_path, "icon.csv", sep = ""),
              sep = ",", row.names = F,  col.names = F)
  
  #Return the icon list as a vector
  return(icon_list)
}


##Insert row function
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}


###----------End Define Functions----------###


##list of icon names
all_icons <- sort(icon_list_getter(path))

##taking the 13thand 14th character which is LC class
icon_class <- substring(all_icons, 13, 14)

##Read in files
zip_path <- paste(path, "zip/", sep="")
files <- list.files(zip_path)

##Creates an empty 'master' data frame to hold confusion table output
confusion_table_master  <- data.frame(BA = numeric(11), 
                                      CC = numeric(11), 
                                      dL = numeric(11),
                                      dO = numeric(11),
                                      EW = numeric(11),
                                      FO = numeric(11),
                                      GS = numeric(11),
                                      OW = numeric(11),
                                      PH = numeric(11),
                                      SS = numeric(11),
                                      WW = numeric(11))
rownames(confusion_table_master) = c('BA','CC','dL','dO','EW','FO',
                                     'GS','OW','PH','SS','WW')



##Begins for loop to loop through participants' zip folders
for(p in files){
  ##Unzippes participant folder
  participant <- unzip(paste(zip_path, p, sep=""))
  ##Looks for the "assignment.csv" file within the now unzipped folder
  check <- participant[6]
  if(substr(check, nchar(check) - 13, nchar(check)) != "assignment.csv"){
    check <- participant[4]
  }
  
  ##reads in "assignnmet.csv" file
  d <- read.delim(check, header=FALSE, sep=",",stringsAsFactors=F)
  
  ##Creates a new vector that uses the numeric category classes to make matching LC class
  class_group <- c()
  for(j in d[,2]){
    if(j == 0)
      class_group <- c(class_group, 'BA')
    if(j == 1)
      class_group <- c(class_group, 'CC')
    if(j == 2)
      class_group <- c(class_group, 'dL')
    if(j == 3)
      class_group <- c(class_group, 'dO')
    if(j == 4)
      class_group <- c(class_group, 'EW')
    if(j == 5)
      class_group <- c(class_group, 'FO')
    if(j == 6)
      class_group <- c(class_group, 'GS')
    if(j == 7)
      class_group <- c(class_group, 'OW')
    if(j == 8)
      class_group <- c(class_group, 'PH')
    if(j == 9)
      class_group <- c(class_group, 'SS')
    if(j == 10)
      class_group <- c(class_group, 'WW')
  }
    
  ##Adds the vector to the assignment.csv data frame 
  d <- cbind(d, class_group)
  
  ##Reads in LC class order
  icons_csv <- participant[7]
  icon_order_result <- read.delim(icons_csv, header=FALSE, sep=",")
  icon_order_result <- substring(icon_order_result[,2], 21, 22)
  
  ##Adds the class order to data frame
  d <- cbind(d, icon_order_result)
  
  
  ####----------------CONFUSION MATRIX STARTS----------------####
  actual <- d[,4]
  predicted <- d[,5]
  data <- data.frame(actual, predicted)
  table(data$actual, data$predicted)
  
  confusion_table <- table(data$actual, data$predicted)
  confusion_matrix <- as.matrix(confusion_table)

  confusion_table_DF <- data.frame(c(rep(0,nrow(confusion_table))))
  confusion_table_DF$BA <- confusion_matrix[,1]
  confusion_table_DF$CC <- confusion_matrix[,2]
  confusion_table_DF$dL <- confusion_matrix[,3]
  confusion_table_DF$dO <- confusion_matrix[,4]
  confusion_table_DF$EW <- confusion_matrix[,5]
  confusion_table_DF$FO <- confusion_matrix[,6]
  confusion_table_DF$GS <- confusion_matrix[,7]
  confusion_table_DF$OW <- confusion_matrix[,8]
  confusion_table_DF$PH <- confusion_matrix[,9]
  confusion_table_DF$SS <- confusion_matrix[,10]
  confusion_table_DF$WW <- confusion_matrix[,11]
  confusion_table_DF <- confusion_table_DF[,-1]
  rownames(confusion_table_DF) = rownames(confusion_table)
  
  for(i in 1:10){
    if(rownames(confusion_table_DF)[i] != unique(colnames(confusion_table_DF)[i])){
      newRowNames <- c(rownames(confusion_table_DF)[1:i-1], unique(colnames(confusion_table_DF)[i]), rownames(confusion_table_DF)[i:nrow(confusion_table_DF)])
      confusion_table_DF <- insertRow(confusion_table_DF, c(rep(0,11)), i)
      rownames(confusion_table_DF) = newRowNames
    }
  }

  if(length(rownames(confusion_table_DF)) != 11){
    confusion_table_DF <- rbind(confusion_table_DF, c(rep(0,11)))
    rownames(confusion_table_DF) = c('BA','CC','dL','dO','EW','FO',
                                     'GS','OW','PH','SS','WW')
  }
  
  confusion_table_master <- confusion_table_master+confusion_table_DF
  print(p)
  
}


####---------------Confusion Percentages---------------####

##create empty matrix
confusion_perc <- matrix(data=NA, nrow=11, ncol=11)

##number of particiapnts 
noP <- length(files)
##total number of icons in each column
tniic <- noP*7

for(x in 1:11){
  for(y in 1:11){
    confusion_perc[x,y] <- round((confusion_table_master[x,y]/tniic)*100, 2)
  }
}

colnames(confusion_perc) = c('BA','CC','dL','dO','EW','FO',
                             'GS','OW','PH','SS','WW')
rownames(confusion_perc) = c('BA','CC','dL','dO','EW','FO',
                             'GS','OW','PH','SS','WW')


####---------------Confusion Percentages END---------------####


write.table(confusion_perc, file="ConfusionMatrixPerc.csv", sep=',')
write.table(confusion_table_master, file="ConfusionMatrixTable.csv", sep=',')



####----------Chi-Squared Test----------####
confusion_matrix_chi <- as.matrix(confusion_table_master)
chisq.test(confusion_matrix_chi, correct=F)
