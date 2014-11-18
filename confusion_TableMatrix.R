##Confusion Matrix
##Kevin Sparks
##5/18/14

##Packages needed
#install(gplots)
require(gplots)
#install(RColorBrewer)
require(RColorBrewer)
#install.packages("Cairo")
require(Cairo)






###############----------user input----------###############




##Define path where participant data is, making sure there is a folder named 'zip' that holds the zipped files in the last folder in the path
path <- 'C:/Users/...'

##Read in expert classification csv, making sure the path includes the csv file at the end
expert_path <- 'C:/Users/.../expert_classification.csv'
expert_classification <- read.csv(expert_path, header=TRUE)

##Define which 'truth' classification to use (nlcd, expert) to measure the participants against
##define 'predicted_var' as 5 for nlcd 2006
##define 'predicted_var' as 6 for nlcd 2011
##define 'predicted_var' as 7 for expert
predicted_var <- 6





###############----------END user input----------###############








##Checks if "/" exists after path. If not, one is added
if(substr(path, nchar(path), nchar(path)) != "/"){
  path <- paste(path, "/", sep = "")
}

##Creates a folder "icon" within the path to save the icon names csv to 
klipart_path <- paste(path, "icon/", sep = "")
dir.create(klipart_path)








###############----------Define Functions----------###############




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




###############----------END Define Functions----------###############








###############----------Preperations for 'for loops'----------###############




##list of icon names
all_icons <- sort(icon_list_getter(path))


##taking the 13th and 14th character which is LC class
icon_class <- substring(all_icons, 13, 14)


##Read in participants zip files
zip_path <- paste(path, "zip/", sep="")
files <- list.files(zip_path)




##Creation of empty data frmae to hold confusion outputs

##Creates an empty 'master' data frame to hold confusion table output for land cover class
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




##Creates empty 77 by 13 matrix to hold confusiontable output for each icon
icon_confusion_matrix  <- matrix(0, 77, 13)
colnames(icon_confusion_matrix) = c('BA','CC','dL','dO','EW','FO',
                                     'GS','OW','PH','SS','WW','wrong group','correct group')
rownames(icon_confusion_matrix) <- all_icons




##Create a dataframe to store the prototype frequency
freq <- data.frame(icon = icon_list_getter(path), 
                     icon_index = 0: (length(icon_list_getter(path))-1), 
                     count = rep(0, length(icon_list_getter(path))),
                     wrong = rep(0, length(icon_list_getter(path))),
                     correct = rep(0, length(icon_list_getter(path)))
                    )




###############----------END Preperations for 'for loops'----------###############















##################################################################################################















##Lay participants compared against 'truth' classification (nlcd/expert)



##for loop for nlcd analysis 
if(predicted_var == 5 | predicted_var == 6){
  ##Begins for loop to loop through participants' zip folders
  for(p in files){
    #p <- '21510002.zip'
    ##Unzippes participant folder
    participant <- unzip(paste(zip_path, p, sep=""))
    ##Looks for the "assignment.csv" file within the now unzipped folder
    check <- participant[6]
    if(substr(check, nchar(check) - 13, nchar(check)) != "assignment.csv"){
      check <- participant[4]
    }
    ##reads in "assignnmet.csv" file
    d <- read.delim(check, header=FALSE, sep=",",stringsAsFactors=F)
    



    ##Creates a new vector that uses the numeric category classes to 
    ##make matching LC class. These are, how many icons where placed
    ##into each predefined group. 
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
      
    ##Adds the vector to the assignment.csv data frame. 
    ##This new vector acts as a meaningful relpacement for column 2. 
    d <- cbind(d, class_group)
    
    
    

    ##nlcd classification order
    
    ##Reads in LC class order
    icons_csv <- participant[7]
    icon_order_result <- read.delim(icons_csv, header=FALSE, sep=",")
    icon_order_result1 <- substring(icon_order_result[,2], 21, 22)
    nlcd_class <- icon_order_result1
    
    ##Adds the class order to data frame. 
    ##This new vector acts as a meaningful replacement for column 3.
    d <- cbind(d, nlcd_class)
    

    ##2011 nlcd changes

    counter <- 0
    icon_order_result_2011 <- data.frame('2011_class' = character(77), stringsAsFactors=FALSE)
    for(c in icon_order_result[,2]){
      counter <- counter + 1 
      if(substring(c, 9, 14) == '31_082'){
        icon_order_result_2011[counter,] <- '/events/31_082_u_GA_SS.jpg'
        print(counter)
      }
      if(substring(c, 9, 14) == '36_096'){
        icon_order_result_2011[counter,] <- '/events/36_096_s_OK_dO.jpg'
        print(counter)
      }
      if(substring(c, 9, 14) == '44_075'){
        icon_order_result_2011[counter,] <- '/events/44_075_n_NY_FO.jpg'
        print(counter)
      }
      if(icon_order_result_2011[counter,] == ''){
        icon_order_result_2011[counter,] <- c
      }
    }
    
    nlcd_class_2011 <- icon_order_result_2011
    nlcd_class_2011 <- substring(nlcd_class_2011[,], 21, 22)
    d <- cbind(d, nlcd_class_2011)

    
    
    ##adding expert classification in same order of participant icons
    
    expert_lc <- c()
    for(a in icon_order_result[,1]){
      for(b in expert_classification[,3]){
        if(a == b){
          expert_lc <- c(expert_lc, toString(expert_classification[b+1,8]))
        }
      }
    }
    
    ##Adds the expert replaced class order to data frame
    ##This new vector is the properly ordered expert classification instead of the nlcd classification
    d <- cbind(d, expert_lc)



    






    ##CONFUSION OF ICONS##
    
    for(x in 1:nrow(d)) {
      if (as.character(d[x,4]) == as.character(d[x,5])) {
        icon_confusion_matrix[d[x,3]+1,d[x,2]+1] <- icon_confusion_matrix[d[x,3]+1,d[x,2]+1] - 1
        icon_confusion_matrix[d[x,3]+1,13] <- icon_confusion_matrix[d[x,3]+1,13] + 1
       } else {
         icon_confusion_matrix[d[x,3]+1,d[x,2]+1] <- icon_confusion_matrix[d[x,3]+1,d[x,2]+1] + 1
          icon_confusion_matrix[d[x,3]+1,12] <- icon_confusion_matrix[d[x,3]+1,12] + 1
       }
    }
    #print(icon_confusion_matrix)


    ##PROTOTYPE CONFUSION##  
     
    for (x in participant) {
      if (substr(x, nchar(x) - 14, nchar(x)) == "gprototypes.csv") {
        check = x
        break
      }
    }
    
    print(check)
    prototype <- read.csv(check, header = F, stringsAsFactors = F)
    for(j in 1:nrow(prototype)){
      #if(prototype[j, 4] != ""){
        freq[as.numeric(prototype[j, 3]) + 1, 3] <- freq[as.numeric(prototype[j, 3]) + 1, 3] + 1

        #print(as.numeric(prototype[j, 3]))
        #print(character(d[as.numeric(prototype[j, 3]),4]))
        #print(as.character(d[as.numeric(prototype[j, 3]),5]))
          
        if (as.character(d[as.numeric(prototype[j, 3])+1,4]) == as.character(d[as.numeric(prototype[j, 3])+1,5])) {
          freq[as.numeric(prototype[j, 3]) + 1, 5] <- freq[as.numeric(prototype[j, 3]) + 1, 5] + 1
        } else {
          freq[as.numeric(prototype[j, 3]) + 1, 4] <- freq[as.numeric(prototype[j, 3]) + 1, 4] + 1
        }
        # }
    }






    



    ##Class confusion matrix##

    ##actual - actions of the participants 
    ##predicted - the 'truth' classification (nlcd, expert)
    
    actual <- d[,4]
    predicted <- d[,predicted_var]
    data <- data.frame(actual, predicted)
    #t(table(data$actual, data$predicted))
    #transpose to occur at the end




    ##Checks in case participant did not use all 11 classes

    
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
    
    
  }

  confusion_table_master <- t(confusion_table_master) 


  ##Write class confusion matrix
  write.table(confusion_table_master, file=paste("ClassConfusionMatrix", substring(files[1], 0, 4), ".csv", sep=''), sep=',')

  ##Write icon confusion matrix
  write.table(icon_confusion_matrix, file=paste("IconConfusionMatrix", substring(files[1], 0, 4), ".csv", sep=''), sep=',')

  ##Write prototype info
  write.table(freq, file=paste("PrototypeWithConfusion", substring(files[1], 0, 4), ".csv", sep=''), sep = ",")




  ##Confusion Percentages

  
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

  ##Write class confusion percentages
  write.table(confusion_perc, file=paste("ClassConfusionMatrixNLCDPercentage", substring(files[1], 0, 4), ".csv", sep = ""), sep = ",")
}





##for loop for expert analysis 
if(predicted_var == 6){
  ##Begins for loop to loop through participants' zip folders
  for(p in files){
    #p <- '21520002.zip'
    ##Unzippes participant folder
    participant <- unzip(paste(zip_path, p, sep=""))
    ##Looks for the "assignment.csv" file within the now unzipped folder
    check <- participant[6]
    if(substr(check, nchar(check) - 13, nchar(check)) != "assignment.csv"){
      check <- participant[4]
    }
    ##reads in "assignnmet.csv" file
    d <- read.delim(check, header=FALSE, sep=",",stringsAsFactors=F)
    



    ##Creates a new vector that uses the numeric category classes to 
    ##make matching LC class. These are, how many icons where placed
    ##into each predefined group. 
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
      
    ##Adds the vector to the assignment.csv data frame. 
    ##This new vector acts as a meaningful relpacement for column 2. 
    d <- cbind(d, class_group)
    
    
    

    ##nlcd classification order
    
    ##Reads in LC class order
    icons_csv <- participant[7]
    icon_order_result <- read.delim(icons_csv, header=FALSE, sep=",")
    icon_order_result1 <- substring(icon_order_result[,2], 21, 22)
    nlcd_class <- icon_order_result1
    
    ##Adds the class order to data frame. 
    ##This new vector acts as a meaningful replacement for column 3.
    d <- cbind(d, nlcd_class)
    

    
    
    ##adding expert classification in same order of participant icons
    
    expert_lc <- c()
    for(a in icon_order_result[,1]){
      for(b in expert_classification[,3]){
        if(a == b){
          expert_lc <- c(expert_lc, toString(expert_classification[b+1,8]))
        }
      }
    }
    
    ##Adds the expert replaced class order to data frame
    ##This new vector is the properly ordered expert classification instead of the nlcd classification
    d <- cbind(d, expert_lc)



    






    ##CONFUSION OF ICONS##
    
    for(x in 1:nrow(d)) {
      if (as.character(d[x,4]) == as.character(d[x,5])) {
        icon_confusion_matrix[d[x,3]+1,d[x,2]+1] <- icon_confusion_matrix[d[x,3]+1,d[x,2]+1] - 1
        icon_confusion_matrix[d[x,3]+1,13] <- icon_confusion_matrix[d[x,3]+1,13] + 1
       } else {
         icon_confusion_matrix[d[x,3]+1,d[x,2]+1] <- icon_confusion_matrix[d[x,3]+1,d[x,2]+1] + 1
          icon_confusion_matrix[d[x,3]+1,12] <- icon_confusion_matrix[d[x,3]+1,12] + 1
       }
    }
    #print(icon_confusion_matrix)


    ##PROTOTYPE CONFUSION##  
     
    for (x in participant) {
      if (substr(x, nchar(x) - 14, nchar(x)) == "gprototypes.csv") {
        check = x
        break
      }
    }
    
    print(check)
    prototype <- read.csv(check, header = F, stringsAsFactors = F)
    for(j in 1:nrow(prototype)){
      #if(prototype[j, 4] != ""){
        freq[as.numeric(prototype[j, 3]) + 1, 3] <- freq[as.numeric(prototype[j, 3]) + 1, 3] + 1

        #print(as.numeric(prototype[j, 3]))
        #print(character(d[as.numeric(prototype[j, 3]),4]))
        #print(as.character(d[as.numeric(prototype[j, 3]),5]))
          
        if (as.character(d[as.numeric(prototype[j, 3])+1,4]) == as.character(d[as.numeric(prototype[j, 3])+1,5])) {
          freq[as.numeric(prototype[j, 3]) + 1, 5] <- freq[as.numeric(prototype[j, 3]) + 1, 5] + 1
        } else {
          freq[as.numeric(prototype[j, 3]) + 1, 4] <- freq[as.numeric(prototype[j, 3]) + 1, 4] + 1
        }
        # }
    }






    



    ##Class confusion matrix##

    ##actual - actions of the participants 
    ##predicted - the 'truth' classification (nlcd, expert)
    
    actual <- d[,4]
    predicted <- d[,predicted_var]
    data <- data.frame(actual, predicted)
    #t(table(data$actual, data$predicted))
    #transpose to occur at the end




    ##Checks in case participant did not use all 11 classes

    
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
    confusion_table_DF$WW <- c(rep(0, nrow(confusion_matrix)))
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
  }

  confusion_table_master <- t(confusion_table_master) 


  ##Write class confusion matrix
  write.table(confusion_table_master, file=paste("ClassConfusionMatrixEx", substring(files[1], 0, 4), ".csv", sep=''), sep=',')

  ##Write icon confusion matrix
  write.table(icon_confusion_matrix, file=paste("IconConfusionMatrixEx", substring(files[1], 0, 4), ".csv", sep=''), sep=',')

  ##Write prototype info
  write.table(freq, file=paste("PrototypeWithConfusionEx", substring(files[1], 0, 4), ".csv", sep=''), sep = ",")




  ##Confusion Percentages

  
  ##Percentages

  # BA_tot <- 2
  # CC_tot <- 9
  # dL_tot <- 8
  # dO_tot <- 5
  # EW_tot <- 4
  # FO_tot <- 16
  # GS_tot <- 3
  # OW_tot <- 7
  # PH_tot <- 10
  # SS_tot <- 13
  exp_class_total <- c(2,9,8,5,4,16,3,7,10,13,0)

  noP <- length(files)

  multiplier_var <- exp_class_total*noP

  ##create empty matrix
  confusion_perc_ex_cl <- matrix(data=NA, nrow=11, ncol=11)


  for(x in 1:11){
    for(y in 1:11){
      confusion_perc_ex_cl[x,y] <- round((confusion_table_master[x,y]/multiplier_var[x])*100, 2)
    }
  }


  colnames(confusion_perc_ex_cl) = c('BA','CC','dL','dO','EW','FO',
                               'GS','OW','PH','SS','WW')
  rownames(confusion_perc_ex_cl) = c('BA','CC','dL','dO','EW','FO',
                               'GS','OW','PH','SS','WW')

  confusion_perc_ex_cl[11,] <- c(rep(0,11))

  ##Write participants compared against expert class confusion matrix percentage 
  write.table(confusion_perc_ex_cl, file=paste("ClassConfusionMatrixExpertPercentage", substring(files[1], 0, 4), ".csv", sep = ""), sep=',')
}















##################################################################################################















##Individual expert analysis versus nlcd predicted




confusion_table_master_ex  <- data.frame(BA = numeric(11), 
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
rownames(confusion_table_master_ex) = c('BA','CC','dL','dO','EW','FO',
                                     'GS','OW','PH','SS','WW')



##Experts 1-4
##Predicted variable is ordered nlcd classification
for(k in 4:7){
  #k <- 4
  actual <- expert_classification[,k]
  predicted <- icon_class
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
    
  confusion_table_master_ex <- confusion_table_master_ex+confusion_table_DF
}

confusion_table_master_ex <- t(confusion_table_master_ex)


##Write expert class confusion matrix
write.table(confusion_table_master_ex, file="ExpertClassConfusionMatrix.csv", sep=',')




##Confusion Percentages

##create empty matrix
confusion_perc_ex <- matrix(data=NA, nrow=11, ncol=11)

##number of particiapnts 
noP <- 4
##total number of icons in each row
tniir <- noP*7

for(x in 1:11){
  for(y in 1:11){
    confusion_perc_ex[x,y] <- round((confusion_table_master_ex[x,y]/tniir)*100, 2)
  }
}

colnames(confusion_perc_ex) = c('BA','CC','dL','dO','EW','FO',
                             'GS','OW','PH','SS','WW')
rownames(confusion_perc_ex) = c('BA','CC','dL','dO','EW','FO',
                             'GS','OW','PH','SS','WW')


##Write expert class percentage confusion matrix
write.table(confusion_perc_ex, file="ExpertClassConfusionPerc.csv", sep = ",")















##################################################################################################
















##Expert agreement compared against nlcd/participants


##Expert agreement analysis compared against nlcd

expert_classification$agreement
actual <- expert_classification$agreement
predicted <- icon_class
data <- data.frame(actual, predicted)
data_table <- table(data$actual, data$predicted)
expert_agreement_matrix <- data_table[-1,]


write.table(expert_agreement_matrix, file="ExpertAgreementNLCD.csv", sep = ",")




########################################





##Expert agreement analysis compared against participants 


confusion_table_master <- data.frame(CC = numeric(11),
  dL = numeric(11),
  FO = numeric(11),
  OW = numeric(11),
  SS = numeric(11))
rownames(confusion_table_master) = c('BA','CC','dL','dO','EW','FO',
                                     'GS','OW','PH','SS','WW')


##template data frame to be called for later in analysis 
confusion_table_master_temp  <- data.frame(BA = numeric(11), 
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
rownames(confusion_table_master_temp) = c('BA','CC','dL','dO','EW','FO',
                                     'GS','OW','PH','SS','WW')


for(p in files){
  #p <- '21510013.zip'
  ##Unzippes participant folder
  participant <- unzip(paste(zip_path, p, sep=""))
  ##Looks for the "assignment.csv" file within the now unzipped folder
  check <- participant[6]
  if(substr(check, nchar(check) - 13, nchar(check)) != "assignment.csv"){
    check <- participant[4]
  }
  ##reads in "assignnmet.csv" file
  d <- read.delim(check, header=FALSE, sep=",",stringsAsFactors=F)
  



  ##Creates a new vector that uses the numeric category classes to 
  ##make matching LC class. These are, how many icons where placed
  ##into each predefined group. 
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
    
  ##Adds the vector to the assignment.csv data frame. 
  ##This new vector acts as a meaningful relpacement for column 2. 
  d <- cbind(d, class_group)
  
  
  

  ##nlcd classification order
  
  ##Reads in LC class order
  icons_csv <- participant[7]
  icon_order_result <- read.delim(icons_csv, header=FALSE, sep=",")
  icon_order_result1 <- substring(icon_order_result[,2], 21, 22)
  nlcd_class <- icon_order_result1
  
  ##Adds the class order to data frame. 
  ##This new vector acts as a meaningful replacement for column 3.
  d <- cbind(d, nlcd_class)
  

  
  
  ##adding expert classification in same order of participant icons
  
  expert_lc <- c()
  for(a in icon_order_result[,1]){
    for(b in expert_classification[,3]){
      if(a == b){
        expert_lc <- c(expert_lc, toString(expert_classification[b+1,8]))
      }
    }
  }
  
  ##Adds the expert replaced class order to data frame
  ##This new vector is the properly ordered expert classification instead of the nlcd classification
  d <- cbind(d, expert_lc)



  
##adding expert agreement in same order of participant icons
  
  expert_agree <- c()
  for(a in icon_order_result[,1]){
    for(b in expert_classification[,3]){
      if(a == b){
        expert_agree <- c(expert_agree, toString(expert_classification[b+1,9]))
      }
    }
  }
  
  ##Adds the expert replaced class order to data frame
  ##This new vector is the properly ordered expert classification instead of the nlcd classification
  d <- cbind(d, expert_agree)




  ##Class confusion matrix##

  ##actual - actions of the participants 
  ##predicted - the 'truth' classification (nlcd, expert)
  
  actual <- d[,4]
  predicted <- d[,7]
  data <- data.frame(actual, predicted)
  data_table <- table(data)
  data_table <- data_table[,-1]
  #transpose to occur at the end





  ##Checks in case participant did not use all 11 classes

  
  confusion_table <- table(data$actual, data$predicted)[,-1]
  confusion_matrix <- as.matrix(confusion_table)

  confusion_table_DF <- data.frame(c(rep(0,nrow(confusion_table))))
  confusion_table_DF$CC <- confusion_matrix[,1]
  confusion_table_DF$dL <- confusion_matrix[,2]
  confusion_table_DF$FO <- confusion_matrix[,3]
  confusion_table_DF$OW <- confusion_matrix[,4]
  confusion_table_DF$SS <- confusion_matrix[,5]
  # confusion_table_DF$FO <- confusion_matrix[,6]
  # confusion_table_DF$GS <- confusion_matrix[,7]
  # confusion_table_DF$OW <- confusion_matrix[,8]
  # confusion_table_DF$PH <- confusion_matrix[,9]
  # confusion_table_DF$SS <- confusion_matrix[,10]
  # confusion_table_DF$WW <- confusion_matrix[,11]
  confusion_table_DF <- confusion_table_DF[,-1]
  rownames(confusion_table_DF) = rownames(confusion_table)   
  
  for(i in 1:10){
    if(rownames(confusion_table_DF)[i] != unique(colnames(confusion_table_master_temp)[i])){
      newRowNames <- c(rownames(confusion_table_DF)[1:i-1], unique(colnames(confusion_table_master_temp)[i]), rownames(confusion_table_DF)[i:nrow(confusion_table_DF)])
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
}

confusion_table_master <- t(confusion_table_master) 


write.table(confusion_table_master, file=paste("ExpertAgreement", substring(files[1], 0, 4), ".csv", sep = ""), sep = ",")
