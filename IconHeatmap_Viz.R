#install(gplots)
library(gplots)
#install(RColorBrewer)
library(RColorBrewer)
#install.packages("Cairo")
library("Cairo")



#####BEGIN user input information#####
#Must define a string for variable "path"
#Must have a folder named "zip" in your path where the zip files are located
#Line 192 contains Legend creation for the heatmaps. Comment out if not desired


path <- "Desktop/test/test2"


#####END user input#####





##Checks if "/" exists after path. If not, one is added
if(substr(path, nchar(path), nchar(path)) != "/"){
  path <- paste(path, "/", sep = "")
}

##Creates a folder "icon" within the path to save the icon names csv to 
klipart_path <- paste(path, "icon/", sep = "")
dir.create(klipart_path)

##Icon list getter: get a list of icon names
##It also saves the icon.csv needed for KlipArt
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


##Defines variable "all_icons" as a list of icon names
all_icons <- sort(icon_list_getter(path))


#####IconHeatmap_Viz#####

##Read in files
zip_path <- paste(path, "zip/", sep="")
files <- list.files(zip_path)

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
  
  
  ##Gathers linguistic responses from participants 
  batch <- unzip(paste(zip_path, p, sep=""))
  check <- batch[6]
  if(substr(check, nchar(check) - 13, nchar(check)) != "batch.csv"){
    check <- batch[5]
  }
  
  ##Reads in linguistic response data
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
  
  colnames(m) <- all_icons
  rownames(m) <- all_icons
  
  ngroups <- length(groups)
  myBreaks <- -1:ngroups 
  
  myColors <- c((rgb(102, 194, 165,  maxColorValue=255)), (rgb(252, 141, 98,  maxColorValue=255)), (rgb(166, 216, 84,  maxColorValue=255)), (rgb(228, 26, 28,  maxColorValue=255)), (rgb(231, 138, 165,  maxColorValue=255)), (rgb(141, 160, 203,  maxColorValue=255)), (rgb(55, 126, 184,  maxColorValue=255)), (rgb(152, 78, 163,  maxColorValue=255)), (rgb(77, 175, 74,  maxColorValue=255)), (rgb(255, 127, 0,  maxColorValue=255)), (rgb(255, 255, 51,  maxColorValue=255)), (rgb(166, 86, 40,  maxColorValue=255)), (rgb(229, 196, 148,  maxColorValue=255)), (rgb(50, 50, 50,  maxColorValue=255)), (rgb(110, 110, 110,  maxColorValue=255)), (rgb(170, 170, 170,  maxColorValue=255)), (rgb(255, 255, 153,  maxColorValue=255)))
  myColors <- append(myColors, brewer.pal(12, "Paired"))
  myColors <- append(myColors, brewer.pal(11, "Set3"))
  myColors <- myColors[(1:(ngroups+1))]
  
  p <- substr(p, 1, nchar(p) - 4 )
  
  jpeg(filename = paste(path, p, "heatmap.jpeg", sep = ""), 
       quality=7000, width = 1200, height = 1200, pointsize = 21)
  heatmap.2(m, Rowv = NA, Colv = NA, trace = "none", col = myColors, 
            breaks = myBreaks, key = "FALSE", na.color = "black")
  cex=0.2
  #legend("topleft",legend=(linguistic_info[,2]), fill=myColors)
  
  dev.off()
  
}