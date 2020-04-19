# copy originals to be safe
firstRun<-TRUE # to edit manually
if(firstRun==TRUE) {
    original_directory = "/home/$USER/Documents/Synchro\\ folder/phone\\ folder/diary"
    copy_directory = "/home/$USER/Downloads/mph/diary_data/"
    system(paste("trash ",copy_directory, "diary",sep=""),intern=FALSE,ignore.stderr = TRUE, ignore.stdout = TRUE) # remove old
    com <- paste("cp -R", original_directory, copy_directory,sep=" ")
    system(com,intern=FALSE,ignore.stderr = TRUE, ignore.stdout = TRUE) # copies new
}

# get file hierarchy
com <- "cd /home/$USER/Downloads/mph/data/diary && find . -iname '*txt'"
file_hierarchy <- system(command=com, intern=TRUE)
n <- length(file_hierarchy)

# create and fill up data frame
df <- data.frame()
for(a in seq(length(file_hierarchy))) {
  temp<-strsplit(file_hierarchy[a],"/")[[1]][2:4]
  df[a,"ID"] <- as.numeric(a)
  df[a,"Year"]<-as.numeric(temp[1])
  df[a,"Month"]<-as.numeric(temp[2])
  df[a,"Day"]<-as.numeric(gsub(".txt","",temp[3])) #removes flie extension

  # fill 4th row with the content of the file
  com <- paste("cat /home/$USER/Documents/Synchro\\ folder/phone\\ folder/diary/",file_hierarchy[a],sep="")
  df[a,"Content"]<- tolower(paste(system(command=com,intern=TRUE),collapse="  <br>  "))
}

# add more rows for actual drug use
drugs <- c("Concerta","Ritaline", "Quasym")
df[,"Drug"]<-"NF" # initialized as "not found"
truth<-0
for(a in seq(length(df[,1]))) {
  for(b in drugs) {
    truth <- grep(b, df[a,"Content"],ignore.case=TRUE)
    if(length(truth)==1) {  # because grep returns logical(0) instead of 0, could be fixed with grepl
     df[a,"Drug"] <- ifelse(df[a,"Drug"]=="NF", b, "Several Found!")
    }
    truth <- 0
  }
}

# correction : ritaline and ritaline LP are both matched above so I need to add this to differentiate them :
for(a in seq(length(df[,1]))) {
  if(df[a,"Drug"]=="Ritaline") {
    truth <- grep("lp", df[a,"Content"],ignore.case=TRUE)
    if(length(truth)==1) { 
      df[a,"Drug"] <- "Ritaline LP"
    }
    truth <- NULL
  }
  occ <- length(strsplit(df[a,"Content"],"italin")[[1]])
  if(occ > 2) {     df[a,"Drug"] <- "Several Found!"     } # if italin several times => probably ritaline LP + ritaline IR
}

# prune irrelevant diary entries
df <- df[-which(df["Drug"]=="NF"),] # drug==NF
df <- df[-c(grep("Donne",df[,"Content"],ignore.case=TRUE)),] # "Donne x drugs"
df <- df[-which(df$Year==2019 & df$Month==4 & df$Day==30),] # "prends pas de"
df <- df[-which(df$Year==2019 & df$Month==11 & df$Day==27),]
df <- df[-which(df$Year==2019 & df$Month==11 & df$Day==1),]
df <- na.omit(df) # remove the NA lines in View mode (I have to put this right at the end apparently :/)
df <- df[-which(df$Drug=="Several Found!"),]
# adds dosage column in mg
df[,"Pill_dosage"] <- 0
for(a in seq(length(df[,1]))) {
      temp <- strsplit(df[a,"Content"],"mg")[[1]][1]
      temp <- strsplit(temp," ")
      mg <- temp[[1]][length(temp[[1]])]
      df[a,"Pill_dosage"] <- as.numeric(mg)
      
      if(length(strsplit(df[1,"Content"],"mg")[[1]]) > 2) {
        df[a,"Pill_dosage"] <- "Error : several dosage"
      }
      rm(temp)
}

# detects mention of hour in lines containing "mg"
df[,"ToD"] <- "" #Time of Day
df[,"ToDhd"] <- "" #Time of Day (half dosage), for ritaline IR
df[,"nbPhd"] <- 0 #Pill half dosage
df[,"nbP"] <- 0 #Pill 
for(a in seq(length(df[,1]))) {
  temp <- df[a,"Content"]
  lines <- strsplit(temp,"<br>")
  for(one_line in lines[[1]]) {
    if(grepl("mg", one_line,ignore.case=TRUE)){ # = if line contains "mg"
      words <- strsplit(one_line," ")
      for(one_word in words[[1]]) {
           if(grepl("[0-9]h", one_word, ignore.case=TRUE)) {
             if(!grepl("(5)",one_word,fixed=TRUE)) {df$ToD[a] <- paste(df$ToD[a], one_word,sep=" ") ; df[a,"nbP"] <- df[a,"nbP"]+1}
             if(grepl("(5)",one_word,fixed=TRUE)) {
               one_word <- gsub("(5)","",one_word,fixed=TRUE) # removes
               df$ToDhd[a] <- paste(df$ToDhd[a], one_word,sep=" ") ; df[a,"nbPhd"] <- df[a,"nbPhd"]+1
               }
          }
       }
    }
  }
}

# get total dosage :
df[,"Daily_dosage"] <- 0
for(a in seq(length(df[,1]))) {
  df[a,"Daily_dosage"] <- df[a,"nbPhd"]*5 + df[a,"nbP"]*df[a,"Pill_dosage"]
}

# get week day :
library(lubridate)
df[,"DoW"] <- "" #Day of the Week
for(a in seq(length(df[,1]))) {
  df[a,"DoW"] <- wday(label=TRUE,paste(sep = "-", df[a,"Year"],df[a,"Month"],df[a,"Day"]))
}


# detects mention of weight
df[,"Weight"] <- as.numeric(NA)
for(a in seq(length(df[,1]))) {
  temp <- df[a,"Content"]
  lines <- strsplit(temp,"<br>")
  for(one_line in lines[[1]]) {
    if(grepl("kg", one_line,ignore.case=TRUE)){ # = if line contains "kg"
      words <- strsplit(one_line," ")
      for(one_word in words[[1]]) {
        if(grepl("[0-9]+ ?kg", one_word, ignore.case=TRUE)) {
            one_word <- gsub(pattern = ",", replacement = ".",x = one_word, fixed = TRUE)
            df$Weight[a] <- gsub(pattern = "([0-9]+) ?kg", replacement = "\\1",x =  one_word,ignore.case = TRUE,fixed = FALSE)
        }
      }
    }
  }
}




View(df)

# todo :
# remove , from hours
# do plots


rm(truth) ; rm(a) ; rm(b) ; rm(firstRun) ; rm(com) ; rm(mg) ; rm(drugs)
rm(occ) ; rm(one_line) ; rm(one_word) ; rm(temp) ; rm(n)
rm(original_directory) ; rm(copy_directory) ; rm(file_hierarchy)
