getwd()
path <- 'C:/Users/user/Documents/liberia_data' #folder where all the csv files are stored
setwd(path)
filenames <- list.files()
df <- data.frame()
#rbind each file after formatting it correctly
for (filename in filenames){
  temp_df <- read.csv(filename, header = TRUE, stringsAsFactors=FALSE)
  if (nrow(temp_df) == 31 | nrow(temp_df) == 32){  
    if (nrow(temp_df) == 32) {
      temp_df <- temp_df[-32,]
    }
    date <- temp_df[1,1]
    temp_df <- temp_df[,-1]
    column_names <- temp_df$Variable
    temp_df <- as.data.frame(t(temp_df[,2]))  #only looking at National - 2nd column
    colnames(temp_df) <- column_names 
    temp_df <- temp_df[1,]
    temp_df <- cbind(date,temp_df)
    df <- rbind(df,temp_df)
    #print(filename)
  }  else { #only one file with nrow == 39
    print(filename)
    print(nrow(temp_df))  
    print("Needs to be manually modified")  

  }
  rm(temp_df) #for memory purposes, garbage cleaning 
}
#This is one file with different column names
filename = '2014-10-04-v142.csv'
temp_df <- read.csv(filename, header = TRUE, stringsAsFactors=FALSE)
date <- temp_df[1,1]
temp_df <- temp_df[,-1]
column_names <- temp_df$Variable
temp_df <- as.data.frame(t(temp_df[,2])) #only looking at National - 2nd column
colnames(temp_df) <- column_names 
temp_df <- temp_df[1,]
temp_df <- cbind(date,temp_df) #add date as first column
library('gtools')
df <- smartbind(df,temp_df) #Efficient rbind of data frames, even if the column names don't match
rm(temp_df) #for memory purposes, garbage cleaning 
df <- df[,1:32] #remove last 5 extra columns
rownames(df) <- NULL

#convert to date
## to give priority to %y format, define your own select_format function:
## ** how to use select_formats **
## By default %Y has precedence:
#parse_date_time(c("27-09-13", "27-09-2013"), "dmy")
## [1] "13-09-27 UTC"   "2013-09-27 UTC"

## to give priority to %y format, define your own select_format function:
my_select <-   function(trained){
  n_fmts <- nchar(gsub("[^%]", "", names(trained))) + grepl("%y", names(trained))*1.5
  names(trained[ which.max(n_fmts) ])
}
library(lubridate)
df$date <- parse_date_time(as.character(df$date), "mdy", select_formats = my_select)
## '[1] "2013-09-27 UTC" "2013-09-27 UTC"

#sort by date 
df <- df[order(df$date),]

write.csv(df, file = "liberia_clean.csv")


#rm(list=setdiff(ls(), "df")) #remove all variables except df

#Function from http://stackoverflow.com/questions/3402371/rbind-different-number-of-columns
### combines data frames (like rbind) but by matching column names
# columns without matches in the other data frame are still combined
# but with NA in the rows corresponding to the data frame without
# the variable
# A warning is issued if there is a type mismatch between columns of
# the same name and an attempt is made to combine the columns
# combineByName <- function(A,B) {
#   a.names <- names(A)
#   b.names <- names(B)
#   all.names <- union(a.names,b.names)
#   print(paste("Number of columns:",length(all.names)))
#   a.type <- NULL
#   for (i in 1:ncol(A)) {
#     a.type[i] <- typeof(A[,i])
#   }
#   b.type <- NULL
#   for (i in 1:ncol(B)) {
#     b.type[i] <- typeof(B[,i])
#   }
#   a_b.names <- names(A)[!names(A)%in%names(B)]
#   b_a.names <- names(B)[!names(B)%in%names(A)]
#   if (length(a_b.names)>0 | length(b_a.names)>0){
#     print("Columns in data frame A but not in data frame B:")
#     print(a_b.names)
#     print("Columns in data frame B but not in data frame A:")
#     print(b_a.names)
#   } else if(a.names==b.names & a.type==b.type){
#     C <- rbind(A,B)
#     return(C)
#   }
#   C <- list()
#   for(i in 1:length(all.names)) {
#     l.a <- all.names[i]%in%a.names
#     pos.a <- match(all.names[i],a.names)
#     typ.a <- a.type[pos.a]
#     l.b <- all.names[i]%in%b.names
#     pos.b <- match(all.names[i],b.names)
#     typ.b <- b.type[pos.b]
#     if(l.a & l.b) {
#       if(typ.a==typ.b) {
#         vec <- c(A[,pos.a],B[,pos.b])
#       } else {
#         warning(c("Type mismatch in variable named: ",all.names[i],"\n"))
#         vec <- try(c(A[,pos.a],B[,pos.b]))
#       }
#     } else if (l.a) {
#       vec <- c(A[,pos.a],rep(NA,nrow(B)))
#     } else {
#       vec <- c(rep(NA,nrow(A)),B[,pos.b])
#     }
#     C[[i]] <- vec
#   }
#   names(C) <- all.names
#   C <- as.data.frame(C)
#   return(C)
# }
