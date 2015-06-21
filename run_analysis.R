run_analysis <- function(){
  library(dplyr)
  subject_train <- read.table("train/subject_train.txt")
  X_train <- read.table("train/X_train.txt")
  y_train <- read.table("train/y_train.txt")
  subject_test <- read.table("test/subject_test.txt")
  X_test <- read.table("test/X_test.txt")
  y_test <- read.table("test/y_test.txt")
  test <- cbind( subject_test, y_test, X_test)
  train <- cbind( subject_train, y_train, X_train)
  mergeset <-rbind(test, train) 
  column_names <- read.table("features.txt")
  column_names[,2] <- as.character(column_names[,2])
  colnames(mergeset)<-c("subjects","activity", column_names[,2])
  mergeset <- mergeset[ ,c(1:8, 43:48, 83:88, 123:128, 163:168, 203, 204, 216, 217, 229, 230, 242, 243, 
                           255, 256, 268:273, 347:352, 426:431, 505, 506, 518, 519, 531, 532, 544, 545 )]
  
  mergeset[,2] <- factor(mergeset[,2], levels=c(1:6), labels=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))
  tidy <- data.frame()
  for (i in 1: 30){
    temp <- filter(mergeset, subjects == i)
    for (j in 3:68) {
      temp2 <- as.data.frame(tapply(temp[ ,j], temp[,2], mean))
      colnames(temp2)[1] <- colnames(temp)[j]
      if (j == 3){
        tidy_temp <- temp2
      } else {
        tidy_temp<-cbind(tidy_temp, temp2)  
      }
    }
        
    tidy_temp$subject <- rep(i, 6)
    tidy_temp$activity <- c(1:6)
    tidy <- rbind(tidy, tidy_temp)
    
  }
  
  tidy$activity <- factor(tidy$activity, levels=c(1:6), labels=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))
  rownames(tidy) <- NULL
  write.table(tidy, file = "tidy_data.txt", row.name=FALSE) 
  
  }