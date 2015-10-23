run_analysis <- function() {

# DOWNLOAD THE FILE AND EXTRACT TO ONE COMMON DIR: 
#         https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# YOU CAN WRITE A FUNCTION TO LOAD THIS DATA BUT I DID IT MANUALLY TO GET THE PROPER FILE SETUP I WANTED

################################################################################    
# GET THE FILES AND LOAD THEM TO MEMORY    
################################################################################
  
    # ACTIVITIES LABELS
    al <- read.table("./ProjectData/activity_labels.txt")
    # SUBJECT TEST
    st <- read.table("./ProjectData/subject_test.txt")
    # X TEST
    xt <- read.table("./ProjectData/X_test.txt")
    # Y TEST
    yt <- read.table("./ProjectData/Y_test.txt")
    # SUBJECT TRAIN
    sb <- read.table("./ProjectData/subject_train.txt")
    # X TRAIN
    xtr <- read.table("./ProjectData/X_train.txt")
    # Y TRAIN
    ytr <- read.table("./ProjectData/Y_train.txt")
    # FEATURES LIST
    f <- read.table("./ProjectData/features.txt")

################################################################################    
## MERGING OF TEST AND TRAIN DATA SETS
################################################################################  
    
    # PREVIOUS THREE MERGES MERGED INTO MASTER TABLE
    a <- cbind(rbind(sb, st), rbind(ytr, yt), rbind(xtr, xt))

################################################################################    
## ADD THE FEATURE HEADERS
################################################################################ 
    
    # IMPLICITLY DEFINE SUBJECT AND ACTIVITY HEADER THEN BRING IN LIST OF FEATURES FROM THE FEATUERS FILE
    colnames(a) <- c("SUB", "ACT", as.character(f[,2]))  

################################################################################    
## EXTRACT THE STD() AND MEAN()
################################################################################ 
    
    # FIND THE COLUMN INDEXES WHERE THE COL NAME CONTAINS "STD()" OR "MEAN()"
    colNums <- sort(c(grep("mean()", colnames(a)), grep("std()", colnames(a))))
    # FILTER EXISTING MERGED DATAFRAME TO THE COLUMN INDEXES IN PREVIOUS STEP. 
    # NOTE THAT COL 1 & 2 ARE HARD CODED (SUBJECT AND ACTIVITY)
    DF <- a[, c(1,2,colNums)]
    # REMOVE meanFREQ COLS SINCE PRIOR FILTERING MISSES THIS
    FINAL <- DF[, !grepl("Freq", colnames(DF))] 

################################################################################    
## GET THE OUTPUT
################################################################################

    # CREATE NEW DF
    OUTPUT <- data.frame()
    # MANUAL LOOP FOR 180 ROW (30 PATIENTS x 6 ACTIVITIES) ITERATONS
    for (i in 1:30) {
    # ONE PASS FOR EACH PATIENT
      s<- subset(FINAL,SUB==i)
    # LOOP FOR ACTIVITIES
      for (j in 1:6){
    # ONE PASS FOR EACH SUBJECT
        av<- subset(s, ACT==j)
    # GET THE AVERAGE AND THE MEAN
        mr<-as.vector(apply(av,2,mean))
    # CLEAN OUTPUT
        OUTPUT<-rbind(OUTPUT,mr)
    # END SUB LOOP
      }
    # END LOOP
    }

################################################################################    
## MAKE THE ACTIVITY LABELS USEFUL (REPLACE NUMERIC VALUES WITH TEXT DESCRIPTIONS)
################################################################################
    
    # GET ACTIVITY LABELS
    al_nums <- al[,2]
    # SET LOOKUP NAMES
    names(al_nums) <- al[,1]
    # SET COLNAMES FOR CLEAN OUTPUT DF
    colnames(OUTPUT)<-colnames(FINAL) 
    # UPDATE ACTIVITY NAMES TO SOMETHING USEFUL
    OUTPUT[,2] = al_nums[as.character(OUTPUT[,2])]  

################################################################################    
## STORE THE CLEAN OUTPUT
################################################################################    
    
    # STORE THE FILE
    write.table(OUTPUT, "final_cleaned.txt", sep = "", row.names = FALSE)
    # SAMPLE THE OUTPUT
    head(OUTPUT)
}