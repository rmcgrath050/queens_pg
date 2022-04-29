
library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(formattable)
library(data.table)
library(tidyverse)
library(shinyjs)

server = function(input,output, session){
  
  
  ###about page toggle buttons
  
  shinyjs::hide("rulesInfo")
  shinyjs::show("aboutInfo")
  
  observeEvent(input$about, {
    shinyjs::toggle("aboutInfo")
    shinyjs::hide("rulesInfo")
  })
  

  observeEvent(input$rules, {
    shinyjs::toggle("rulesInfo")
    # shinyjs::show("rulesInfo")
     shinyjs::hide("aboutInfo")
  })
  
  #attempt to disable the download buttons 
  
  # observeEvent({
  #   if(is.null(picked_data())){
  #     shinyjs::disable("downloadReport")
  #     shinyjs::disable("downloadPlot")
  #   } else {
  #     shinyjs: enable("downloadReport")
  #     shinyjs::disable("downloadPlot")
  #   }
  # })
 
  
  #setting up reactive values for output of plot 
  vals <- reactiveValues()
  
# ===============================================
# functions for file inputs
#================================================
  
  #all count as fails for student
  fails <- c("FAIL", "F", "ABS", "ABSM", "F*", "RN", "AUD", "MNA", "INC", "FE", "FAS", "ABSM", "ABSCA", "CONT",
             "FCA", "FE", "RNCA") ###double check is ATT a fail 
  
  #exceptional codes from the fails 
  exceptions <- c("ABS", "ABSM", "RN", "AUD", "MNA", "INC", "ABSM", "ABSCA", "RNCA", "CONT", "ATT")
  

  
  #getting current date for name download of files
  currentDate <- format(Sys.Date(), "%d-%m-%Y")
  
  
  
  ############################################################
  
  #read data for qsis function
  read_qsis_data <- function(qsis_data){
    


    qsis_df <- read.csv(qsis_data,  fill = TRUE, header = FALSE, sep = ",")
 
      
      #checking columns to make sure file is correct
      required_cols <- c('Last', 'ID', 'Subject', 'Catalog', 'Grade',
                        'Email', 'First Name', 'Sub Plan Descr', 'Plan Descr', 'Add Dt') 



    #making sure read in as dataframe
    qsis_df <- as.data.frame(qsis_df)
    
    #changing column names
    colnames(qsis_df) <- qsis_df[1,]
    
    #checking colnames to make sure they match for validation
    col_names <- colnames(qsis_df) 
    
    #making sure correct file is uploaded
     validate(
       need(required_cols %in% col_names, "Incorrect file type for Qsis details - please upload correct file"),
       #only accepts csv files on UI  - no need to check for CSV file type
     )

    #delete rubbish row header from dataframe
    qsis_df <- qsis_df[-c(1) , ]
    
    #changing sub plan col name to remove empty spaces 
    colnames(qsis_df)[2] <- "First_Name"
    colnames(qsis_df)[11] <- "Final"
    colnames(qsis_df)[12] <- "Classification"
    colnames(qsis_df)[26] <- "Plan_Descr"
    colnames(qsis_df)[28] <- "Sub_Plan_Descr"
    
    
    #changing classification to uppercase to avoid reading errors - checker for masters pathways 
    qsis_df$Plan_Descr <- toupper(qsis_df$Plan_Descr)
    
    
    
    return(qsis_df)
    
  }
  
  #reading in module results
  read_module_results <- function(module_data){
    
   #reading in csv file
    module_marks <- read.csv(module_data,  fill = TRUE , header = FALSE, sep = ",")
    
    #making sure read in file is a dataframe
    module_marks <- as.data.frame(module_marks)
    
    #changing column names to 7th row of file - headings placed here
    colnames(module_marks) <- module_marks[7,]
    
    col_names = colnames(module_marks) 
    
    
    required_cols <- c('Student', "Final") 
    
    
    validate(
      need(required_cols %in% col_names, "Incorrect file type for module details - please recheck files uploaded 
           for modules"),
      #only accepts csv files on UI  - no need to check for CSV file type
    )
    
    #changing student ID to unique name
    colnames(module_marks)[1] <- "ID"
    
    #changing unnamed column needed from module marks
    colnames(module_marks)[5] <- "Classification"
    
    
    
    
    #getting the important/needed fields from data
    module_marks <- module_marks[ , c("ID", "Student", "Classification", "Final")]
    
    
    #delete rubbish rows from dataframes
    module_marks <- module_marks[-c(1:9) , ]
    
    
    #changing classification to uppercase to avoid reading errors
    module_marks$Classification <- toupper(module_marks$Classification)

    #removing rows with empty values to remove waste data - not needed 
    #module_marks <- na.omit(module_marks)
    
    return(module_marks)
    
  }
  
  #updating the final classification (marks) to 0 for students registered - exceptions
  updating_exceptions <- function(mark, grade){
    
    ##checking exceptions as some may have no marks
    #exceptions <- c("ABS", "ABSM", "RN", "AUD", "MNA", "INC", "ABSM", "CONT", "ABSCA")
    
    
    if(grade %in% exceptions && mark == ""){
      mark= "0"
    } else {
      mark = mark
    }
    
    return(mark)
  }   
  
  #function to work out mod fails per each student 
  mods_fail_total <- function(grades) {
    
    #failed mods
    fail_count = 0
    
    # fail codes
    #fails <- c("FAIL", "F", "ABS", "ABSM", "F*", "RN", "Aud", "MNA", "INC", "FE", "FAS", "ABSM", "ABSCA")
    
    
    #loop through df by grades per a student 
    for (i in 1:length(grades)) {
      if (grades[i] %in% fails) {
        
        #calculating fails
        fail_count = fail_count + 1
      }
    }
    
    return(fail_count)
    
  }
  
  #year average per student
  year_average <- function(marks, grades){
    
    mods = 0
    total = 0
    year_average = 0
    
    #removing NA values from dataframe for calculation purposes
    marks <- marks %>% discard(~all(is.na(.) | . ==""))
    grades <- grades  %>% discard(~all(is.na(.) | . ==""))
    
    #if marks df exists but has no data
    if (dim(marks)[2] == 0) {
      
      return (year_average)
      
      
    } else {
      
      #if marks df has columns but no observations.
      if (dim(marks)[1] == 0) {
        
        return (year_average)
        
        
      } else {
        
        #marks calaulcation for year average
        for(x in 1:ncol(marks)){
          
          mods = mods + 1
          
          for(x in 1:ncol(grades)){
            
            
            if(grades[x] == "PH"){
              marks[x] = 40
            } else {
              marks[x] = as.numeric(marks[x])
              total = total + marks[x]
            }
          }
          
          #testing print outs work
          # print(marks[x])
          #print(mods)
          
          
        }
        
        
      }
      
      
      
      
      
      total = sum(marks)
      #print(total)
      
      
      
      
      #ceiling function rounds year average UP
      year_average = ceiling(total/ mods)
      
      
      #floor can replace ceiling to round marks down if preferred 
      # year_average = floor(total/ mods)
      
      
      #returns average for the year
      return(round(year_average))
      
    }
    
  }
  
  #function for meeting BIT students
  invite_meeting_bit <- function(progression_status, grades) {
    
    #count exceptions / fails
    #exceptions <- c("ABS", "ABSM", "F*", "RN", "AUD", "MNA", "INC", "FE", "FAS", "ABSM", "F")
    
    exceptions_count = 0
    
    #counting how many exceptions/fails student has 
    for(i in 1:length(grades)){
      if(grades[i] %in% exceptions){
        exceptions_count = exceptions_count + 1
      }  
    } 
    
    
    meeting <- case_when(
      #if progression status is no meeting is automatically yes - this would have already considered failed grades 
      progression_status == "No" ~ "Yes",
      
      #if progression status yes but a fail / exceptional grade returned - letter sent 
      progression_status == "Yes" && exceptions_count > 0 ~ "Letter of concern",
      
      #if progression status is yes with no exceptions/fails to consider 
      TRUE ~ "No"
    )
    
    return(meeting)
    
  }
  
  #function invite meeting other pathways which have MEng students
  invite_meeting_cs <- function(progression_status, grades, pathway, year_avg) {
    
    #count exceptions / fails
    #exceptions <- c("ABS", "ABSM", "F*", "RN", "AUD", "MNA", "INC", "FE", "FAS", "ABSM", "F")
    
    exceptions_count = 0
    
    #working out has MEng student passed year average
    master_codes <- toupper(c("MEng (UM) Computer Science", "MEng (UM) Software Engineer P", "MEng (UM) Software Engineering"))
    if(pathway %in% master_codes && year_avg < 60) { 
      pathway = "failed"
    } else {
      pathway = "passed"
    }
    
    
    #calculating exceptions/fails per student 
    for(i in 1:length(grades)){
      if(grades[i] %in% exceptions){
        exceptions_count = exceptions_count + 1
      }  
    } 
    
    
    meeting <- case_when(
      
      #if progression status is no meeting is automatically yes
      progression_status == "No" ~ "Yes",
      
      #if progression status yes but some fails / exceptional grades returned - letter sent 
      progression_status == "Yes" && exceptions_count > 0 ~ "Letter of concern",
      
      #if MEng student < 60% - letter
      pathway == "failed" ~ "Letter of concern",
      
      #if progression status is yes with no exceptions/fails to consider 
      TRUE ~ "No"
    )
    
    return(meeting)
    
  }
  
  #progression for all
  progression <- function(fail_cats) {
    
    progression = case_when(
      fail_cats >= 40  ~ "No",
      #fail_cats == 20 ~ "Letter of concern",
      TRUE ~ "Yes" 
    ) 
    
    
    return(progression)
  }
  
  ###################################################
  
  # Comments
  
  ################################################

  #semester one only
  comments_sem1 <- function(failed_cats, grades, pathway, year_avg, course) {
    
    #getting amount of times student was absent in modules registered 
    absent_count <- 0 
    absent_codes <- c("ABS", "ABSM")
    
    master_codes <- toupper(c("MEng (UM) Computer Science", "MEng (UM) Software Engineer P", "MEng (UM) Software Engineering"))
    
    
    mods_taken = 0 
    
    # adding modules taken and + how many ABS exams per student
    for(i in 1:length(grades)){
      mods_taken = mods_taken + 1
      
      if(grades[i] %in% absent_codes){
        absent_count = absent_count + 1
      }
    }
    
    
    
    
    
    comment <- case_when(
      
      ###############
      # Meeting 
      ###############
      
      #more than one or more failed grades due to absent comments
      absent_count >=3 ~ "At least three Abs/AbsM grades - inviting to SSM",
      absent_count ==2 ~ "Two Abs/AbsM grades - inviting to SSM",
      
      # 1 fail and 1 absent grade
      absent_count ==1 && failed_cats == 40 ~ "One failed grade & Abs/AbsM - inviting to SSM",
      
      #if 40 or more cats failed
      failed_cats >= 40 ~ "At least two failed grades - inviting to SSM",
      
      
      ###############
      # Letter
      ###############
      
      #one absent grade only
      absent_count ==1 && failed_cats ==20 ~ "One Abs/AbsM grade - Sending letter of concern",
      
      #if 20 cats failed 
      failed_cats == 20 ~ "One failed grade - Take in August or next AY - Sending letter of concern",
      
      #ACC1022 + MEGT1009 - core mods
      course == "BIT" && mods_taken < 2 ~  "Two core modules must be taken in! Sending letter of concern",
      
      #Semester 2 CS core mods - csc1023, (wuthout ssd) csc1029 + 1 optional
      course == "CS" && mods_taken < 3  ~  "Three core modules must be taken! Sending letter of concern",
      TRUE ~ " "
      
    )
    
    
    #reading in pathway and checking if its MEng and the year average for the student
    if(pathway %in% master_codes && year_avg < 60) { 
      pathway = "failed"
    } else {
      pathway = "passed"
    }
    
    
    #if failed concatenate this comment to the return comment
    if(pathway == "failed") {
      comment = paste(comment, "MEng progression requires more than 60% average", sep = "* ")
      
    }
    
    
    return(comment)
    
    
  }
  
  #semester 2 + can be used sem1 + sem2 table results together
  comments_sem2 <- function(failed_cats, grades, pathway, year_avg) {
    
    absent_count <- 0 
    absent_codes <- c("ABS", "ABSM")
    
    master_codes <- toupper(c("MEng (UM) Computer Science", "MEng (UM) Software Engineer P", "MEng (UM) Software Engineering"))
    
    
    mods_taken = 0 
    
    for(i in 1:length(grades)){
      mods_taken = mods_taken + 1
      
      if(grades[i] %in% absent_codes){
        absent_count = absent_count + 1
      }
    }
    

    comment <- case_when(
      
      ###############
      # Meeting 
      ###############
      
      #more than one or more failed grades due to absent comments
  
      #if 40 or more cats failed
      
      absent_count >=3 ~ "At least three Abs/AbsM grades - inviting to SSM",
      absent_count ==2 ~ "Two Abs/AbsM grades - inviting to SSM",
      absent_count ==1 && failed_cats >= 60 ~ "At least two failed grades & Abs/AbsM - inviting to SSM",
      absent_count ==1 && failed_cats == 40 ~ "One failed grade & Abs/AbsM - inviting to SSM",
      
      failed_cats >= 40 ~ "At least two failed grades - inviting to SSM",
      
      
      ###############
      # Meeting 
      ###############
      
      absent_count ==1 && failed_cats == 20 ~ "One Abs/AbsM grade - Sending letter of concern",
      failed_cats == 20 ~ "One failed grade - Sending letter of concern",
      
      
      #semester 2 CS core mods - csc1023, + 2 optional - depending on with/without SSD
      #semester 2 BIT core mods - csc1023, csc1024 + 1 optional
      mods_taken < 3  ~  "Three core modules must be taken! Sending letter of concern",
      TRUE ~ " "
      
    )
    
    #reading in pathway and checking if its MEng and the year average for the student
    if(pathway %in% master_codes && year_avg < 60) { 
      pathway = "failed"
    } else {
      pathway = "passed"
    }
    
    
    #if failed concatenate this comment to the return comment
    if(pathway == "failed") {
      comment = paste(comment, "MEng progression requires more than 60% average", sep = "* ")
      
    }
    
    
    return(comment)
    
  }
  
  #semester 3 final comments - 
  comments_final <- function(failed_cats, grades, pathway, year_avg, course) {
    
    #exceptional grades
    #exceptions <- c("ABS", "ABSM", "RN", "AUD", "MNA", "INC", "ABSM")
    
    #codes for MEng pathways 
    master_codes <- toupper(c("MEng (UM) Computer Science", "MEng (UM) Software Engineer P", "MEng (UM) Software Engineering"))
    

    mods_taken = 0
    
    #loop through students grade to analyse their results
    for(i in 1:length(grades)){
      
      #modules taken 
      mods_taken = mods_taken + 1

    }

    comment <- case_when(
      
      #pathway == "failed" ~ "Meng progression requires more than 60%  year average - Invite to SSM",
      #3 failed for BIT - 4 failed for other pathways 
      failed_cats >= 100 ~ "Invited to S1 SSM, recommend retake next AY",
      failed_cats >= 80  ~ "Invited to S1 SSM – Must pass another 60 CATS to progress to stage 2",
      failed_cats >= 60 ~ "Invited to S1 SSM – Pass two in August to progress or take next AY",
      failed_cats >= 40 ~ "Invited to S1 SSM,  Pass one in August to progress or take next AY",
      
      #one failed grade for student
      failed_cats == 20 ~ "Take in August or next AY",
      
      #not sure if this is still a check in s3? 
      course == "BIT" && mods_taken < 5 ~  "Five core modules must be taken in! Sending letter of concern",
      course == "CS" && mods_taken < 6 ~  "Six core modules must be taken! Sending letter of concern",
      TRUE ~ " "
    )

    
    if(pathway %in% master_codes && year_avg <=60) { #&& year_avg <60
      pathway = "failed"
    } else {
      pathway = "passed"
    }
    
    
    
    #if failed concatenate this comment to the return comment
    if(pathway == "failed") {
      comment = paste(comment, "MEng progression requires more than 60% average", sep = "* ")
      
    }
    
    
    return(comment)

  }

  
  #############################################
  
  #BIT only functions
  
  #############################################
  
  #semester 2 BIT students function 
  cat_fails_bit <- function(grades){
    
    fail_count = 0
    
    #fails <- c("FAIL", "F", "ABS", "ABSM", "F*", "RN", "AUD", "MNA", "INC", "FE", "FAS", "ABSM", "ABSCA")
    
    for (i in 1:length(grades)) {
      if(grades[i] %in% fails){
        fail_count = fail_count + 1
      } 
      
      
      
      #adding an extra fail for this mod worth 40 cats 
      if ("Grade-S2-CSC1024" %in% colnames(grades)){
        if (grades$`Grade-S2-CSC1024`[i] %in% fails) {
          fail_count = fail_count + 1
        } 
      }
      
      
    }
    
    cats_fail_total = fail_count *20 
    
    
    return(cats_fail_total)
    
  }

  #=============================================
  # Reading in progression tables
  #=============================================  
  
  ###### Semester 1
  

  #Semester one CS_SE_CIT Pathway results
  sem1_CS_path <- function(qsis_data, csc1022, csc1025, csc1026, csc1027){
    
    #finding out how many students there is for S1 - returns a Integer value
    num_of_students <- length(unique(qsis_data$ID)) 
    
    #dataframe of unique student details - returns data of unique students
    each_student <- distinct(qsis_data , ID, .keep_all = TRUE)
    
    
    #creating empty output table
    Sem1_output <- data.frame(matrix(data = NA, nrow = num_of_students , ncol = 20))
    
    #work out amount of columns for s1 - naming relevant fields to output
    colnames(Sem1_output)[1:3] <- colnames(qsis_data)[c(1,2,3)]
    colnames(Sem1_output)[4:5] <- c("20-S1-CSC1022","Grade-S1-CSC1022")
    colnames(Sem1_output)[6:7] <- c("20-S1-CSC1025","Grade-S1-CSC1025")
    colnames(Sem1_output)[8:9] <- c("20-S1-CSC1026","Grade-S1-CSC1026")
    colnames(Sem1_output)[10:11] <- c("20-S1-CSC1027","Grade-S1-CSC1027")
    colnames(Sem1_output)[12:14] <- colnames(qsis_data)[c(19,26,28)]
    colnames(Sem1_output)[15:20] <- c("Module-Fail","CAT-Fail","Year_Avg","Progressed?","Comments","Invite To SSM")
    
    #reading in results and working out backend functions
    for (i in 1:num_of_students) {
      
      Sem1_output$Last[i] <- each_student$Last[i] 
      Sem1_output$First_Name[i] <- each_student$First_Name[i]
      Sem1_output$ID[i] <- each_student$ID[i]
      
      for (j in 1:nrow(csc1022)){
        if(Sem1_output$ID[i] == csc1022$ID[j]){
          #registered students on course returns 0 for no marks present on test data
          Sem1_output$`Grade-S1-CSC1022`[i] <- csc1022$Classification[j]
          Sem1_output$`20-S1-CSC1022`[i] <- updating_exceptions(csc1022$Final[j],csc1022$Classification[j] )
          
        }
      }
      
      for (j in 1:nrow(csc1025)){
        if(Sem1_output$ID[i] == csc1025$ID[j]){
          Sem1_output$`Grade-S1-CSC1025`[i] <- csc1025$Classification[j]
          Sem1_output$`20-S1-CSC1025`[i] <- updating_exceptions(csc1025$Final[j],  csc1025$Classification[j])
          
          
        }
      }
      
      for (j in 1:nrow(csc1026)){
        if(Sem1_output$ID[i] == csc1026$ID[j]){
          Sem1_output$`Grade-S1-CSC1026`[i] <- csc1026$Classification[j]
          #registered students on course returns 0 for no marks present on test data
          Sem1_output$`20-S1-CSC1026`[i] <- updating_exceptions(csc1026$Final[j],
                                                                csc1026$Classification[j])
          
          
        }
      }
      
      
      for (j in 1:nrow(csc1027)){
        if(Sem1_output$ID[i] == csc1027$ID[j]){
          Sem1_output$`Grade-S1-CSC1027`[i] <- csc1027$Classification[j]
          Sem1_output$`20-S1-CSC1027`[i] <- updating_exceptions(csc1027$Final[j],
                                                                csc1027$Classification[j])
          
        }
      }
      
      #Sem1_output$mods_fail_total$Module-Fail(i)
      #changing pathway to lower to compare for masters students after
      Sem1_output$Email[i] <- each_student$Email[i]
      Sem1_output$Plan_Descr[i] <- each_student$Plan_Descr[i]
      Sem1_output$Sub_Plan_Descr[i] <- each_student$Sub_Plan_Descr[i]
      
      #pipeline of grades for each student
      subset_grades <- select(Sem1_output[i, ], 5,7,9,11) 
      
      #MIGHT NEED TO CHANGE TO AS NUMERIC IN FUNCTION - BLANK VALUES ACCOUNT FOR 22/02
      subset_marks <- select(Sem1_output[i,], 4,6,8,10) 
      
      #reading row of student grades into function to work out mods failed
      Sem1_output$`Module-Fail`[i] <- mods_fail_total(subset_grades)
      
      #Calculating how many cats failed with mods_fail_total function
      Sem1_output$`CAT-Fail`[i] <- mods_fail_total(subset_grades) * 20
      
      #working out year average for each student with year_average function
      Sem1_output$Year_Avg[i] <- year_average(subset_marks, subset_grades)
      
      #working out progression with progression function
      Sem1_output$`Progressed?`[i] <- progression(Sem1_output$`CAT-Fail`[i])
      
      Sem1_output$Comments[i] <- comments_sem1(Sem1_output$`CAT-Fail`[i],
                                               subset_grades,Sem1_output$Plan_Descr[i],Sem1_output$Year_Avg[i], "CS")
      
      Sem1_output$`Invite To SSM`[i] <- invite_meeting_cs(Sem1_output$`Progressed?`[i],subset_grades,
                                                          Sem1_output$Plan_Descr[i], Sem1_output$Year_Avg[i])
    }
    
    return(Sem1_output)
    
  } 
  
  # #Semester one BIT Pathway results  - ADD TO UI
  sem1_BIT_path <- function(qsis_data, acc1022, mgt1009){
    
    
    #finding out how many students there is for S1
    num_of_students <- length(unique(qsis_data$ID))
    
    #getting BIT QSIS distinct student details
    each_student <- distinct(qsis_data , ID, .keep_all = TRUE)
    
    #creating empty output table
    Sem1_output <- data.frame(matrix(data = NA, nrow = num_of_students , ncol = 16))
    
    
    #work out amount of columns for s1 - naming relevant fields to output
    colnames(Sem1_output)[1:3] <- colnames(qsis_data)[c(1,2,3)]
    colnames(Sem1_output)[4:5] <- c("20-S1-ACC1022","Grade-S1-ACC1022")
    colnames(Sem1_output)[6:7] <- c("20-S1-MGT1009","Grade-S1-MGT1009")
    colnames(Sem1_output)[8:10] <- colnames(qsis_data)[c(19,26,28)]
    colnames(Sem1_output)[11:16] <- c("Module-Fail","CAT-Fail","Year_Avg","Progressed?","Comments","Invite To SSM")
    
    
    #reading in results and working out backend functions
    for (i in 1:num_of_students) {
      
      Sem1_output$Last[i] <- each_student$Last[i]
      Sem1_output$First_Name[i] <- each_student$First_Name[i]
      Sem1_output$ID[i] <- each_student$ID[i]
      
      for (j in 1:nrow(acc1022)){
        if(Sem1_output$ID[i] == acc1022$ID[j]){
          Sem1_output$`Grade-S1-ACC1022`[i] <- acc1022$Classification[j]
          #registered students on course returns 0 for no marks present on test data
          Sem1_output$`20-S1-ACC1022`[i] <- updating_exceptions(acc1022$Final[j],
                                                                Sem1_output$`Grade-S1-ACC1022`[i])
          
        }
      }
      
      
      
      for (j in 1:nrow(mgt1009)){
        if(Sem1_output$ID[i] == mgt1009$ID[j]){
          Sem1_output$`Grade-S1-MGT1009`[i] <- mgt1009$Classification[j]
          #registered students on course returns 0 for no marks present on test data
          Sem1_output$`20-S1-MGT1009`[i] <- updating_exceptions(mgt1009$Final[j],
                                                                Sem1_output$`Grade-S1-MGT1009`[i] )
          
          
        }
      }
      
      
      
      #Sem1_output$mods_fail_total$Module-Fail(i)
      
      Sem1_output$Email[i] <- each_student$Email[i]
      Sem1_output$Plan_Descr[i] <- each_student$Plan_Descr[i]
      Sem1_output$Sub_Plan_Descr[i] <- each_student$Sub_Plan_Descr[i]
      
      
      
      #pipeline of grades for each student
      subset_grades <- select(Sem1_output[i, ], 5,7) 
      
      
      
      #MIGHT NEED TO CHANGE TO AS NUMERIC IN FUNCTION - BLANK VALUES ACCOUNT FOR 22/02
      subset_marks <- select(Sem1_output[i,], 4,6) 
      
      #reading row of student grades into function to work out mods failed
      Sem1_output$`Module-Fail`[i] <- mods_fail_total(subset_grades)
      
      
      
      #Calculating how many cats failed with mods_fail_total function
      Sem1_output$`CAT-Fail`[i] <- cat_fails_bit(subset_grades)
      
      
      
      #working out year average for each student with year_average function
      Sem1_output$Year_Avg[i] <- year_average(subset_marks, subset_grades)
      
      
      
      #working out progression with progression function
      Sem1_output$`Progressed?`[i] <- progression(Sem1_output$`CAT-Fail`[i])
      
      Sem1_output$Comments[i] <- comments_sem1(Sem1_output$`CAT-Fail`[i], subset_grades,
                                               Sem1_output$Plan_Descr[i], Sem1_output$Year_Avg[i], "BIT" )
      
      Sem1_output$`Invite To SSM`[i] <- invite_meeting_bit(Sem1_output$`Progressed?`[i], subset_grades)
      
      
    }
    
    
    return(Sem1_output)
    
  }
  

  ###### Semester 2
  
  ###semester two pathway results
  sem2_CS_path <- function(qsis_data, csc1023, csc1028, csc1029, csc1030, csc1031){
    
    
    #finding out how many students there is for S2
    num_of_students <- length(unique(qsis_data$ID))
    
    #dataframe of unique student details - returns dataframe of unique students
    each_student <- distinct(qsis_data , ID, .keep_all = TRUE)
    
    
    #creating empty output table
    Sem2_output <- data.frame(matrix(data = NA, nrow = num_of_students , ncol = 22))
    
    #work out amount of columns for s1 - naming relevant fields to output
    colnames(Sem2_output)[1:3] <- colnames(qsis_data)[c(1,2,3)]
    colnames(Sem2_output)[4:5] <- c("20-S2-CSC1023","Grade-S2-CSC1023")
    colnames(Sem2_output)[6:7] <- c("20-S2-CSC1028","Grade-S2-CSC1028")
    colnames(Sem2_output)[8:9] <- c("20-S2-CSC1029","Grade-S2-CSC1029")
    colnames(Sem2_output)[10:11] <- c("20-S2-CSC1030","Grade-S2-CSC1030")
    colnames(Sem2_output)[12:13] <- c("20-S2-CSC1031","Grade-S2-CSC1031")
    
    #this collects the Term year, email, pathway and sub plan description
    colnames(Sem2_output)[14:16] <- colnames(qsis_data)[c(19,26,28)]
    colnames(Sem2_output)[17:22] <- c("Module-Fail","CAT-Fail","Year_Avg","Progressed?","Comments","Invite To SSM")
    
    
    
    #reading in results and working out backend functions
    for (i in 1:num_of_students) {
      
      Sem2_output$Last[i] <- each_student$Last[i]
      Sem2_output$First_Name[i] <- each_student$First_Name[i]
      Sem2_output$ID[i] <- each_student$ID[i]
      
      
      ######################################
      #Reading in uploaded files next - classifcation should be upper from read in 
      #######################################
      
      for (j in 1:nrow(csc1023)){
        if(Sem2_output$ID[i] == csc1023$ID[j]){
          Sem2_output$`Grade-S2-CSC1023`[i] <- csc1023$Classification[j] 
          Sem2_output$`20-S2-CSC1023`[i] <- updating_exceptions(csc1023$Final[j], Sem2_output$`Grade-S2-CSC1023`[i])
        }
      }
      
      
      for (j in 1:nrow(csc1028)){
        if(Sem2_output$ID[i] == csc1028$ID[j]){
          Sem2_output$`Grade-S2-CSC1028`[i] <- csc1028$Classification[j]
          Sem2_output$`20-S2-CSC1028`[i] <- updating_exceptions(csc1028$Final[j], Sem2_output$`Grade-S2-CSC1028`[i])
        }
      }
      
      for (j in 1:nrow(csc1029)){
        if(Sem2_output$ID[i] == csc1029$ID[j]){
          Sem2_output$`Grade-S2-CSC1029`[i] <- csc1029$Classification[j]
          Sem2_output$`20-S2-CSC1029`[i] <- updating_exceptions(csc1029$Final[j], Sem2_output$`Grade-S2-CSC1029`[i])
        }
      }
      
      
      for (j in 1:nrow(csc1030)){
        if(Sem2_output$ID[i] == csc1030$ID[j]){
          Sem2_output$`Grade-S2-CSC1030`[i] <- csc1030$Classification[j]
          Sem2_output$`20-S2-CSC1030`[i] <- updating_exceptions(csc1030$Final[j], Sem2_output$`Grade-S2-CSC1030`[i])
          
        }
      }
      
      for (j in 1:nrow(csc1031)){
        if(Sem2_output$ID[i] == csc1031$ID[j]){
          Sem2_output$`Grade-S2-CSC1031`[i] <- csc1031$Classification[j]
          Sem2_output$`20-S2-CSC1031`[i] <- updating_exceptions(csc1031$Final[j],Sem2_output$`Grade-S2-CSC1031`[i])
        }
      }
      
      
      Sem2_output$Email[i] <- each_student$Email[i]
      Sem2_output$Plan_Descr[i] <- each_student$Plan_Descr[i]
      Sem2_output$Sub_Plan_Descr[i] <- each_student$Sub_Plan_Descr[i]
      
      
      #pipeline of grades for each student
      subset_grades <- select(Sem2_output[i, ], 5,7,9,11,13) #%>% discard(~all(is.na(.) | . ==""))
      
      #tidyverse discard function
      subset_marks <- select(Sem2_output[i,], 4,6,8,10,12) # %>% discard(~all(is.na(.) | . ==""))
      
      
      
      #reading row of student grades into function to work out mods failed
      Sem2_output$`Module-Fail`[i] <- mods_fail_total(subset_grades)
      
      #Calculating how many cats failed with mods_fail_total function
      Sem2_output$`CAT-Fail`[i] <- mods_fail_total(subset_grades) * 20
      
      #working out year average for each student with year_average function
      Sem2_output$Year_Avg[i] <- year_average(subset_marks,subset_grades)
      
      #working out progression with progression function
      Sem2_output$`Progressed?`[i] <- progression(Sem2_output$`CAT-Fail`[i]
                                                  
      )
      
      Sem2_output$Comments[i] <- comments_sem2(Sem2_output$`CAT-Fail`[i],
                                               subset_grades,
                                               Sem2_output$Plan_Descr[i] ,
                                               Sem2_output$Year_Avg[i]
      )
      
      Sem2_output$`Invite To SSM`[i] <- invite_meeting_cs(Sem2_output$`Progressed?`[i],subset_grades, Sem2_output$Plan_Descr[i],
                                                          Sem2_output$Year_Avg)
      
      
      
    }
    # }
    
    return(Sem2_output)
  }  
  
  
  sem2_BIT_path <- function(qsis_data, csc1023, csc1024, eco1007, mgt1012, mgt1013){
    
    #finding out how many students there is for S1 - returns a Integer value
    num_of_students <- length(unique(qsis_data$ID))
    
    #dataframe of unique student details - returns data of unique students
    each_student <- distinct(qsis_data , ID, .keep_all = TRUE)
    
    
    #finding out how many students there is for S1 - returns a Integer value
    num_of_students <- length(unique(qsis_data$ID))
    
    #dataframe of unique student details - returns data of unique students
    each_student <- distinct(qsis_data , ID, .keep_all = TRUE)
    
    #creating empty output table
    Sem2_output <- data.frame(matrix(data = NA, nrow = num_of_students , ncol = 22))
    
    #work out amount of columns for s1 - naming relevant fields to output
    colnames(Sem2_output)[1:3] <- colnames(qsis_data)[c(1,2,3)]
    colnames(Sem2_output)[4:5] <- c("20-S2-CSC1023","Grade-S2-CSC1023")
    colnames(Sem2_output)[6:7] <- c("20-S2-CSC1024","Grade-S2-CSC1024")
    colnames(Sem2_output)[8:9] <- c("20-S2-ECO1007","Grade-S2-ECO1007")
    colnames(Sem2_output)[10:11] <- c("20-S2-MGT1012","Grade-S2-MGT1012")
    colnames(Sem2_output)[12:13] <- c("20-S2-MGT1013","Grade-S2-MGT1013")
    colnames(Sem2_output)[14:16] <- colnames(qsis_data)[c(19,26,28)]
    colnames(Sem2_output)[17:22] <- c("Module-Fail","CAT-Fail","Year_Avg","Progressed?","Comments","Invite To SSM")
    
    
    #reading in results and working out backend functions
    for (i in 1:num_of_students) {
      
      Sem2_output$Last[i] <- each_student$Last[i]
      Sem2_output$First_Name[i] <- each_student$First_Name[i]
      Sem2_output$ID[i] <- each_student$ID[i]
      
      
      for (j in 1:nrow(csc1023)){
        if(Sem2_output$ID[i] == csc1023$ID[j]){
          Sem2_output$`Grade-S2-CSC1023`[i] <- csc1023$Classification[j]
          #registered students on course returns 0 for no marks present on test data
          Sem2_output$`20-S2-CSC1023`[i] <- updating_exceptions(csc1023$Final[j],
                                                                Sem2_output$`Grade-S2-CSC1023`[i])
          
          
        }
      }
      
      
      for (j in 1:nrow(csc1024)){
        if(Sem2_output$ID[i] == csc1024$ID[j]){
          Sem2_output$`Grade-S2-CSC1024`[i] <- csc1024$Classification[j]
          Sem2_output$`20-S2-CSC1024`[i] <- updating_exceptions(csc1024$Final[j],
                                                                Sem2_output$`Grade-S2-CSC1024`[i])
          
        }
      }
      
      
      for (j in 1:nrow(eco1007)){
        if(Sem2_output$ID[i] == eco1007$ID[j]){
          Sem2_output$`Grade-S2-ECO1007`[i] <- eco1007$Classification[j]
          Sem2_output$`20-S2-ECO1007`[i] <- updating_exceptions(eco1007$Final[j],
                                                                Sem2_output$`Grade-S2-ECO1007`[i])
          
        }
      }
      
      
      for (j in 1:nrow(mgt1012)){
        if(Sem2_output$ID[i] == mgt1012$ID[j]){
          Sem2_output$`Grade-S2-MGT1012`[i] <- mgt1012$Classification[j]
          Sem2_output$`20-S2-MGT1012`[i] <- updating_exceptions(mgt1012$Final[j],
                                                                Sem2_output$`Grade-S2-MGT1012`[i])
          
        }
      }
      
      for (j in 1:nrow(mgt1013)){
        if(Sem2_output$ID[i] == mgt1013$ID[j]){
          Sem2_output$`Grade-S2-MGT1013`[i] <- mgt1013$Classification[j]
          Sem2_output$`20-S2-MGT1013`[i] <- updating_exceptions(mgt1013$Final[j],
                                                                Sem2_output$`Grade-S2-MGT1013`[i])
          
        }
      }
      
      Sem2_output$Email[i] <- each_student$Email[i]
      Sem2_output$Plan_Descr[i] <- each_student$Plan_Descr[i]
      Sem2_output$Sub_Plan_Descr[i] <- each_student$Sub_Plan_Descr[i]
      
      
      #pipeline of grades for each student
      subset_grades <- select(Sem2_output[i, ], 5,7,9,11,13)  
      #subset_csc1024 <- select(Sem2_output[i,], 11)
      
      # #pipeline of marks for each student
      subset_marks <- select(Sem2_output[i,], 4,6,8,10,12)  
      
      #reading row of student grades into function to work out mods failed
      Sem2_output$`Module-Fail`[i] <- mods_fail_total(subset_grades)
      
      #Calculating how many cats failed with mods_fail_total function
      Sem2_output$`CAT-Fail`[i] <- cat_fails_bit(subset_grades)
      
      #working out year average for each student with year_average function
      Sem2_output$Year_Avg[i] <- year_average(subset_marks, subset_grades)
      
      
      #working out progression with progression function
      Sem2_output$`Progressed?`[i] <-
        progression(
          Sem2_output$`CAT-Fail`[i]
        )
      
      
      Sem2_output$Comments[i] <- comments_sem2(
        Sem2_output$`CAT-Fail`[i],
        subset_grades,
        Sem2_output$Plan_Descr[i],
        Sem2_output$Year_Avg[i]
      )
      
      Sem2_output$`Invite To SSM`[i] <-
        invite_meeting_bit(Sem2_output$`Progressed?`[i], subset_grades)
      
    }
    
    
    return(Sem2_output)
    
  }
  
  
  ###### Semester 1 + 2

  #Semester one +  two CS_SE_CIT Pathway results
  sem1_2_CS_path <- function(qsis_data, csc1023, csc1028, csc1029, csc1030, csc1031){
    
    
    #finding out how many students there is for S1 - returns a Integer value
    num_of_students <- length(unique(qsis_data$ID))
    
    #dataframe of unique student details - returns data of unique students
    each_student <- distinct(qsis_data , ID, .keep_all = TRUE)
    
    #grades of s1
    csc1022 <- qsis_data %>% select(ID, Catalog, Final, Classification) %>% filter(Catalog == 1022)
    csc1025 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1025)
    csc1026 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1026)
    csc1027 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1027)
    
    #creating empty output table
    Sem2_output <- data.frame(matrix(data = NA, nrow = num_of_students , ncol = 30))
    
    #work out amount of columns for s1 - naming relevant fields to output
    colnames(Sem2_output)[1:3] <- colnames(qsis_data)[c(1,2,3)]
    colnames(Sem2_output)[4:5] <- c("20-S1-CSC1022","Grade-S1-CSC1022")
    colnames(Sem2_output)[6:7] <- c("20-S1-CSC1025","Grade-S1-CSC1025")
    colnames(Sem2_output)[8:9] <- c("20-S1-CSC1026","Grade-S1-CSC1026")
    colnames(Sem2_output)[10:11] <- c("20-S1-CSC1027","Grade-S1-CSC1027")
    colnames(Sem2_output)[12:13] <- c("20-S2-CSC1023","Grade-S2-CSC1023")
    colnames(Sem2_output)[14:15] <- c("20-S2-CSC1028","Grade-S2-CSC1028")
    colnames(Sem2_output)[16:17] <- c("20-S2-CSC1029","Grade-S2-CSC1029")
    colnames(Sem2_output)[18:19] <- c("20-S2-CSC1030","Grade-S2-CSC1030")
    colnames(Sem2_output)[20:21] <- c("20-S2-CSC1031","Grade-S2-CSC1031")
    
    #this collects the Term year, email, pathway and sub plan description
    colnames(Sem2_output)[22:24] <- colnames(qsis_data)[c(19,26,28)]
    colnames(Sem2_output)[25:30] <- c("Module-Fail","CAT-Fail","Year_Avg","Progressed?","Comments","Invite To SSM")
    
    
    
    #reading in results and working out backend functions
    for (i in 1:num_of_students) {
      
      Sem2_output$Last[i] <- each_student$Last[i]
      Sem2_output$First_Name[i] <- each_student$First_Name[i]
      Sem2_output$ID[i] <- each_student$ID[i]
      
      for (j in 1:nrow(csc1022)){
        if(Sem2_output$ID[i] == csc1022$ID[j]){
          #registered students on course returns 0 for no marks present on test data
          Sem2_output$`Grade-S1-CSC1022`[i] <- toupper(csc1022$Classification[j])
          Sem2_output$`20-S1-CSC1022`[i] <- updating_exceptions(csc1022$Final[j],
                                                                Sem2_output$`Grade-S1-CSC1022`[i])
       
        }
      }
      
      for (j in 1:nrow(csc1025)){
        if(Sem2_output$ID[i] == csc1025$ID[j]){
          Sem2_output$`Grade-S1-CSC1025`[i] <- toupper(csc1025$Classification[j])
          Sem2_output$`20-S1-CSC1025`[i] <- updating_exceptions(csc1025$Final[j],
                                                                Sem2_output$`Grade-S1-CSC1025`[i])
   
          
        }
      }
      
      for (j in 1:nrow(csc1026)){
        if(Sem2_output$ID[i] == csc1026$ID[j]){
          Sem2_output$`Grade-S1-CSC1026`[i] <- toupper(csc1026$Classification[j])
          
          #registered students on course returns 0 for no marks present on test data
          Sem2_output$`20-S1-CSC1026`[i] <- updating_exceptions(csc1026$Final[j],
                                                                Sem2_output$`Grade-S1-CSC1026`[i])
       
          
        }
      }
      
      
      for (j in 1:nrow(csc1027)){
        if(Sem2_output$ID[i] == csc1027$ID[j]){
          Sem2_output$`Grade-S1-CSC1027`[i] <- toupper(csc1027$Classification[j])
          Sem2_output$`20-S1-CSC1027`[i] <- updating_exceptions(csc1027$Final[j],
                                                                Sem2_output$`Grade-S1-CSC1027`[i])
        
        }
      }
      
      
      
      ######################################
      #Reading in uploaded files next - classifcation should be upper from read in 
      #######################################
      
      for (j in 1:nrow(csc1023)){
        if(Sem2_output$ID[i] == csc1023$ID[j]){
          Sem2_output$`Grade-S2-CSC1023`[i] <- csc1023$Classification[j]
          Sem2_output$`20-S2-CSC1023`[i] <- updating_exceptions(csc1023$Final[j],
                                                                Sem2_output$`Grade-S2-CSC1023`[i])
    
        }
      }
      
      
      for (j in 1:nrow(csc1028)){
        if(Sem2_output$ID[i] == csc1028$ID[j]){
          Sem2_output$`Grade-S2-CSC1028`[i] <- csc1028$Classification[j]
          Sem2_output$`20-S2-CSC1028`[i] <- updating_exceptions(csc1028$Final[j],
                                                                Sem2_output$`Grade-S2-CSC1028`[i])
      
        }
      }
      
      for (j in 1:nrow(csc1029)){
        if(Sem2_output$ID[i] == csc1029$ID[j]){
          Sem2_output$`Grade-S2-CSC1029`[i] <- csc1029$Classification[j]
          Sem2_output$`20-S2-CSC1029`[i] <- updating_exceptions(csc1029$Final[j],
                                                                Sem2_output$`Grade-S2-CSC1029`[i])
         
        }
      }
      
      
      for (j in 1:nrow(csc1030)){
        if(Sem2_output$ID[i] == csc1030$ID[j]){
          Sem2_output$`Grade-S2-CSC1030`[i] <- csc1030$Classification[j]
          Sem2_output$`20-S2-CSC1030`[i] <- updating_exceptions(csc1030$Final[j],
                                                                Sem2_output$`Grade-S2-CSC1030`[i])
     
        }
      }
      
      for (j in 1:nrow(csc1031)){
        if(Sem2_output$ID[i] == csc1031$ID[j]){
          Sem2_output$`Grade-S2-CSC1031`[i] <- csc1031$Classification[j]
          Sem2_output$`20-S2-CSC1031`[i] <- updating_exceptions(csc1031$Final[j],
                                                                Sem2_output$`Grade-S2-CSC1031`[i])
        
        }
      }
      
      
      Sem2_output$Email[i] <- each_student$Email[i]
      Sem2_output$Plan_Descr[i] <- each_student$Plan_Descr[i]
      Sem2_output$Sub_Plan_Descr[i] <- each_student$Sub_Plan_Descr[i]
      
      #pipeline of grades for each student
      subset_grades <- select(Sem2_output[i, ], 5,7,9,11,13,15,17,19,21)  
      
      subset_marks <- select(Sem2_output[i,], 4,6,8,10,12,14,16,18,20)
      
      #reading row of student grades into function to work out mods failed
      Sem2_output$`Module-Fail`[i] <- mods_fail_total(subset_grades)
      
      #Calculating how many cats failed with mods_fail_total function
      Sem2_output$`CAT-Fail`[i] <- mods_fail_total(subset_grades) * 20
      
      #working out year average for each student with year_average function
      Sem2_output$Year_Avg[i] <- year_average(subset_marks,subset_grades )
      
      #working out progression with progression function
      Sem2_output$`Progressed?`[i] <- progression(Sem2_output$`CAT-Fail`[i] 
                                                
      )
      
      Sem2_output$Comments[i] <- comments_sem2(Sem2_output$`CAT-Fail`[i], 
                                               subset_grades, 
                                               Sem2_output$Plan_Descr[i],
                                               Sem2_output$Year_Avg[i]
                                               
                                         
      )
      
      Sem2_output$`Invite To SSM`[i] <- invite_meeting_cs(Sem2_output$`Progressed?`[i],subset_grades,
                                                          Sem2_output$Plan_Descr[i],Sem2_output$Year_Avg[i])
      
      
    }
    
    return(Sem2_output)
  } 
  
  
  #Semester two BIT Pathway results
  sem1_2_BIT_path <- function(qsis_data, csc1023, csc1024, eco1007, mgt1012, mgt1013){
    
    #finding out how many students there is for S1 - returns a Integer value
    num_of_students <- length(unique(qsis_data$ID))
    
    #dataframe of unique student details - returns data of unique students
    each_student <- distinct(qsis_data , ID, .keep_all = TRUE)
    
    
    #finding out how many students there is for S1 - returns a Integer value
    num_of_students <- length(unique(qsis_data$ID))
    
    #dataframe of unique student details - returns data of unique students
    each_student <- distinct(qsis_data , ID, .keep_all = TRUE)
    
    #grades of s1 BIT
    acc1022<- qsis_data %>% select(ID, Catalog, Final, Classification) %>% filter(Catalog == 1002)
    mgt1009 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1009)
    
    
    #creating empty output table
    Sem2_output <- data.frame(matrix(data = NA, nrow = num_of_students , ncol = 26))
    
    #work out amount of columns for s1 - naming relevant fields to output
    colnames(Sem2_output)[1:3] <- colnames(qsis_data)[c(1,2,3)]
    colnames(Sem2_output)[4:5] <- c("20-S1-ACC1022","Grade-S1-ACC1022")
    colnames(Sem2_output)[6:7] <- c("20-S1-MGT1009","Grade-S1-MGT1009")
    colnames(Sem2_output)[8:9] <- c("20-S2-CSC1023","Grade-S2-CSC1023")
    colnames(Sem2_output)[10:11] <- c("20-S2-CSC1024","Grade-S2-CSC1024")
    colnames(Sem2_output)[12:13] <- c("20-S2-ECO1007","Grade-S2-ECO1007")
    colnames(Sem2_output)[14:15] <- c("20-S2-MGT1012","Grade-S2-MGT1012")
    colnames(Sem2_output)[16:17] <- c("20-S2-MGT1013","Grade-S2-MGT1013")
    colnames(Sem2_output)[18:20] <- colnames(qsis_data)[c(19,26,28)]
    colnames(Sem2_output)[21:26] <- c("Module-Fail","CAT-Fail","Year_Avg","Progressed?","Comments","Invite To SSM")
    
    
    #reading in results and working out backend functions
    for (i in 1:num_of_students) {
      
      Sem2_output$Last[i] <- each_student$Last[i]
      Sem2_output$First_Name[i] <- each_student$First_Name[i]
      Sem2_output$ID[i] <- each_student$ID[i]
   
      
      for (j in 1:nrow(acc1022)){
        if(Sem2_output$ID[i] == acc1022$ID[j]){
          Sem2_output$`Grade-S1-ACC1022`[i] <- toupper(acc1022$Classification[j])
          #updating fails function updates failed mods with 0
          Sem2_output$`20-S1-ACC1022`[i] <- updating_exceptions(acc1022$Final[j],
                                                                Sem2_output$`Grade-S1-ACC1022`[i])
        
        }
      }
      
      for (j in 1:nrow(mgt1009)){
        if(Sem2_output$ID[i] == mgt1009$ID[j]){
          Sem2_output$`Grade-S1-MGT1009`[i] <- toupper(mgt1009$Classification[j])
          Sem2_output$`20-S1-MGT1009`[i] <- updating_exceptions(mgt1009$Final[j],
                                                                Sem2_output$`Grade-S1-MGT1009`[i])
        
          
        }
      }
      
      for (j in 1:nrow(csc1023)){
        if(Sem2_output$ID[i] == csc1023$ID[j]){
          #registered students on course returns 0 for no marks present on test data
          Sem2_output$`Grade-S2-CSC1023`[i] <- csc1023$Classification[j]
          Sem2_output$`20-S2-CSC1023`[i] <- updating_exceptions(csc1023$Final[j],
                                                                Sem2_output$`Grade-S2-CSC1023`[i])
         
          
        }
      }
      
  
      for (j in 1:nrow(csc1024)){
        if(Sem2_output$ID[i] == csc1024$ID[j]){
          Sem2_output$`Grade-S2-CSC1024`[i] <- csc1024$Classification[j]
          Sem2_output$`20-S2-CSC1024`[i] <- updating_exceptions(csc1024$Final[j],
                                                                Sem2_output$`Grade-S2-CSC1024`[i] )
       
        }
      }
      
      
      for (j in 1:nrow(eco1007)){
        if(Sem2_output$ID[i] == eco1007$ID[j]){
          Sem2_output$`Grade-S2-ECO1007`[i] <- eco1007$Classification[j]
          Sem2_output$`20-S2-ECO1007`[i] <- updating_exceptions(eco1007$Final[j],
                                                                Sem2_output$`Grade-S2-ECO1007`[i])
   
        }
      }
      
      
      
      
      for (j in 1:nrow(mgt1012)){
        if(Sem2_output$ID[i] == mgt1012$ID[j]){
          Sem2_output$`Grade-S2-MGT1012`[i] <- mgt1012$Classification[j]
          Sem2_output$`20-S2-MGT1012`[i] <- updating_exceptions(mgt1012$Final[j],
                                                                Sem2_output$`Grade-S2-MGT1012`[i])
         
        }
      }
      
      for (j in 1:nrow(mgt1013)){
        if(Sem2_output$ID[i] == mgt1013$ID[j]){
          Sem2_output$`Grade-S2-MGT1013`[i] <- mgt1013$Classification[j]
          Sem2_output$`20-S2-MGT1013`[i] <- updating_exceptions(mgt1013$Final[j],
                                                                Sem2_output$`Grade-S2-MGT1013`[i])
        
        }
      }
      
      Sem2_output$Email[i] <- each_student$Email[i]
      Sem2_output$Plan_Descr[i] <- each_student$Plan_Descr[i]
      Sem2_output$Sub_Plan_Descr[i] <- each_student$Sub_Plan_Descr[i]
      
      
      #pipeline of grades for each student
      subset_grades <- select(Sem2_output[i, ], 5,7,9,11,13,15,17) 
      
      #subset_csc1024 <- select(Sem2_output[i,], 11)
      
      # #pipeline of marks for each student
      subset_marks <- select(Sem2_output[i,], 4,6,8,10,12,14,16)  
      
      #reading row of student grades into function to work out mods failed
      Sem2_output$`Module-Fail`[i] <- mods_fail_total(subset_grades)
      
      #Calculating how many cats failed with mods_fail_total function
      Sem2_output$`CAT-Fail`[i] <- cat_fails_bit(subset_grades)
      
      #working out year average for each student with year_average function
      Sem2_output$Year_Avg[i] <- year_average(subset_marks, subset_grades)
      
      
      #working out progression with progression function
      Sem2_output$`Progressed?`[i] <-
        progression(
          Sem2_output$`CAT-Fail`[i]
        )
      
      
      Sem2_output$Comments[i] <- comments_sem2(
        Sem2_output$`CAT-Fail`[i],
        subset_grades,
        Sem2_output$Plan_Descr[i],
        Sem2_output$Year_Avg[i]

        
      )
      
      Sem2_output$`Invite To SSM`[i] <-
        invite_meeting_bit(Sem2_output$`Progressed?`[i], subset_grades)
      
    }
    
    
    return(Sem2_output)
    
  }
  
  ###### Semester 3

  #Semester three BIT Pathway results

  sem3_BIT_path <- function(qsis_data, resits){
    
    
    #finding out how many students there is for S1 - returns a Integer value
    num_of_students <- length(unique(qsis_data$ID))
    
    #dataframe of unique student details - returns data of unique students
    each_student <- distinct(qsis_data , ID, .keep_all = TRUE)
    
    #finding out how many students there is for S1 - returns a Integer value
    num_of_students <- length(unique(qsis_data$ID))
    
    #dataframe of unique student details - returns data of unique students
    each_student <- distinct(qsis_data , ID, .keep_all = TRUE)
    
    #grades of s1 BIT
    acc1022<- qsis_data %>% select(ID, Catalog, Final, Classification) %>% filter(Catalog == 1002)
    mgt1009 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1009)
    
    #grades of s2 BIT
    csc1023 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1023)
    csc1024 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1024)
    eco1007 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1007)
    mgt1012 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1012)
    mgt1013 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1013)
    
    
    #creating empty output table
    Sem3_output <- data.frame(matrix(data = NA, nrow = num_of_students , ncol = 26))
    
    #work out amount of columns for s1 - naming relevant fields to output
    colnames(Sem3_output)[1:3] <- colnames(qsis_data)[c(1,2,3)]
    colnames(Sem3_output)[4:5] <- c("20-S1-ACC1022","Grade-S1-ACC1022")
    colnames(Sem3_output)[6:7] <- c("20-S1-MGT1009","Grade-S1-MGT1009")
    colnames(Sem3_output)[8:9] <- c("20-S2-CSC1023","Grade-S2-CSC1023")
    colnames(Sem3_output)[10:11] <- c("20-S2-CSC1024","Grade-S2-CSC1024")
    colnames(Sem3_output)[12:13] <- c("20-S2-ECO1007","Grade-S2-ECO1007")
    colnames(Sem3_output)[14:15] <- c("20-S2-MGT1012","Grade-S2-MGT1012")
    colnames(Sem3_output)[16:17] <- c("20-S2-MGT1013","Grade-S2-MGT1013")
    colnames(Sem3_output)[18:20] <- colnames(qsis_data)[c(19,26,28)]
    colnames(Sem3_output)[21:26] <- c("Module-Fail","CAT-Fail","Year_Avg","Progressed?","Comments","Invite To SSM")
    
    
    
    ######################################################
    # for organsing data from the qsis file into sem3_output table
    
    ######################################################
    for (i in 1:num_of_students) {
      
      Sem3_output$Last[i] <- each_student$Last[i]
      Sem3_output$First_Name[i] <- each_student$First_Name[i]
      Sem3_output$ID[i] <- each_student$ID[i]
      
      for (j in 1:nrow(acc1022)){
        if(Sem3_output$ID[i] == acc1022$ID[j]){
          Sem3_output$`Grade-S1-ACC1022`[i] <- toupper(acc1022$Classification[j])
          #updating fails function updates failed mods with 0
          Sem3_output$`20-S1-ACC1022`[i] <- updating_exceptions(acc1022$Final[j],
                                                                Sem3_output$`Grade-S1-ACC1022`[i])
          
        }
      }
      
      for (j in 1:nrow(mgt1009)){
        if(Sem3_output$ID[i] == mgt1009$ID[j]){
          Sem3_output$`Grade-S1-MGT1009`[i] <- toupper(mgt1009$Classification[j])
          Sem3_output$`20-S1-MGT1009`[i] <- updating_exceptions(mgt1009$Final[j],
                                                                Sem3_output$`Grade-S1-MGT1009`[i])
          
          
        }
      }
      
      
      for (j in 1:nrow(csc1023)){
        if(Sem3_output$ID[i] == csc1023$ID[j]){
          Sem3_output$`Grade-S2-CSC1023`[i] <- toupper(csc1023$Classification[j])
          #registered students on course returns 0 for no marks present on test data
          Sem3_output$`20-S2-CSC1023`[i] <- updating_exceptions(csc1023$Final[j],
                                                                Sem3_output$`Grade-S2-CSC1023`[i])
          
          
        }
      }
      
      
      for (j in 1:nrow(csc1024)){
        if(Sem3_output$ID[i] == csc1024$ID[j]){
          Sem3_output$`Grade-S2-CSC1024`[i] <- toupper(csc1024$Classification[j])
          #registered students on course returns 0 for no marks present on test data
          Sem3_output$`20-S2-CSC1024`[i] <- updating_exceptions(csc1024$Final[j],
                                                                Sem3_output$`Grade-S2-CSC1024`[i])
          
          
        }
      }
      
      
      for (j in 1:nrow(eco1007)){
        if(Sem3_output$ID[i] == eco1007$ID[j]){
          Sem3_output$`Grade-S2-ECO1007`[i] <- toupper(eco1007$Classification[j])
          #registered students on course returns 0 for no marks present on test data
          Sem3_output$`20-S2-ECO1007`[i] <- updating_exceptions(eco1007$Final[j],
                                                                Sem3_output$`Grade-S2-ECO1007`[i] )
          
          
        }
      }
      
      
      for (j in 1:nrow(mgt1012)){
        if(Sem3_output$ID[i] == mgt1012$ID[j]){
          Sem3_output$`Grade-S2-MGT1012`[i] <- toupper(mgt1012$Classification[j])
          Sem3_output$`20-S2-MGT1012`[i] <- updating_exceptions(mgt1012$Final[j],
                                                                Sem3_output$`Grade-S2-MGT1012`[i])
          
        }
      }
      
      
      for (j in 1:nrow(mgt1013)){
        if(Sem3_output$ID[i] == mgt1013$ID[j]){
          Sem3_output$`Grade-S2-MGT1013`[i] <-toupper(mgt1013$Classification[j])
          Sem3_output$`20-S2-MGT1013`[i] <- updating_exceptions(mgt1013$Final[j],
                                                                Sem3_output$`Grade-S2-MGT1013`[i])
          
        }
      }
      
      
      Sem3_output$Email[i] <- each_student$Email[i]
      Sem3_output$Plan_Descr[i] <- each_student$Plan_Descr[i]
      Sem3_output$Sub_Plan_Descr[i] <- each_student$Sub_Plan_Descr[i]
      
    } 
    
    
    
    ######################################################
    ###checking for resits - replacing existing values in table for repeats from list 
    ######################################################
    
    
    
    for (i in 1:num_of_students) {
      
      
      #### - CSC1024 
      if(exists("csc1024_resit", where=resits)){
        
        #add to already created table 
        #allows for further multiplication of data  - by converting to dataframe
        csc1024_repeat <- resits$csc1024_resit
        
        for (j in 1:nrow(csc1024_repeat)){
          if(Sem3_output$ID[i] == csc1024_repeat$ID[j]){
            Sem3_output$`Grade-S2-CSC1024`[i] <- toupper(csc1024_repeat$Classification[j])
            #registered students on course returns 0 for no marks present on test data
            Sem3_output$`20-S2-CSC1024`[i] <- updating_exceptions(csc1024_repeat$Final[j],
                                                                  Sem3_output$`Grade-S2-CSC1024`[i])
            
          }
        }
      }
      
      
      #### - ACC1022
      if(exists("acc1022_resit", where=resits)){
        
        #add to already created table 
        #allows for further multiplication of data  - by converting to dataframe
        acc1022_repeat <- resits$acc1022_resit
        
        for (j in 1:nrow(acc1022_repeat)){
          if(Sem3_output$ID[i] == acc1022_repeat$ID[j]){
            Sem3_output$`Grade-S1-ACC1022`[i] <- toupper(acc1022_repeat$Classification[j])
            #updating fails function updates failed mods with 0
            Sem3_output$`20-S1-ACC1022`[i] <- updating_exceptions(acc1022_repeat$Final[j],
                                                                  Sem3_output$`Grade-S1-ACC1022`[i])
            
          }
          
        }
        
      }
      
      #### - MGT1009
      if(exists("mgt1009_resit", where=resits)){
        
        #allows for further multiplication of data  - by converting to dataframe
        mgt1009_repeat <- resits$mgt1009_resit
        
        
        for (j in 1:nrow(mgt1009_repeat)) {
          if(Sem3_output$ID[i] == mgt1009_repeat$ID[j]){
            Sem3_output$`Grade-S1-MGT1009`[i] <- toupper(mgt1009_repeat$Classification[j])
            Sem3_output$`20-S1-MGT1009`[i] <- updating_exceptions(mgt1009_repeat$Final[j],
                                                                  Sem3_output$`Grade-S1-MGT1009`[i])
            
          }
        }
        
      }  
      
      
      #### - CSC1023
      if(exists("csc1023_resit", where=resits)){
        csc1023_repeat <- resits$csc1023_resit
        
        for (j in 1:nrow(csc1023_repeat)) {
          if(Sem3_output$ID[i] == csc1023_repeat$ID[j]){
            Sem3_output$`Grade-S2-CSC1023`[i] <- toupper(csc1023_repeat$Classification[j])
            #registered students on course returns 0 for no marks present on test data
            Sem3_output$`20-S2-CSC1023`[i] <- updating_exceptions(csc1023_repeat$Final[j],
                                                                  Sem3_output$`Grade-S2-CSC1023`[i])
            
          }
        }
      }
      
      
      
      #### - ECO1007
      if(exists("eco1007_resit", where = resits)) {
        eco1007_repeat <- resits$eco1007_resit
        
        for (j in 1:nrow(eco1007_repeat)) {
          if (Sem3_output$ID[i] == eco1007_repeat$ID[j]) {
            Sem3_output$`Grade-S2-ECO1007`[i] <-
              toupper(eco1007_repeat$Classification[j])
            #registered students on course returns 0 for no marks present on test data
            Sem3_output$`20-S2-ECO1007`[i] <-
              updating_exceptions(eco1007_repeat$Final[j],Sem3_output$`Grade-S2-ECO1007`[i])
            
            
          }
        }
      }
      
      
      #### - MGT1012
      if(exists("mgt1012_resit", where=resits)){
        
        mgt1012_repeat <- resits$mgt1012_resit
        
        
        for (j in 1:nrow(mgt1012_repeat)) {
          if(Sem3_output$ID[i] == mgt1012_repeat$ID[j]){
            Sem3_output$`Grade-S2-MGT1012`[i] <- toupper(mgt1012_repeat$Classification[j])
            #registered students on course returns 0 for no marks present on test data
            Sem3_output$`20-S2-MGT1012`[i] <- updating_exceptions(mgt1012_repeat$Final[j],
                                                                  Sem3_output$`Grade-S2-MGT1012`[i])
            
          }
        }
      }
      
      
      
      
      if(exists("mgt1013_resit", where=resits)){
        
        mgt1013_repeat <- resits$mgt1013_resit
        
        for (j in 1:nrow(mgt1013_repeat)) {
          if(Sem3_output$ID[i] == mgt1012_repeat$ID[j]){
            Sem3_output$`Grade-S2-MGT1013`[i] <- toupper(mgt1013_repeat$Classification[j])
            #registered students on course returns 0 for no marks present on test data
            Sem3_output$`20-S2-MGT1013`[i] <- updating_exceptions(mgt1013_repeat$Final[j],
                                                                  Sem3_output$`Grade-S2-MGT1013`[i])
            
          }
          
        }  
      }
      
    }
    
    
    ######################################################
    # for analysing the student progression  functions on results read in 
    
    ######################################################
    
    
    
    for (i in 1:num_of_students) {
      
      subset_grades <- select(Sem3_output[i, ], 5,7,9,11,13,15,17) 
      
      # #pipeline of marks for each student
      subset_marks <- select(Sem3_output[i,], 4,6,8,10,12,14,16)  
      
      #reading row of student grades into function to work out mods failed
      Sem3_output$`Module-Fail`[i] <- mods_fail_total(subset_grades)
      
      #print(subset_grades)
      
      #Calculating how many cats failed with mods_fail_total function
      Sem3_output$`CAT-Fail`[i] <- cat_fails_bit(subset_grades)
      
      #working out year average for each student with year_average function
      Sem3_output$Year_Avg[i] <- year_average(subset_marks, subset_grades)
      
      
      Sem3_output$`Progressed?`[i] <- progression(Sem3_output$`CAT-Fail`[i]
      )
      
      Sem3_output$Comments[i] <- comments_final(Sem3_output$`CAT-Fail`[i], subset_grades, 
                                                Sem3_output$Plan_Descr[i],Sem3_output$Year_Avg[i], "BIT"
      )
      
      Sem3_output$`Invite To SSM`[i] <- invite_meeting_bit(Sem3_output$`Progressed?`[i],Sem3_output$Plan_Descr[i])
      
    }
    
    
    return(Sem3_output)
    
    
  }
  
  
  
  #Semester three CS_SE_CIT  Pathway results
  sem3_CS_path <- function(qsis_data, resits){
    
    
    #finding out how many students there is for S1 - returns a Integer value
    num_of_students <- length(unique(qsis_data$ID))
    
    #dataframe of unique student details - returns data of unique students
    each_student <- distinct(qsis_data , ID, .keep_all = TRUE)
    
    #finding out how many students there is for S1 - returns a Integer value
    num_of_students <- length(unique(qsis_data$ID))
    
    #dataframe of unique student details - returns data of unique students
    each_student <- distinct(qsis_data , ID, .keep_all = TRUE)
    
    
    ###filtering out results of semester one and two 
    csc1022 <- qsis_data %>% select(ID, Catalog, Final, Classification) %>% filter(Catalog == 1022)
    csc1025 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1025)
    csc1026 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1026)
    csc1027 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1027)
    csc1023 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1023)
    csc1028 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1028)
    csc1029 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1029)
    csc1030 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1030)
    csc1031 <- qsis_data %>% select(ID, Catalog,  Final, Classification) %>% filter(Catalog == 1031)
    
    
    
    #creating empty output table
    Sem3_output <- data.frame(matrix(data = NA, nrow = num_of_students , ncol = 30))
    
    #work out amount of columns for s1 + s2- naming relevant fields to output
    colnames(Sem3_output)[1:3] <- colnames(qsis_data)[c(1,2,3)]
    colnames(Sem3_output)[4:5] <- c("20-S1-CSC1022","Grade-S1-CSC1022")
    colnames(Sem3_output)[6:7] <- c("20-S1-CSC1025","Grade-S1-CSC1025")
    colnames(Sem3_output)[8:9] <- c("20-S1-CSC1026","Grade-S1-CSC1026")
    colnames(Sem3_output)[10:11] <- c("20-S1-CSC1027","Grade-S1-CSC1027")
    colnames(Sem3_output)[12:13] <- c("20-S2-CSC1023","Grade-S2-CSC1023")
    colnames(Sem3_output)[14:15] <- c("20-S2-CSC1028","Grade-S2-CSC1028")
    colnames(Sem3_output)[16:17] <- c("20-S2-CSC1029","Grade-S2-CSC1029")
    colnames(Sem3_output)[18:19] <- c("20-S2-CSC1030","Grade-S2-CSC1030")
    colnames(Sem3_output)[20:21] <- c("20-S2-CSC1031","Grade-S2-CSC1031")
    
    #this collects the Term year, email, pathway and sub plan description
    colnames(Sem3_output)[22:24] <- colnames(qsis_data)[c(19,26,28)]
    colnames(Sem3_output)[25:30] <- c("Module-Fail","CAT-Fail","Year_Avg","Progressed?","Comments","Invite To SSM")
    
    
    ######################################################
    # for organsing data from the qsis file into sem3_output table
    
    ######################################################
    
    for (i in 1:num_of_students) {
      
      #details from qsis 
      
      Sem3_output$Last[i] <- each_student$Last[i]
      Sem3_output$First_Name[i] <- each_student$First_Name[i]
      Sem3_output$ID[i] <- each_student$ID[i]
      
      Sem3_output$Email[i] <- each_student$Email[i]
      Sem3_output$Plan_Descr[i] <- each_student$Plan_Descr[i]
      Sem3_output$Sub_Plan_Descr[i] <- each_student$Sub_Plan_Descr[i]
      
      
      for (j in 1:nrow(csc1022)){
        if(Sem3_output$ID[i] == csc1022$ID[j]){
          Sem3_output$`Grade-S1-CSC1022`[i] <- toupper(csc1022$Classification[j])
          #registered students on course returns 0 for no marks present on test data
          Sem3_output$`20-S1-CSC1022`[i] <- updating_exceptions(csc1022$Final[j],
                                                                Sem3_output$`Grade-S1-CSC1022`[i])
          
        }
      }
      
      for (j in 1:nrow(csc1025)){
        if(Sem3_output$ID[i] == csc1025$ID[j]){
          Sem3_output$`Grade-S1-CSC1025`[i] <- toupper(csc1025$Classification[j])
          Sem3_output$`20-S1-CSC1025`[i] <- updating_exceptions(csc1025$Final[j],
                                                                Sem3_output$`Grade-S1-CSC1025`[i])
          
          
        }
      }
      
      for (j in 1:nrow(csc1026)){
        if(Sem3_output$ID[i] == csc1026$ID[j]){
          Sem3_output$`Grade-S1-CSC1026`[i] <- toupper(csc1026$Classification[j])
          #registered students on course returns 0 for no marks present on data
          Sem3_output$`20-S1-CSC1026`[i] <- updating_exceptions(csc1026$Final[j],
                                                                Sem3_output$`Grade-S1-CSC1026`[i] )
          
        }
      }
      
      for (j in 1:nrow(csc1027)){
        if(Sem3_output$ID[i] == csc1027$ID[j]){
          Sem3_output$`Grade-S1-CSC1027`[i] <- toupper(csc1027$Classification[j])
          Sem3_output$`20-S1-CSC1027`[i] <- updating_exceptions(csc1027$Final[j],
                                                                Sem3_output$`Grade-S1-CSC1027`[i])
          
        }
      }
      
      for (j in 1:nrow(csc1023)){
        if(Sem3_output$ID[i] == csc1023$ID[j]){
          Sem3_output$`Grade-S2-CSC1023`[i] <- toupper(csc1023$Classification[j])
          Sem3_output$`20-S2-CSC1023`[i] <- updating_exceptions(csc1023$Final[j],
                                                                Sem3_output$`Grade-S2-CSC1023`[i])
          
        }
      }
      
      for (j in 1:nrow(csc1028)){
        if(Sem3_output$ID[i] == csc1028$ID[j]){
          Sem3_output$`Grade-S2-CSC1028`[i] <- toupper(csc1028$Classification[j])
          Sem3_output$`20-S2-CSC1028`[i] <- updating_exceptions(csc1028$Final[j],
                                                                Sem3_output$`Grade-S2-CSC1028`[i])
          
        }
      }
      
      for (j in 1:nrow(csc1029)){
        if(Sem3_output$ID[i] == csc1029$ID[j]){
          Sem3_output$`Grade-S2-CSC1029`[i] <- toupper(csc1029$Classification[j])
          Sem3_output$`20-S2-CSC1029`[i] <- updating_exceptions(csc1029$Final[j],
                                                                Sem3_output$`Grade-S2-CSC1029`[i])
          
        }
      }
      
      for (j in 1:nrow(csc1030)){
        if(Sem3_output$ID[i] == csc1030$ID[j]){
          Sem3_output$`Grade-S2-CSC1030`[i] <- toupper(csc1030$Classification[j])
          Sem3_output$`20-S2-CSC1030`[i] <- updating_exceptions(csc1030$Final[j],
                                                                Sem3_output$`Grade-S2-CSC1030`[i])
          
        }
      }
      
      for (j in 1:nrow(csc1031)){
        if(Sem3_output$ID[i] == csc1031$ID[j]){
          Sem3_output$`Grade-S2-CSC1031`[i] <- toupper(csc1031$Classification[j])
          Sem3_output$`20-S2-CSC1031`[i] <- updating_exceptions(csc1031$Final[j],
                                                                Sem3_output$`Grade-S2-CSC1031`[i])
          
        }
      }
      
    }
    
    
    ######################################################
    ###checking for resits - replacing existing values with 
    # files including repeat marks / classifications 
    
    ######################################################
    
    
    for (i in 1:num_of_students) {
      
      
      #### - CSC1022 
      if(exists("csc1022_resit", where=resits)){
        
        #add to already created table 
        #allows for further multiplication of data  - by converting to dataframe
        csc1022_repeat <- resits$csc1022_resit
        
        for (j in 1:nrow(csc1022_repeat)){
          if(Sem3_output$ID[i] == csc1022_repeat$ID[j]){
            Sem3_output$`Grade-S1-CSC1022`[i] <- toupper(csc1022_repeat$Classification[j])
            #registered students on course returns 0 for no marks present on test data
            Sem3_output$`20-S1-CSC1022`[i] <- updating_exceptions(csc1022_repeat$Final[j],
                                                                  Sem3_output$`Grade-S1-CSC1022`[i])
            
          }
        }
      }
      
      
      #### - CSC1025
      if(exists("csc1025_resit", where=resits)){
        
        #add to already created table 
        #allows for further multiplication of data  - by converting to dataframe
        csc1025_repeat <- resits$csc1025_resit
        
        for (j in 1:nrow(csc1025_repeat)){
          if(Sem3_output$ID[i] == csc1025_repeat$ID[j]){
            Sem3_output$`Grade-S1-CSC1025`[i] <- toupper(csc1025_repeat$Classification[j])
            
            #updating fails function updates failed mods with 0
            Sem3_output$`20-S1-CSC1025`[i] <- updating_exceptions(csc1025_repeat$Final[j],
                                                                  Sem3_output$`Grade-S1-CSC1025`[i])
            
          }
          
        }
        
      }
      
      
      #### - CSC1026
      if(exists("csc1026_resit", where=resits)){
        
        #allows for further multiplication of data  - by converting to dataframe
        csc1026_repeat <- resits$csc1026_resit
        
        
        for (j in 1:nrow(csc1026_repeat)) {
          if(Sem3_output$ID[i] == csc1026_repeat$ID[j]){
            Sem3_output$`Grade-S1-CSC1026`[i] <- toupper(csc1026_repeat$Classification[j])
            Sem3_output$`20-S1-CSC1026`[i] <- updating_exceptions(csc1026_repeat$Final[j],
                                                                  Sem3_output$`Grade-S1-CSC1026`[i])
            
          }
        }
        
      }  
      
      
      #### - CSC1027
      if(exists("csc1027_resit", where=resits)){
        csc1027_repeat <- resits$csc1027_resit
        
        for (j in 1:nrow(csc1027_repeat)) {
          if(Sem3_output$ID[i] == csc1027_repeat$ID[j]){
            Sem3_output$`Grade-S1-CSC1027`[i] <- toupper(csc1027_repeat$Classification[j])
            
            #registered students on course returns 0 for no marks present on test data
            Sem3_output$`20-S1-CSC1027`[i] <- updating_exceptions(csc1027_repeat$Final[j],
                                                                  Sem3_output$`Grade-S1-CSC1027`[i])
            
          }
        }
      }
      
      
      #### - CSC1023
      if(exists("csc1023_resit", where=resits)){
        csc1023_repeat <- resits$csc1023_resit
        
        for (j in 1:nrow(csc1023_repeat)) {
          if(Sem3_output$ID[i] == csc1023_repeat$ID[j]){
            Sem3_output$`Grade-S2-CSC1023`[i] <- toupper(csc1023_repeat$Classification[j])
            
            #registered students on course returns 0 for no marks present on test data
            Sem3_output$`20-S2-CSC1023`[i] <- updating_exceptions(csc1023_repeat$Final[j],
                                                                  Sem3_output$`Grade-S2-CSC1023`[i])
            
          }
        }
      }
      
      
      #### - CSC1028
      if(exists("csc1028_resit", where=resits)){
        csc1028_repeat <- resits$csc1028_resit
        
        for (j in 1:nrow(csc1028_repeat)) {
          if(Sem3_output$ID[i] == csc1028_repeat$ID[j]){
            Sem3_output$`Grade-S2-CSC1028`[i] <- toupper(csc1028_repeat$Classification[j])
            
            #registered students on course returns 0 for no marks present on test data
            Sem3_output$`20-S2-CSC1028`[i] <- updating_exceptions(csc1028_repeat$Final[j],
                                                                  Sem3_output$`Grade-S2-CSC1028`[i])
            
          }
        }
      }
      
      
      #### - CSC1029
      if(exists("csc1029_resit", where=resits)){
        csc1029_repeat <- resits$csc1029_resit
        
        for (j in 1:nrow(csc1029_repeat)) {
          if(Sem3_output$ID[i] == csc1029_repeat$ID[j]){
            Sem3_output$`Grade-S2-CSC1029`[i] <- toupper(csc1029_repeat$Classification[j])
            
            #registered students on course returns 0 for no marks present on test data
            Sem3_output$`20-S2-CSC1029`[i] <- updating_exceptions(csc1029_repeat$Final[j],
                                                                  Sem3_output$`Grade-S2-CSC1029`[i])
            
          }
        }
      }
      
      
      #### - CSC1030
      if(exists("csc1030_resit", where=resits)){
        csc1030_repeat <- resits$csc1030_resit
        
        for (j in 1:nrow(csc1030_repeat)) {
          if(Sem3_output$ID[i] == csc1030_repeat$ID[j]){
            Sem3_output$`Grade-S2-CSC1030`[i] <- toupper(csc1030_repeat$Classification[j])
            
            #registered students on course returns 0 for no marks present on test data
            Sem3_output$`20-S2-CSC1030`[i] <- updating_exceptions(csc1030_repeat$Final[j],
                                                                  Sem3_output$`Grade-S2-CSC1030`[i])
            
          }
        }
      }
      
      #### - CSC1031
      if(exists("csc1031_resit", where=resits)){
        csc1031_repeat <- resits$csc1031_resit
        
        for (j in 1:nrow(csc1031_repeat)) {
          if(Sem3_output$ID[i] == csc1031_repeat$ID[j]){
            Sem3_output$`Grade-S2-CSC1031`[i] <- toupper(csc1031_repeat$Classification[j])
            
            #registered students on course returns 0 for no marks present on test data
            Sem3_output$`20-S2-CSC1031`[i] <- updating_exceptions(csc1031_repeat$Final[j],
                                                                  Sem3_output$`Grade-S2-CSC1031`[i])
            
          }
        }
      }
      
      
      
    }
    
    
    ######################################################
    # for analysing the student progression  functions on results read in 
    
    ######################################################
    
    
    
    for (i in 1:num_of_students) {
      
      subset_grades <- select(Sem3_output[i, ], 5,7,9,11,13,15,17, 19,21)  
      
      # #pipeline of marks for each student
      subset_marks <- select(Sem3_output[i,], 4,6,8,10,12,14,16,18, 20) 
      
      #reading row of student grades into function to work out mods failed
      Sem3_output$`Module-Fail`[i] <- mods_fail_total(subset_grades)
      
      #print(subset_grades)
      
      #Calculating how many cats failed with mods_fail_total function
      Sem3_output$`CAT-Fail`[i] <- mods_fail_total(subset_grades) * 20 
      
      #working out year average for each student with year_average function
      Sem3_output$Year_Avg[i] <- year_average(subset_marks, subset_grades)
      
      #working out progression with progression function
      Sem3_output$`Progressed?`[i] <- progression(Sem3_output$`CAT-Fail`[i]
      )
      
      Sem3_output$Comments[i] <- comments_final(Sem3_output$`CAT-Fail`[i], subset_grades, 
                                                Sem3_output$Plan_Descr[i],Sem3_output$Year_Avg[i], "CS"
      )
      
      Sem3_output$`Invite To SSM`[i] <- invite_meeting_cs(Sem3_output$`Progressed?`[i], subset_grades,
                                                          Sem3_output$Plan_Descr[i],Sem3_output$Year_Avg[i])
      
    }
    
    
    return(Sem3_output)
    
    
  }
  
  #=============================================
  # table design functions 
  #=============================================
  
  
  #customGreen = "#71CA97"
  customGreen = "#008000"
  customRed = "#a30000"
  customOrange = "#FFA07A"
  
  
  #custom function to access modules - highlighted red under 40 - FAILED mods
  year_average_formatter <-
    formattable::formatter("span", style= x ~ style(
      color = ifelse(x >= 60, customGreen, ifelse (x< 40, customRed, "black")),
      font.weight = "bold"
    ))

  
  
  #failed mods under 40 - fail highlighted red
  failed_mods_formatter <- formattable::formatter("span", style= x ~ style(
    display = "block",
    `color` = case_when(
      x < 40 ~ customRed
    ),
    `background-color` = case_when(
      #x == "F" ~ "#ff7f7f",
      TRUE ~ "#FFFFFF"
    ),
    
    `font-weight` = case_when(
      x < 40 ~ "bold"
    )
  ))
  
  ###Progression  - still working on this
  progression_formatter <- formattable::formatter("span", style = x ~ style(
    font.weight = "bold",
    color = ifelse( x == "Yes", customGreen,  ifelse(x == "No", customRed, "black"))
  ),
  x ~ icontext(ifelse(x == "Yes" , "ok", "remove"), x)
  )

  
  #attempted code to base progression attribute on invite to SSM attribute values 
  
  # progressionFormatter2 <- formattable::formatter("span", style = `Invite To SSM`  ~ style(
  #   font.weight = "bold",
  #   color = case_when(
  #     `Invite To SSM` == "Yes" ~ "#FF6347",
  #     `Invite To SSM` == "Letter of concern" ~ "#FFFF00",
  #     `Invite To SSM` == "No" ~ "white"
  #   )),
  # x  ~ icontext(ifelse(x == "Yes" , "ok", "remove"), x)
  # )
  

  # #invite meeting? - yes highlighted in red, letter of concern is highlighted yellow
  invite_meeting_formatter <- formatter("span", style = x ~ style(
    font.weight  = "bold",
    display = "block",
    `background-color` = case_when(
      x == "Yes" ~ "#FF6347",
      x == "Letter of concern" ~ "	#FFFF00",
      x == "No" ~ "white"
    )
  )
  )

  
  #formatter formatting grades  + highlighting attention to absent grades + fails - may need
  #nmore codes - try adding a vector os the excpetion codes 
  grade_formatter <-
    formattable::formatter("span", style= x ~ style(
      display = "block",
      `background-color` = case_when(
        
        #FAILS
        x == "F" ~ "#ff7f7f",
        x == "FAIL" ~ "#ff7f7f",
    
        
        #ABSENTS
        x == "ABS" ~ "#707070",
        x == "ABSM" ~ "#707070",
        
        
        #EXCEPTIONS
        x == "CONT" ~ "#FFFACD",
        x == "RN" ~ "#FFFACD",
        x == "AUD" ~ "#FFFACD",
        x == "MNA" ~ "#FFFACD",
        x == "INC" ~ "#FFFACD",
        
        TRUE ~ "#FFFFFF"
        
      ),
      
      `color` = case_when(
        x == "ABS" ~ "#FFFFFF",
        x == "ABSM" ~ "#FFFFFF",
        x == "F" ~ "bold"
      ),
      
      
      `font-weight` = case_when(
        x == "ABS" ~ "bold",
        x == "ABSM" ~ "bold",
        x == "F" ~ "bold"
      )
    ))
  


  #=============================================
  
  # Reactive functions  
  
  #=============================================  
  
  #reset button on the generator form - uses shiny JS 
  observeEvent(input$clear, {

    # WONT REGENERATE NEW TABLE ---******
    #removeUI(selector = "#progress_results") 
    #trying with input 23-05
    # reset(
    #   
    #   output$progress_results <- renderDataTable(
    #     {
    #     }
    #   )
    # )
    #shinyjs::reset("progress_table")
    
    #reseting options choosen by user  - S1
    reset("student_upload")
    reset("csc1022_file")
    reset("csc1025_file")
    reset("csc1026_file")
    reset("csc1027_file")
    
    reset("acc1022_file")
    reset("mgt1009_file")
    
    
    #semester 2  student options
    reset("csc1023_file")
    reset("csc1028_file")
    reset("csc1029_file")
    reset("csc1030_file")
    reset("csc1031_file")
    
    #semester 2 bit files 
    reset('input$csc1023_file_bit')
    reset('input$mgt1012_file')
    reset('input$csc1024_file')
    reset('input$mgt1013_file')
    reset('input$eco1007_file')
    
    #semester 3 inputs 
    reset('csc1022_repeat_cs')
    reset('csc1025_repeat')
    reset('csc1026_repeat')
    reset('csc1027_repeat')
    reset('csc1028_repeat')
    reset('csc1029_repeat')
    reset('csc1030_repeat')
    reset('csc1031_repeat')
    
    

    
    reset('acc1022_repeat')
    reset('mgt1009_repeat')
    reset('mgt1012_repeat')
    reset('mgt1013_repeat')
    reset('csc1024_repeat')
    reset('csc1023_repeat')
    reset('eco1007_repeat')

  })
  
  ################
  # CS / SE / CIT  reactive data
  ###################
  
  
  # CS semester 1 reactive data
  data_s1_cs = reactive(
    {
      
      inFile <- input$student_upload
      qsis <- read_qsis_data(inFile$datapath)
      
      inFile <- input$csc1022_file
      x1022 <- read_module_results(inFile$datapath)
      
      inFile <- input$csc1025_file
      x1025 <- read_module_results(inFile$datapath)
      
      inFile <- input$csc1026_file
      x1026 <- read_module_results(inFile$datapath)
      
      inFile <- input$csc1027_file
      x1027 <- read_module_results(inFile$datapath)
      
      #adding files to Sem1 function
      table <- sem1_CS_path(qsis, x1022, x1025, x1026, x1027)
      
      #repalcing NA values
      #table[is.na(table)] <- '-'
      
      
      
      
    }
  )
  
  # data semester 2 CS 
  data_s2_cs <- reactive (
    {
      
      
      
      #once validations checked read files in
      inFile <- input$student_upload
      qsis <- read_qsis_data(inFile$datapath)
      
      inFile <- input$csc1023_file
      x1023 <- read_module_results(inFile$datapath)
      
      inFile <- input$csc1028_file
      x1028 <- read_module_results(inFile$datapath)
      
      inFile <- input$csc1029_file
      x1029 <- read_module_results(inFile$datapath)
      
      inFile <- input$csc1030_file
      x1030 <- read_module_results(inFile$datapath)
      
      inFile <- input$csc1031_file
      x1031 <- read_module_results(inFile$datapath)
      
      
      
      #when user clicks generate action button  - as an if statement to input?
      #generate after read in - prevents error message appearing on screen about missing columns
      #input$generate
      
      table <- sem2_CS_path(qsis, x1023, x1028, x1029, x1030, x1031)
      
    }
  )

  # semester 1 +semester 2 cs data
  data_s1_s2_cs <- reactive ({
    
    inFile <- input$student_upload
    # if (is.null(inFile))
    #   return(NULL)
    qsis <- read_qsis_data(inFile$datapath)

    inFile <- input$csc1023_file
    x1023 <- read_module_results(inFile$datapath)

    inFile <- input$csc1028_file
    x1028 <- read_module_results(inFile$datapath)

    inFile <- input$csc1029_file
    x1029 <- read_module_results(inFile$datapath)

    inFile <- input$csc1030_file
    x1030 <- read_module_results(inFile$datapath)

    inFile <- input$csc1031_file
    x1031 <- read_module_results(inFile$datapath)


    #when user clicks generate action button  - as an if statement to input?
    #generate after read in - prevents error message appearing on screen about missing columns
    #input$generate

    table <- sem1_2_CS_path(qsis, x1023, x1028, x1029, x1030, x1031)
  })
  
  
  # # semester 3 CS data 
  data_s3_cs <- reactive ({


    repeats = list()

    #isolate reading in inputs for S1 SE/CS/CIT
    #return null to stop no file error on progress results output tab


    inFile <- input$student_upload
    qsis <- read_qsis_data(inFile$datapath)

    #semester one

    #csc1022
    if (is.null(input$csc1022_repeat_cs)){

    } else {
      inFile <- input$csc1022_repeat_cs
      x1022 <- read_module_results(inFile$datapath)
      repeats[["csc1022_resit"]] = x1022
    }


    #csc1025
    if (is.null(input$csc1025_repeat)){

    } else {
      inFile <- input$csc1025_repeat
      x1025 <- read_module_results(inFile$datapath)
      repeats[["csc1025_resit"]] = x1025
    }

    #csc1026
    if (is.null(input$csc1026_repeat)){

    } else {
      inFile <- input$csc1026_repeat
      x1026 <- read_module_results(inFile$datapath)
      repeats[["csc1026_resit"]] = x1026
    }


    #csc1027
    if (is.null(input$csc1027_repeat)){

    } else {
      inFile <- input$csc1027_repeat
      x1027 <- read_module_results(inFile$datapath)
      repeats[["csc1027_resit"]] = x1027
    }



    ##semester 2


    #csc1023
    if (is.null(input$csc1023_repeat)){

    } else {
      inFile <- input$csc1023_repeat
      x1023 <- read_module_results(inFile$datapath)
      repeats[["csc1023_resit"]] = x1023
    }


    #csc1028
    if (is.null(input$csc1028_repeat)){

    } else {
      inFile <- input$csc1028_repeat
      x1028 <- read_module_results(inFile$datapath)
      repeats[["csc1028_resit"]] = x1028
    }



    #csc1029
    if (is.null(input$csc1029_repeat)){

    } else {
      inFile <- input$csc1029_repeat
      x1029 <- read_module_results(inFile$datapath)
      repeats[["csc1029_resit"]] = x1029
    }



    #csc1030
    if (is.null(input$csc1030_repeat)){

    } else {
      inFile <- input$csc1030_repeat
      x1030 <- read_module_results(inFile$datapath)
      repeats[["csc1030_resit"]] = x1030
    }


    #csc1031
    if (is.null(input$csc1031_repeat)){

    } else {
      inFile <- input$csc1031_repeat
      x1031 <- read_module_results(inFile$datapath)
      repeats[["csc1031_resit"]] = x1031
    }



    table <- sem3_CS_path(qsis, repeats)


  })
  
  
  ################
  # BIT reactive data 
  ###################
  
  #reactive BIT s1 data
  data_bit_s1 <- reactive({
    
    
    
    inFile <- input$student_upload
    qsis <- read_qsis_data(inFile$datapath)
    
    
    
    #reading in acc1002
    inFile <- input$acc1022_file
    #validate(need(input$acc1002_file != "", "Please select a dataset for acc1022"))
    x1022 <- read_module_results(inFile$datapath)
    
    
    #reading in mgt1009
    inFile <- input$mgt1009_file
    #validate(need(input$mgt1009_file != "", "Please select a dataset for mgt1002"))
    x1009 <- read_module_results(inFile$datapath)
    
    
    
    table <- sem1_BIT_path(qsis, x1022, x1009)
    
    
  })
  
  
  #reactive BIT S2
  data_bit_s2 <- reactive({
    
  
      
      inFile <- input$student_upload
      # if (is.null(inFile))
      #   return(NULL)
      qsis <- read_qsis_data(inFile$datapath)
      
      inFile <- input$csc1023_file_bit
      # if (is.null(inFile))
      #   return(NULL)
      x1023 <- read_module_results(inFile$datapath)
      
      inFile <- input$csc1024_file
      # if (is.null(inFile))
      #   return(NULL)
      x1024 <- read_module_results(inFile$datapath)
      
      inFile <- input$eco1007_file
      # if (is.null(inFile))
      #   return(NULL)
      x1007 <- read_module_results(inFile$datapath)
      
      inFile <- input$mgt1012_file
      # if (is.null(inFile))
      #   return(NULL)
      x1012 <- read_module_results(inFile$datapath)
      
      inFile <- input$mgt1013_file
      # if (is.null(inFile))
      #   return(NULL)
      x1013 <- read_module_results(inFile$datapath)
      

    
    
    table <- sem2_BIT_path(qsis, x1023, x1024, x1007, x1012, x1013)
    
    
    
    
  })
  
  #reactive BIT S1 + S2 
  data_bit_s1_s2 <- reactive({
    
    #reading iinput files required
    inFile <- input$student_upload
    qsis <- read_qsis_data(inFile$datapath)
    
    inFile <- input$csc1023_file_bit
    x1023 <- read_module_results(inFile$datapath)
    
    inFile <- input$csc1024_file
    x1024 <- read_module_results(inFile$datapath)
    
    inFile <- input$eco1007_file
    x1007 <- read_module_results(inFile$datapath)
    
    inFile <- input$mgt1012_file
    x1012 <- read_module_results(inFile$datapath)
    
    inFile <- input$mgt1013_file
    x1013 <- read_module_results(inFile$datapath)
    
    
    #using function to create datatable 
    table <- sem1_2_BIT_path(qsis, x1023, x1024, x1007, x1012, x1013)
    
    
    
    
  })
  
  #reactive BIT S3  
  data_bit_s3 <- reactive ({
    
    
    repeats = list()
    
    
    
    ##QSIS FILE REQUIRED 
    inFile <- input$student_upload
    if (is.null(inFile))
      return(NULL)
    qsis <- read_qsis_data(inFile$datapath)
    
    
    # #semester one repeats
    
    #acc1002
    if (is.null(input$acc1022_repeat)){
      
    } else {
      inFile <- input$acc1022_repeat
      x1022 <- read_module_results(inFile$datapath)
      repeats[["acc1022_resit"]] = x1022
    }
    
    #mgt1009
    if (is.null(input$mgt1009_repeat)){
      
    } else {
      inFile <- input$mgt1009_repeat
      x1009 <- read_module_results(inFile$datapath)
      repeats[["mgt1009_resit"]] = x1009
    }
    
    
    # # #semester two repeats
    
    #csc1024
    if (is.null(input$csc1024_repeat)){
      
    } else {
      inFile = input$csc1024_repeat
      x1024 <- read_module_results(inFile$datapath)
      repeats[["csc1024_resit"]] = x1024
    }
    
    
    # # ##eco1007
    if(is.null(input$eco1007_repeat)){
      
    } else{
      inFile =input$eco1007_repeat
      x1007 <- read_module_results(inFile$datapath)
      repeats[["eco1007_resit"]] = x1007
    }
    
    
    #mgt1012
    if(is.null(input$mgt1012_repeat)){
      
    } else {
      inFile <- input$mgt1012_repeat
      x1012 <- read_module_results(inFile$datapath)
      repeats[["mgt1012_resit"]] = x1012
    }
    
    #CSC1023
    if (is.null(input$csc1023_repeat)){
      
    } else {
      inFile <- input$csc1023_repeat
      x1023 <- read_module_results(inFile$datapath)
      repeats[["csc1023_resit"]] = x1023
    }
    
    
    
    #mgt1013
    
    if (is.null(input$mgt1013_repeat)){
      
    } else {
      inFile <- input$mgt1013_repeat
      x1013 <- read_module_results(inFile$datapath)
      repeats[["mgt1013_resit"]] = x1013
    }
    
    
    
    
    table<- sem3_BIT_path(qsis, repeats)
    
  
  })
  
  
  
  #############################´
  
  # Semester one - generating results to a table
  
  ##################################
  
  #Semester one - CS/SE/CIT
  generate_s1_cs = reactive(
    {
 
        # inFile <- input$student_upload
        # qsis <- read_qsis_data(inFile$datapath)
        # 
        # inFile <- input$csc1022_file
        # x1022 <- read_module_results(inFile$datapath)
        # 
        # inFile <- input$csc1025_file
        # x1025 <- read_module_results(inFile$datapath)
        # 
        # inFile <- input$csc1026_file
        # x1026 <- read_module_results(inFile$datapath)
        # 
        # inFile <- input$csc1027_file
        # x1027 <- read_module_results(inFile$datapath)
        # 
        # #adding files to Sem1 function
        # table <- sem1_CS_path(qsis, x1022, x1025, x1026, x1027)
        # 
        # #repalcing NA values
        # table[is.na(table)] <- '-'
        
        table <- data_s1_cs()  
        table[is.na(table)] = "-"
      
        colnames(table)[2] = "First Name"
        colnames(table)[4] = "Mark CSC1022"
        colnames(table)[5] = "Grade CSC1022"
        colnames(table)[6] = "Mark CSC1025"
        colnames(table)[7] = "Grade CSC1025"
        colnames(table)[8] = "Mark CSC1026"
        colnames(table)[9] = "Grade CSC1026"
        colnames(table)[10] = "Mark CSC1027"
        colnames(table)[11] = "Grade CSC1027"
    
      

      #changing table into a formattable table
      table <- formattable(
        table,
        list(

          `Year_Avg` = year_average_formatter,
          `Progressed?` = progression_formatter,
          `Module-Fail` = color_tile("transparent", customOrange),
          `CAT-Fail` = color_tile("transparent", customOrange),
          `Invite To SSM` = invite_meeting_formatter,
          `Grade CSC1022` =  grade_formatter,
          `Grade CSC1025` =  grade_formatter,
          `Grade CSC1026` =  grade_formatter,
          `Grade CSC1027` =  grade_formatter,
          `Mark CSC1022` =  failed_mods_formatter,
          `Mark CSC1025` =  failed_mods_formatter,
          `Mark CSC1026` =  failed_mods_formatter,
          `Mark CSC1027` =  failed_mods_formatter
  
        )
      ) %>% #converting table back to DT table
        as.datatable(table,
                     
                     # buttons styling
                     callback=JS('$("button.buttons-copy").css("background","#8FBC8F"); 
                          $("button.buttons-print").css("background","#FFFACD"); 
                          $("button.buttons-excel").css("background","#8FBC8F"); 
                          $("button.buttons-csv").css("background","#FFFACD"); 
                          return table;'),
                     
                     #Determines whether the HTML entities in the table are escaped or not
                     escape = FALSE,
                     
                     #fixed 2 first columns, buttons and fixed header
                     extensions= c('FixedColumns', 'Buttons', 'FixedHeader'),
          
                     #filter = 'top',
                     
                     
                      options = list(
                       fixedColumns = list(leftColumns = 2),
                       fixedHeader = TRUE,
                       
                       #download table options
                       dom = "Bfrtip",
                   
                       buttons = list(
                         list(extend = "csv", filename = paste(currentDate, 'Stage 1 Queens CS-SE-CIT  Semester 1 Results', sep = " ")),
                         list(extend = "excel", filename = paste(currentDate, 'Stage 1 Queens CS-SE-CIT Semester 1 Results', sep = " "))
                       ), 
                       
                       scrollX = TRUE,
                
                       
                      columnDefs = list(list(className = 'dt-center', targets = c(3:10, 14:17,19)),
                                        list(width = '500px', targets = "_all")
                                        )
                      ),
                
                     rownames = FALSE
                     
        ) %>% DT::formatStyle(columns = c("Grade CSC1022", "Grade CSC1025", "Grade CSC1026", "Grade CSC1027"),
                                    backgroundColor = "#a3c2c2") %>% DT::formatStyle(columns =
                                                                                       c("Mark CSC1022", "Mark CSC1025", "Mark CSC1026", "Mark CSC1027"), backgroundColor = "#80d4ff")
      
      
      return(table)
      
    }
  )
  
  #Semster one  - BIT
  generate_s1_bit = reactive(
    {
 
      # inFile <- input$student_upload
      # qsis <- read_qsis_data(inFile$datapath)
      # 
      # 
      # 
      # #reading in acc1002
      # inFile <- input$acc1022_file
      # #validate(need(input$acc1002_file != "", "Please select a dataset for acc1022"))
      # x1022 <- read_module_results(inFile$datapath)
      # 
      # 
      # #reading in mgt1009
      # inFile <- input$mgt1009_file
      # #validate(need(input$mgt1009_file != "", "Please select a dataset for mgt1002"))
      # x1009 <- read_module_results(inFile$datapath)
      # 


      #table <- sem1_BIT_path(qsis, x1022, x1009)
      
      
      table <- data_bit_s1()
      
      table[is.na(table)] = "-"
      

      colnames(table)[4] = "Mark ACC1022"
      colnames(table)[5] = "Grade ACC1022"
      colnames(table)[6] = "Mark MGT1009"
      colnames(table)[7] = "Grade MGT1009"
      
      #creating header names
      heading <- colnames(table)
      
   
      #sketch for heading of final output table S1 CS -NOT WORKING
      table_heading = htmltools::withTags(table(class = 'display',
                                                
                                                initComplete = JS(
                                                  "function(settings, json) {",
                                                  "$(this.api().table().header()).css({'background-color': 'red', 'color': 'white'});",
                                                  "}"),
                                               
                                                # tr(
                                                #   th(rowspan = 2, ''),
                                                #   th(colspan = 4, ''),
                                                #   th(colspan = 1, '20'),
                                                #   th(colspan = 1, 'Grade'),
                                                #   th(colspan = 1, '20'),
                                                #   th(colspan = 1, 'Grade'),
                                                #   th(colspan = 1, '20'),
                                                #   th(colspan = 1, 'Grade'),
                                                #   th(colspan = 1, '20'),
                                                #   th(colspan = 1, 'Grade')
                                                # )
                                                #
                                                
                                                thead(#Heading of col names provided from data
                                                  
                                                  # tr(
                                                  #   
                                                  #   th(colspan = 3, ''),
                                                  #   th(colspan = 2, 'S1'),
                                                  #   th(colspan = 2, 'S1'),
                                                  # ),
                                                  # 
                                                  
                                                  #to apply heading to table from data
                                                  tr(lapply(
                                                    rep(heading, 1), th
                                                  )),
                                                  
                                                  
                                                  
                                                  
                                                  #https://stackoverflow.com/questions/52151312/dt-shiny-different-custom-column-header-by-column
                                                  
                                                )
      )
      )
      
      
      
      table <- formattable(
        table,
        list(
          `Year_Avg` = year_average_formatter,
          `Progressed?` = progressionFormatter2,
          `Module-Fail` = color_tile("transparent", customOrange),
          `CAT-Fail` = color_tile("transparent", customOrange),
          `Invite To SSM` = invite_meeting_formatter,
          `Grade ACC1022` = grade_formatter,
          `Grade MGT1009` = grade_formatter,
          `Mark ACC1022` = failed_mods_formatter,
          `Mark MGT1009` = failed_mods_formatter
        )
        
      )  %>%
        as.datatable(table,
                     #caption = 'Stage 1 BIT Semester 1',
                     
                     #   DT::formatStyle(columns = "Petal.Width", backgroundColor = "#33aa3388")
                     # 
                     # 
                     
                     # formatStyle(columns = "Grade ACC1022", backgroundColor = "#33aa33"), 
                     # 
                     callback=JS('
                          $("button.buttons-excel").css("background","#8FBC8F"); 
                          $("button.buttons-csv").css("background","#FFFACD"); 
                          return table;'),
                     
                  
                     
                     #creating header
                     escape = FALSE,
                     extensions= c('FixedColumns', 'Buttons'),
                      #container = table_heading,
                     
                     
                     #creating column filters
                     filter = "top",
                     
                     options = list(
                       

                       scrollX = TRUE,
                       
                  
                       
                        columnDefs = list(list(className = 'dt-center', targets = c(3:6, 10:13, 15))),
                       fixedColumns = list(leftColumns = 2),
                       
                       #download table options
                       dom = "Bfrtip",
                       # buttons = list('copy', 'csv', 'excel', 'print', list(
                       #   extend = "csv", title = "Stage 1 BIT Semester 1"
                       # )),
                       
                       buttons = list(
                         list(extend = "csv", filename = paste(currentDate, 'Stage 1 BIT Semester 1 Results', sep = " ")),
                          list(extend = "excel", filename = paste(currentDate, 'Stage 1 BIT Semester 1 Results', sep = " "))
                    
                       )
                       
                       #scrollX = TRUE
                       
                     ),
                     rownames = FALSE) %>% DT::formatStyle(columns = c("Grade ACC1022", "Grade MGT1009"), 
                                                           backgroundColor = "#ADD8E6") %>% DT::formatStyle(columns = 
                                                          c("Mark ACC1022", "Mark MGT1009"), backgroundColor = "#D8BFD8")  ##F0FFF0
      
      
      return(table)
      
    }
  )
  
  ########################
  #SEMESTER 2 ONLY
  ########################
  
  #Semester 2 SE/CS/CIT table
  generate_s2_cs = reactive(
    {

      table <- data_s2_cs()
      
      table[is.na(table)] = "-"
      
      #DT[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
      #table[,lapply(.SD, function(x){ifelse(is.na(x), '-',x)})]
      
      colnames(table)[4] = "Mark CSC1023"
      colnames(table)[5] = "Grade CSC1023"
      
      colnames(table)[6] = "Mark CSC1028"
      colnames(table)[7] = "Grade CSC1028"
      
      colnames(table)[8] = "Mark CSC1029"
      colnames(table)[9] = "Grade CSC1029"
      
      colnames(table)[10] = "Mark CSC1030"
      colnames(table)[11] = "Grade CSC1030"
      
      colnames(table)[12] = "Mark CSC1031"
      colnames(table)[13] = "Grade CSC1031"
      
      
      #formattable table
      table <- formattable(
        table,
        list(
          
          `Year_Avg` = year_average_formatter,
          `Progressed?` = progression_formatter,
          `Module-Fail` = color_tile("transparent", customOrange),
          `CAT-Fail` = color_tile("transparent", customOrange),
          `Invite To SSM` = invite_meeting_formatter,
          
          #`Mark CSC1022` = color_tile(customLow, customHigh),
          #`Mark CSC1022` = color_bar(customGreen, customGreen0),
          
          
          `Grade CSC1023` =  grade_formatter,
          `Grade CSC1028` =  grade_formatter,
          `Grade CSC1029` =  grade_formatter,
          `Grade CSC1030` =  grade_formatter,
          `Grade CSC1031` =  grade_formatter,
          
          
          `Mark CSC1023` =  failed_mods_formatter,
          `Mark CSC1028` =  failed_mods_formatter,
          `Mark CSC1029` =  failed_mods_formatter,
          `Mark CSC1030` =  failed_mods_formatter,
          `Mark CSC1031` =  failed_mods_formatter
          
        )
      ) %>%
        as.datatable(table,
                     
                     callback=JS('$("button.buttons-copy").css("background","#8FBC8F");
                          $("button.buttons-print").css("background","#FFFACD");
                          $("button.buttons-excel").css("background","#8FBC8F");
                          $("button.buttons-csv").css("background","#FFFACD");
                          return table;'),
                     
                     
                     escape = FALSE,
                     extensions= c('FixedColumns', 'Buttons', 'FixedHeader'),
                     #container = table_heading_cs_s2,
                     
                     #creating column filters
                     filter = "top",
                     
                     
                     options = list(scrollX = TRUE,
                                    
                                    columnDefs = list(list(className = 'dt-center', targets = c(3:13, 16:19, 21))),
                                    #columnDefs = list(list(width = '500px', targets = "_all")),
                                    
                                    
                                    fixedColumns = list(leftColumns = 2),
                                    #fixedHeader = TRUE,
                                    
                                    
                                    
                                    ##datatables start at index 0
                                    #columnDefs = list(list(className = 'dt-center', targets = c(3:13, 17:10, 22))),
                                    #columnDefs = list(list(width = '500px', targets = "_all")),
                                    
                                    ##options for download buttons DT extension
                                    dom = "Bfrtip",
                                    
                                    buttons = list(
                                      list(extend = "csv", filename = paste(currentDate, 'Stage 1 Queens CS-SE-CIT  Semester 2 Results', sep = " ")),
                                      list(extend = "excel", filename = paste(currentDate, 'Stage 1 Queens CS-SE-CIT Semester 2 Results', sep = " "))
                                      
                                    )
                     ),
                     # container = sketch,
                     rownames = FALSE)  %>% DT::formatStyle(columns = c("Grade CSC1023", "Grade CSC1028", "Grade CSC1029", "Grade CSC1030", "Grade CSC1031"), 
                                                            backgroundColor = "#fff6e9") %>% DT::formatStyle(columns =
                                                                                                               c("Mark CSC1023", "Mark CSC1028", "Mark CSC1029", "Mark CSC1030", "Mark CSC1031"), backgroundColor = "#d2e7ff") 
      
      
      return(table)
    }
  )
  
  
  #Semester 2 BIT table
  generate_s2_bit = reactive(
    {

      table <- data_bit_s2()
      table[is.na(table)] = "-"
    
      colnames(table)[4] = "Mark CSC1023"
      colnames(table)[6] = "Mark CSC1024"
      colnames(table)[8]  = "Mark ECO1007"
      colnames(table)[10]  = "Mark MGT1012"
      colnames(table)[12]  = "Mark MGT1013"

      colnames(table)[5] = "Grade CSC1023"
      colnames(table)[7] = "Grade CSC1024"
      colnames(table)[9]  = "Grade ECO1007"
      colnames(table)[11]  = "Grade MGT1012"
      colnames(table)[13]  = "Grade MGT1013"
      
      
      table <- formattable(
        table,
        
        list(
          
    
          #`Year_Avg` = year_average_formatter,
          `Progressed?` = progression_formatter,
          `Module-Fail` = color_tile("transparent", customOrange),
          `CAT-Fail` = color_tile("transparent", customOrange),
          `Invite To SSM` = invite_meeting_formatter,
          `Grade CSC1023` =  grade_formatter,
          `Grade CSC1024` = grade_formatter,
          `Grade ECO1007` = grade_formatter,
          `Grade MGT1012` = grade_formatter,
          `Grade MGT1013` = grade_formatter,
          `Mark CSC1023` = failed_mods_formatter,
          `Mark CSC1024` = failed_mods_formatter,
          `Mark ECO1007`= failed_mods_formatter,
          `Mark MGT1012` = failed_mods_formatter,
          `Mark MGT1013` = failed_mods_formatter
        
        )
      ) %>%
        as.datatable(table,
                     
                     # buttons styling
                     callback=JS('$("button.buttons-copy").css("background","#8FBC8F"); 
                          $("button.buttons-print").css("background","#FFFACD"); 
                          $("button.buttons-excel").css("background","#8FBC8F"); 
                          $("button.buttons-csv").css("background","#FFFACD"); 
                          return table;'),
                     
                     
                     
                     escape = FALSE,
                     extensions= c('FixedColumns', 'Buttons'),
                     filter = 'top',
                     #container = final_heading_bit,
                     
                     options = list(scrollX = TRUE,
                                    
                                    
                                    
                                    fixedColumns = list(leftColumns = 2),
                                    
                                    columnDefs = list(list(className = 'dt-center', targets = c(3:12, 16:19, 21))),
                                    
                                    
                                    
                                    #download table options
                                    dom = "Bfrtip",
                                    buttons = list(
                                      list(extend = "csv", filename = paste(currentDate, 'Stage 1 BIT Semester 2 Results', sep = " ")),
                                      list(extend = "excel", filename = paste(currentDate, 'Stage 1 BIT Semester 2 Results', sep = " "))
                                      
                                    )
                     ),# container = sketch,
                     rownames = FALSE,
        )
      
      return(table)
    }
  )
  
  
  ############################
  # SEMESTER 1 +2 
  #############################
  
  
  generate_s1_s2_cs = reactive(
    {
    
        
        # inFile <- input$student_upload
        # # if (is.null(inFile))
        # #   return(NULL)
        # qsis <- read_qsis_data(inFile$datapath)
        # 
        # inFile <- input$csc1023_file
        # x1023 <- read_module_results(inFile$datapath)
        # 
        # inFile <- input$csc1028_file
        # x1028 <- read_module_results(inFile$datapath)
        # 
        # inFile <- input$csc1029_file
        # x1029 <- read_module_results(inFile$datapath)
        # 
        # inFile <- input$csc1030_file
        # x1030 <- read_module_results(inFile$datapath)
        # 
        # inFile <- input$csc1031_file
        # x1031 <- read_module_results(inFile$datapath)

      
      #when user clicks generate action button  - as an if statement to input?
      #generate after read in - prevents error message appearing on screen about missing columns
      #input$generate
      
      
     # table <- sem1_2_CS_path(qsis, x1023, x1028, x1029, x1030, x1031)
      
      
      
      # if (is.null(table) ) {
      #   return("No data found - recheck inputs and if correct, please contact administrator of system")
      # } else {
      #   
      
      table <- data_s1_s2_cs()
      table[is.na(table)] = "-"
      
      #table[is.na(table)] = "-"
      
      colnames(table)[4] = "Mark CSC1022"
      colnames(table)[5] = "Grade CSC1022"
      
      colnames(table)[6] = "Mark CSC1025"
      colnames(table)[7] = "Grade CSC1025"
      
      colnames(table)[8] = "Mark CSC1026"
      colnames(table)[9] = "Grade CSC1026"
      
      colnames(table)[10] = "Mark CSC1027"
      colnames(table)[11] = "Grade CSC1027"
      
      colnames(table)[12] = "Mark CSC1023"
      colnames(table)[13] = "Grade CSC1023"
      
      colnames(table)[14] = "Mark CSC1028"
      colnames(table)[15] = "Grade CSC1028"
      
      colnames(table)[16] = "Mark CSC1029"
      colnames(table)[17] = "Grade CSC1029"
      
      colnames(table)[18] = "Mark CSC1030"
      colnames(table)[19] = "Grade CSC1030"
      
      colnames(table)[20] = "Mark CSC1031"
      colnames(table)[21] = "Grade CSC1031"
      
      
      # #creating header names
      heading <- colnames(table)
      #
      # #sketch for heading of final output table S1 CS -NOT WORKING
      
      table_heading = htmltools::withTags(table(class = 'display',
                                                thead(#Heading of col names provided from data
                                                  
                                                  tr(
                                                    
                                                    th(colspan = 5),
                                                    th(colspan = 2, 'S1'),
                                                    th(colspan = 2, 'S1'),
                                                    th(colspan = 2, 'S1'),
                                                    th(colspan = 2, 'S1'),
                                                    
                                                  ),
                                                  #
                                                  #                                      tr(
                                                  #                                        th(rowspan = 2, ''),
                                                  #                                        th(colspan = 4, ''),
                                                  #                                        th(colspan = 1, '20'),
                                                  #                                        th(colspan = 1, 'Grade'),
                                                  #                                        th(colspan = 1, '20'),
                                                  #                                        th(colspan = 1, 'Grade'),
                                                  #                                        th(colspan = 1, '20'),
                                                  #                                        th(colspan = 1, 'Grade'),
                                                  #                                        th(colspan = 1, '20'),
                                                  #                                        th(colspan = 1, 'Grade')
                                                  #                                      ),
                                                  #
                                                  #                                      #to apply heading to table from data
                                                  tr(lapply(
                                                    rep(heading, 1), th
                                                  )),
                                                  #
                                                  #                                      #https://stackoverflow.com/questions/52151312/dt-shiny-different-custom-column-header-by-column
                                                  #
                                                )))
      
      #
      #
      #
      # #new code 22/03 - testing formatable
      #
      table <- formattable(
        table,
        list(
          
          #align = c("l", "l", "l","c", "c","c","c","c","c","c", "c"),
          #`ID` = formatter("span", style = ~ style(color = "blue", font.weight = "bold")),
          
          `Year_Avg` = year_average_formatter,
          `Progressed?` = progression_formatter,
          `Module-Fail` = color_tile("transparent", customOrange),
          `CAT-Fail` = color_tile("transparent", customOrange),
          `Invite To SSM` = invite_meeting_formatter,
    
          
          `Grade CSC1022` =  grade_formatter,
          `Grade CSC1023` =  grade_formatter,
          `Grade CSC1025` =  grade_formatter,
          `Grade CSC1026` =  grade_formatter,
          `Grade CSC1027` =  grade_formatter,
          `Grade CSC1028` =  grade_formatter,
          `Grade CSC1029` =  grade_formatter,
          `Grade CSC1030` =  grade_formatter,
          `Grade CSC1031` =  grade_formatter,
          
          `Mark CSC1022` =  failed_mods_formatter,
          `Mark CSC1023` =  failed_mods_formatter,
          `Mark CSC1025` =  failed_mods_formatter,
          `Mark CSC1026` =  failed_mods_formatter,
          `Mark CSC1027` =  failed_mods_formatter,
          `Mark CSC1028` =  failed_mods_formatter,
          `Mark CSC1029` =  failed_mods_formatter,
          `Mark CSC1030` =  failed_mods_formatter,
          `Mark CSC1031` =  failed_mods_formatter
          
        )
      ) %>%
        as.datatable(table,
                     
                     callback=JS('$("button.buttons-copy").css("background","#8FBC8F");
                          $("button.buttons-print").css("background","#FFFACD");
                          $("button.buttons-excel").css("background","#8FBC8F");
                          $("button.buttons-csv").css("background","#FFFACD");
                          return table;'),
                     
                     
                     escape = FALSE,
                     extensions= c('FixedColumns', 'Buttons', 'FixedHeader'),
                     #container = table_heading_cs_s1,
                     
                     #creating column filters
                     filter = "top",
                     
                     
                     #
                     options = list(scrollX = TRUE,
                                    
                                    fixedColumns = list(leftColumns = 2),
                                    fixedHeader = TRUE,
                                    
                                    
                                    
                                    ##datatables start at index 0
                                    columnDefs = list(list(className = 'dt-center', targets = c(3:20, 24:27, 29))),
                                    
                                    ##options for download buttons DT extension
                                    dom = "Bfrtip",
                                 
                                    buttons = list(
                                      list(extend = "csv", filename = paste(currentDate, 'Stage 1 Queens CS-SE-CIT  Semester 1 & 2 Results', sep = " ")),
                                      list(extend = "excel", filename = paste(currentDate, 'Stage 1 Queens CS-SE-CIT Semester 1 & 2 Results', sep = " "))
                                      
                                    )
                                  
                     ), 
                     rownames = FALSE) 
    
      
      return(table)
      }
    # }
  )
  
  generate_s1_2_bit = reactive(
    {
   
      
      

     
      
      #when user clicks generate action button  - as an if statement to input? 
      #generate after read in - prevents error message appearing on screen about missing columns 
      #input$generate
      
      table <- data_bit_s1_s2()
      
      table[is.na(table)] = "-"
      
      
      colnames(table)[4] = "Mark ACC1022"
      colnames(table)[6] = "Mark MGT1009"
      colnames(table)[8] = "Mark CSC1023"
      colnames(table)[10] = "Mark CSC1024"
      colnames(table)[12]  = "Mark ECO1007"
      colnames(table)[14]  = "Mark MGT1012"
      colnames(table)[16]  = "Mark MGT1013"
      colnames(table)[5] = "Grade ACC1022"
      colnames(table)[7] = "Grade MGT1009"
      colnames(table)[9] = "Grade CSC1023"
      colnames(table)[11] = "Grade CSC1024"
      colnames(table)[13]  = "Grade ECO1007"
      colnames(table)[15]  = "Grade MGT1012"
      colnames(table)[17]  = "Grade MGT1013"
      
      #creating header names
      heading <- colnames(table)
      
      
      #sketch for heading of final output table S1 CS -NOT WORKING
      final_heading_bit = htmltools::withTags(table(class = 'display',
                                                    
                                                    
                                                    thead(#Heading of col names provided from data
                                                      
                                                      tr(
                                                        
                                                        th(colspan = 3),
                                                        th(colspan = 2, 'S1'),
                                                        th(colspan = 2, 'S1'),
                                                        th(colspan = 2, 'S2'),
                                                        th(colspan = 2, 'S2'),
                                                        th(colspan = 2, 'S2'),
                                                        th(colspan = 2, 'S2'),
                                                        th(colspan = 2, 'S2')
                                                        
                                                      ),
                                                      
                                                      
                                                      #to apply heading to table from data
                                                      tr(lapply(
                                                        rep(heading, 1), th
                                                      )),
                                                      
                                                      
                                                      
                                                      
                                                      #https://stackoverflow.com/questions/52151312/dt-shiny-different-custom-column-header-by-column
                                                      
                                                    )
      )
      )
      
      
      table <- formattable(
        table,
        
        list(
          
          #align = c("l", "l", "l","c", "c","c","c","c","c","c", "c"),
          #`ID` = formatter("span", style = ~ style(color = "blue", font.weight = "bold")),
          
          #`Year_Avg` = year_average_formatter,
          `Progressed?` = progression_formatter,
          `Module-Fail` = color_tile("transparent", customOrange),
          `CAT-Fail` = color_tile("transparent", customOrange),
          `Invite To SSM` = invite_meeting_formatter,
          `Grade ACC1022` = grade_formatter,
          `Grade MGT1009` = grade_formatter,
          `Grade CSC1023` =  grade_formatter,
          `Grade CSC1024` = grade_formatter,
          `Grade ECO1007` = grade_formatter,
          `Grade MGT1012` = grade_formatter,
          `Grade MGT1013` = grade_formatter,
          
          
          `Mark ACC1022` = failed_mods_formatter,
          `Mark MGT1009` = failed_mods_formatter,
          `Mark CSC1023` = failed_mods_formatter,
          `Mark CSC1024` = failed_mods_formatter,
          `Mark ECO1007`= failed_mods_formatter,
          `Mark MGT1012` = failed_mods_formatter,
          `Mark MGT1013` = failed_mods_formatter
          
          
          
          
        )
      ) %>%
        as.datatable(table,
                     
                     # buttons styling
                     callback=JS('$("button.buttons-copy").css("background","#8FBC8F"); 
                          $("button.buttons-print").css("background","#FFFACD"); 
                          $("button.buttons-excel").css("background","#8FBC8F"); 
                          $("button.buttons-csv").css("background","#FFFACD"); 
                          return table;'),
                     
                     
                     
                     escape = FALSE,
                     extensions= c('FixedColumns', 'Buttons'),
                     filter = 'top',
                     container = final_heading_bit,
                     
                     options = list(scrollX = TRUE,
                                    
                                    fixedColumns = list(leftColumns = 2),
                                    
                                    #columnDefs = list(list(className = 'dt-center', targets = c(3:17, 21:24, 26))),
                                    columnDefs = list(list(className = 'dt-center', targets = c(2:16, 20:23, 25))),
                                    
                                    
                                    #download table options
                                    dom = "Bfrtip",
                                    
                                    buttons = list(
                                      list(extend = "csv", filename = paste(currentDate, 'Stage 1 Queens BIT  Semester 1 & 2 Results', sep = " ")),
                                      list(extend = "excel", filename = paste(currentDate, 'Stage 1 Queens BIT Semester 1 & 2 Results', sep = " "))
                                      
                                    )
                                    
                     ),# container = sketch,
                     rownames = FALSE,
        )
      
      return(table)
    }
  )
  
  
  ######################
  # SEMESTER 3 ONLY
  #######################
  

  generate_s3_bit = reactive(
    {
      
      
      
      table <-   data_bit_s3()
  
      table[is.na(table)] = "-"
      
      colnames(table)[4] = "Mark ACC1022"
      colnames(table)[6] = "Mark MGT1009"
      colnames(table)[8] = "Mark CSC1023"
      colnames(table)[10] = "Mark CSC1024"
      colnames(table)[12]  = "Mark ECO1007"
      colnames(table)[14]  = "Mark MGT1012"
      colnames(table)[16]  = "Mark MGT1013"
      colnames(table)[5] = "Grade ACC1022"
      colnames(table)[7] = "Grade MGT1009"
      colnames(table)[9] = "Grade CSC1023"
      colnames(table)[11] = "Grade CSC1024"
      colnames(table)[13]  = "Grade ECO1007"
      colnames(table)[15]  = "Grade MGT1012"
      colnames(table)[17]  = "Grade MGT1013"
      
      #creating header names
      heading <- colnames(table)

      #sketch for heading of final output table S1 CS -NOT WORKING
      final_heading_bit = htmltools::withTags(table(class = 'display',
                                  
                                                
                                                thead(
                                                  
                                                  tr(
                                                    
                                                    th(colspan = 3),
                                                    th(colspan = 2, 'S1'),
                                                    th(colspan = 2, 'S1'),
                                                    th(colspan = 2, 'S2'),
                                                    th(colspan = 2, 'S2'),
                                                    th(colspan = 2, 'S2'),
                                                    th(colspan = 2, 'S2'),
                                                    th(colspan = 2, 'S2')
                                                    
                                                  ),
                                                  
                                                  
                                                  #to apply heading to table from data
                                                  tr(lapply(
                                                    rep(heading, 1), th
                                                  )),
                                                  
                                        
                                                )
      )
      )
   
      table <- formattable(
        table,
 
        list(

         
          `Year_Avg` = year_average_formatter,
          `Progressed?` = progression_formatter,
          `Module-Fail` = color_tile("transparent", customOrange),
          `CAT-Fail` = color_tile("transparent", customOrange),
          `Invite To SSM` = invite_meeting_formatter,
          `Grade ACC1022` = grade_formatter,
          `Grade MGT1009` = grade_formatter,
          `Grade CSC1023` =  grade_formatter,
          `Grade CSC1024` = grade_formatter,
          `Grade ECO1007` = grade_formatter,
          `Grade MGT1012` = grade_formatter,
          `Grade MGT1013` = grade_formatter
        
        )
      ) %>%
        as.datatable(table,
                     
                     # buttons styling
                     callback=JS('$("button.buttons-copy").css("background","#8FBC8F"); 
                          $("button.buttons-print").css("background","#FFFACD"); 
                          $("button.buttons-excel").css("background","#8FBC8F"); 
                          $("button.buttons-csv").css("background","#FFFACD"); 
                          return table;'),
                     
                     
                     
                     escape = FALSE,
                     extensions= c('FixedColumns', 'Buttons'),
                     filter = 'top',
                      container = final_heading_bit,
                     
                     options = list(
                       
                       
                       scrollX = TRUE,
                                    
                           
                                 
                                    
                                    columnDefs = list(list(className = 'dt-center', targets = c(2:16, 20:23, 25))),
                                    fixedColumns = list(leftColumns = 2),
                              
                                    
                                    #download table options
                                    dom = "Bfrtip",
                                    
                                    buttons = list(
                                      list(extend = "csv", filename = paste(currentDate, 'Stage 1 Queens BIT Semester 3 Summer Results', sep = " ")),
                                      list(extend = "excel", filename = paste(currentDate, 'Stage 1 Queens BIT Semester 3 Summer Results', sep = " "))
                                      
                                    )
                                    ),
                     rownames = FALSE,
        )
      
      return(table)
    }
  )

  
  generate_s3_cs = reactive(
    {
      
      
      # repeats = list()
      # 
      # #isolate reading in inputs for S1 SE/CS/CIT
      # #return null to stop no file error on progress results output tab
      # 
      # 
      #   inFile <- input$student_upload
      #   qsis <- read_qsis_data(inFile$datapath)
      # 
      #   #semester one
      # 
      #   #csc1022
      #   if (is.null(input$csc1022_repeat_cs)){
      # 
      #   } else {
      #     inFile <- input$csc1022_repeat_cs
      #     x1022 <- read_module_results(inFile$datapath)
      #     repeats[["csc1022_resit"]] = x1022
      #   }
      # 
      # 
      #   #csc1025
      #   if (is.null(input$csc1025_repeat)){
      # 
      #   } else {
      #     inFile <- input$csc1025_repeat
      #     x1025 <- read_module_results(inFile$datapath)
      #     repeats[["csc1025_resit"]] = x1025
      #   }
      # 
      #   #csc1026
      #   if (is.null(input$csc1026_repeat)){
      # 
      #   } else {
      #     inFile <- input$csc1026_repeat
      #     x1026 <- read_module_results(inFile$datapath)
      #     repeats[["csc1026_resit"]] = x1026
      #   }
      # 
      # 
      #   #csc1027
      #   if (is.null(input$csc1027_repeat)){
      # 
      #   } else {
      #     inFile <- input$csc1027_repeat
      #     x1027 <- read_module_results(inFile$datapath)
      #     repeats[["csc1027_resit"]] = x1027
      #   }
      # 
      # 
      # 
      #   ##semester 2
      # 
      # 
      #   #csc1023
      #   if (is.null(input$csc1023_repeat)){
      # 
      #   } else {
      #     inFile <- input$csc1023_repeat
      #     x1023 <- read_module_results(inFile$datapath)
      #     repeats[["csc1023_resit"]] = x1023
      #   }
      # 
      # 
      #   #csc1028
      #   if (is.null(input$csc1028_repeat)){
      # 
      #   } else {
      #     inFile <- input$csc1028_repeat
      #     x1028 <- read_module_results(inFile$datapath)
      #     repeats[["csc1028_resit"]] = x1028
      #   }
      # 
      # 
      # 
      #   #csc1029
      #   if (is.null(input$csc1029_repeat)){
      # 
      #   } else {
      #     inFile <- input$csc1029_repeat
      #     x1029 <- read_module_results(inFile$datapath)
      #     repeats[["csc1029_resit"]] = x1029
      #   }
      # 
      # 
      # 
      #   #csc1030
      #   if (is.null(input$csc1030_repeat)){
      # 
      #   } else {
      #     inFile <- input$csc1030_repeat
      #     x1030 <- read_module_results(inFile$datapath)
      #     repeats[["csc1030_resit"]] = x1030
      #   }
      # 
      # 
      #   #csc1031
      #   if (is.null(input$csc1031_repeat)){
      # 
      #   } else {
      #     inFile <- input$csc1031_repeat
      #     x1031 <- read_module_results(inFile$datapath)
      #     repeats[["csc1031_resit"]] = x1031
      #   }


      #
      #     #when user clicks generate action button  - as an if statement to input?
      #     #generate after read in - prevents error message appearing on screen about missing columns
      #     #input$generate

      # table <- sem3_CS_path(qsis, repeats)

      
      table <- data_s3_cs()
      
      colnames(table)[4] = "Mark CSC1022"
      colnames(table)[6] = "Mark CSC1025"
      colnames(table)[8] = "Mark CSC1026"
      colnames(table)[10] = "Mark CSC1027"
      colnames(table)[5] = "Grade CSC1022"
      colnames(table)[7] = "Grade CSC1025"
      colnames(table)[9] = "Grade CSC1026"
      colnames(table)[11] = "Grade CSC1027"
      colnames(table)[12] = "Mark CSC1023"
      colnames(table)[13] = "Grade CSC1023"
      colnames(table)[14] = "Mark CSC1028"
      colnames(table)[15] = "Grade CSC1028"
      colnames(table)[16] = "Mark CSC1029"
      colnames(table)[17] = "Grade CSC1029"
      colnames(table)[18] = "Mark CSC1030"
      colnames(table)[19] = "Grade CSC1030"
      colnames(table)[20] = "Mark CSC1031"
      colnames(table)[21] = "Grade CSC1031"
      
      
      
      
      
      #creating header names
      heading <- colnames(table)
      
      
      #sketch for heading of final output table S1 CS -NOT WORKING
      cs_s3_heading = htmltools::withTags(table(class = 'display',
                                                thead(#Heading of col names provided from data
                                                  
                                                  tr(
                                                    
                                                    th(colspan = 3),
                                                    th(colspan = 2, 'S1'),
                                                    th(colspan = 2, 'S1'),
                                                    th(colspan = 2, 'S1'),
                                                    th(colspan = 2, 'S1'),
                                                    th(colspan = 2, 'S2'),
                                                    th(colspan = 2, 'S2'),
                                                    th(colspan = 2, 'S2'),
                                                    th(colspan = 2, 'S2'),
                                                    th(colspan = 2, 'S2'),
                                                    
                                                  ),
                                                  
                                                  # tr(
                                                  #   th(rowspan = 2, ''),
                                                  #   th(colspan = 4, ''),
                                                  #   th(colspan = 1, '20'),
                                                  #   th(colspan = 1, 'Grade'),
                                                  #   th(colspan = 1, '20'),
                                                  #   th(colspan = 1, 'Grade'),
                                                  #   th(colspan = 1, '20'),
                                                  #   th(colspan = 1, 'Grade'),
                                                  #   th(colspan = 1, '20'),
                                                  #   th(colspan = 1, 'Grade')
                                                  # ),
                                                  
                                                  #to apply heading to table from data
                                                  tr(lapply(
                                                    rep(heading, 1), th
                                                  )),
                                                  
                                                  #https://stackoverflow.com/questions/52151312/dt-shiny-different-custom-column-header-by-column
                                                  
                                                )))
      #     #
      #     #
      #     #
      #     #
      #     # #new code 22/03 - testing formatable
      #     #
      table <- formattable(
        table,
        list(
          
          #align = c("l", "l", "l","c", "c","c","c","c","c","c", "c"),
          #`ID` = formatter("span", style = ~ style(color = "blue", font.weight = "bold")),
          
          `Year_Avg` = year_average_formatter,
          `Progressed?` = progression_formatter,
          `Module-Fail` = color_tile("transparent", customOrange),
          `CAT-Fail` = color_tile("transparent", customOrange),
          `Invite To SSM` = invite_meeting_formatter,
          
          #highlighting any failed mods in red
          `Mark CSC1022` =  failed_mods_formatter,
          `Mark CSC1023` =  failed_mods_formatter,
          `Mark CSC1025` =  failed_mods_formatter,
          `Mark CSC1026` =  failed_mods_formatter,
          `Mark CSC1027` =  failed_mods_formatter,
          `Mark CSC1028` =  failed_mods_formatter,
          `Mark CSC1029` =  failed_mods_formatter,
          `Mark CSC1030` =  failed_mods_formatter,
          `Mark CSC1031` =  failed_mods_formatter,
          
          #`Mark CSC1022` = color_bar(customGreen, customGreen0),
          
          #highlighting any fails and exceptions to user
          `Grade CSC1022` =  grade_formatter,
          `Grade CSC1023` = grade_formatter,
          `Grade CSC1025` =  grade_formatter,
          `Grade CSC1026` =  grade_formatter,
          `Grade CSC1027` =  grade_formatter,
          `Grade CSC1028` =  grade_formatter,
          `Grade CSC1029` =  grade_formatter,
          `Grade CSC1030` =  grade_formatter,
          `Grade CSC1031` =  grade_formatter
          
          # `Grade.S1.CSC1026` = grade_formatter,
          #`Module.Fail` = formatter("span", style = ~ style(font.weight = "bold"))
          
        )
      ) %>%
        as.datatable(table, 
                     
                     # buttons styling
                     callback=JS('$("button.buttons-copy").css("background","#8FBC8F"); 
                          $("button.buttons-print").css("background","#FFFACD"); 
                          $("button.buttons-excel").css("background","#8FBC8F"); 
                          $("button.buttons-csv").css("background","#FFFACD"); 
                          return table;'),
                     
                     
                     escape = FALSE,
                     extensions= c('FixedColumns', 'Buttons', 'FixedHeader'),
                     container = cs_s3_heading,
                   
                     filter = 'top',
                  
                   
                     options = list(scrollX = TRUE,
                                    
                                    ##datatables start at index 0
                                    #columnDefs = list(list(className = 'dt-center', targets = c(3:20, 25:28, 30))), 
                                    
                                    #columnDefs = list(list(width = '600px', targets = "_all")),
                                                           # width = '2000px', targets = "_all")),
                                    
                                    
                                    columnDefs = list(list(className = 'dt-center', targets = c(3:20, 24:27, 29))),
                                    
  
                                    fixedColumns = list(leftColumns = 2),
                                    #fixedHeader = TRUE,
                                    
                                    
                                    #download table options
                                    dom = "Bfrtip",
                                    
                                    buttons = list(
                                      list(extend = "csv", filename = paste(currentDate, 'Stage 1 Queens CS-SE-CIT  Semester 3 Summer Results', sep = " ")),
                                      list(extend = "excel", filename = paste(currentDate, 'Stage 1 Queens CS-SE-CIT Semester 3 Summer Results', sep = " "))
                                      
                                    )
                                    
                     ), # container = sketch, 
                     rownames = FALSE
        )
      
      return(table)
    }
  )
  
###change over bit - change over the graphs from test_a3_bit - 
  ##mention about - changing the design of reading in reactive file uploads 
  

  #=======================================
  
  # Output on screen - assignments to function above 
  
  
  #≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠≠ 
  
  
  ####testing picked data for plot
  picked_data <- reactive({
    
    
    #testing input here
    input$generate
    
    
    isolate({
      
      #############
      # Semester 1 
      ###############
      
      ####### BIT SEM 1
      if (input$pick_path =='BIT' && input$semester_num == 'Semester 1'){
        
        validate(
          
          need(input$student_upload, "Please load Qsis file"),
          need(input$acc1022_file, "Please upload ACC1022"),
          need(input$mgt1009_file, "Please upload MGT1009"),
          
        )
        
        reactive_data <- data_bit_s1()
      }
      
      
      #####CS.SE.CIT SEM 1
      if(input$pick_path =='CS/SE/CIT' && input$semester_num == 'Semester 1'){
        
        validate(
          
          need(input$student_upload, "Please load Qsis file"),
          need(input$csc1022_file, "Please upload CSC1022"),
          need(input$csc1025_file, "Please upload CSC1025"),
          need(input$csc1026_file, "Please upload CSC1026"),
          need(input$csc1027_file, "Please upload CSC1027")
          
        )
        reactive_data <- data_s1_cs()
        
      }
      
      
      #############
      # Semester 2
      ###############
      
      ##### BIT SEM 2
      if(input$pick_path =='BIT' && input$semester_num == 'Semester 2'){
        
        
        validate(
          
          need(input$student_upload, "Please load Qsis file"),
          need(input$csc1023_file_bit, "Please upload CSC1023"),
          need(input$mgt1012_file, "Please upload MGT1012"),
          need(input$csc1024_file, "Please upload CSC1024"),
          need(input$mgt1013_file, "Please upload MGT1013"),
          need(input$eco1007_file, "Please upload ECO1007")
        )
        
        
        reactive_data <- data_bit_s2()
        
      }    
      
      
      ######CS.SE.CIT SEM 2  
      if(input$pick_path =='CS/SE/CIT' && input$semester_num == 'Semester 2'){
        
        
        validate(
          
          need(input$student_upload, "Please load Qsis file"),
          need(input$csc1023_file, "Please upload CSC1023"),
          need(input$csc1028_file, "Please upload CSC1028"),
          need(input$csc1029_file, "Please upload CSC1029"),
          need(input$csc1030_file, "Please upload CSC1030"),
          need(input$csc1031_file, "Please upload CSC1031"),
          
        )
        
        reactive_data <- data_s2_cs()
        
       
      }
      
      
      #############
      # Semester 1  + 2 
      ###############
      
      
      
      ####### CS.SE.CIT SEM 1 + 2 
      if(input$pick_path =='CS/SE/CIT' && input$semester_num == 'Semester 1 + 2'){
        
        
        validate(
          
          need(input$student_upload, "Please load Qsis file"),
          need(input$csc1023_file, "Please upload CSC1023"),
          need(input$csc1028_file, "Please upload CSC1028"),
          need(input$csc1029_file, "Please upload CSC1029"),
          need(input$csc1030_file, "Please upload CSC1030"),
          need(input$csc1031_file, "Please upload CSC1031"),
          
        )
        
        reactive_data <- data_s1_s2_cs() 
      }
      
      ######### BIT SEM 1 + 2
      if(input$pick_path =='BIT' && input$semester_num == 'Semester 1 + 2'){
        
        
        
        validate(
          
          need(input$student_upload, "Please load Qsis file"),
          need(input$csc1023_file_bit, "Please upload CSC1023"),
          need(input$mgt1012_file, "Please upload MGT1012"),
          need(input$csc1024_file, "Please upload CSC1024"),
          need(input$mgt1013_file, "Please upload MGT1013"),
          need(input$eco1007_file, "Please upload ECO1007")
        )
        
      reactive_data <- data_bit_s1_s2()
      }  
      
      
      #############
      # Semester 3 
      ###############
      
      
      if(input$pick_path =='BIT' && input$semester_num == 'Semester 3'){
        
        validate(
          need(input$student_upload, "Please load Qsis file")
        )
        
        reactive_data <- data_bit_s3()
        
      }  
      
      
      if(input$pick_path =='CS/SE/CIT' && input$semester_num == 'Semester 3'){
        
        validate(
          need(input$student_upload, "Please load Qsis file")
        )
        
        
        reactive_data <- data_s3_cs()
      }  
      
      #Sem1_output[is.na(Sem1_output)] <- '-'
    })
    
    
    #testing input here
    #input$generate
    
    
    return(reactive_data)
    
  })
  
  
#might need to change to picked_pathway _ table 
  picked_pathway <- reactive({
    

    #testing input here
    input$generate

    
    isolate({
    
    #############
    # Semester 1 
    ###############
    
    ####### BIT SEM 1
    if (input$pick_path =='BIT' && input$semester_num == 'Semester 1'){
      
      validate(
        
        need(input$student_upload, "Please load Qsis file"),
        need(input$acc1022_file, "Please upload ACC1022"),
        need(input$mgt1009_file, "Please upload MGT1009")
        
      )
      
        user_choice <- generate_s1_bit()
    }
      
      
    #####CS.SE.CIT SEM 1
    if(input$pick_path =='CS/SE/CIT' && input$semester_num == 'Semester 1'){
      
      validate(

        need(input$student_upload, "Please load Qsis file"),
        need(input$csc1022_file, "Please upload CSC1022"),
        need(input$csc1025_file, "Please upload CSC1025"),
        need(input$csc1026_file, "Please upload CSC1026"),
        need(input$csc1027_file, "Please upload CSC1027")

      )
        user_choice <- generate_s1_cs()

    }
      
    
    #############
    # Semester 2
    ###############
      
    ##### BIT SEM 2
    if(input$pick_path =='BIT' && input$semester_num == 'Semester 2'){
        
        
        validate(
          
          need(input$student_upload, "Please load Qsis file"),
          need(input$csc1023_file_bit, "Please upload CSC1023"),
          need(input$mgt1012_file, "Please upload MGT1012"),
          need(input$csc1024_file, "Please upload CSC1024"),
          need(input$mgt1013_file, "Please upload MGT1013"),
          need(input$eco1007_file, "Please upload ECO1007")
        )
        
        
        user_choice <- generate_s2_bit()
      }    
 
      
    ######CS.SE.CIT SEM 2  
    if(input$pick_path =='CS/SE/CIT' && input$semester_num == 'Semester 2'){
      
      
      validate(
        
        need(input$student_upload, "Please load Qsis file"),
        need(input$csc1023_file, "Please upload CSC1023"),
        need(input$csc1028_file, "Please upload CSC1028"),
        need(input$csc1029_file, "Please upload CSC1029"),
        need(input$csc1030_file, "Please upload CSC1030"),
        need(input$csc1031_file, "Please upload CSC1031")
        
      )
      
        user_choice <- generate_s2_cs()
    }
      
      
    #############
    # Semester 1  + 2 
    ###############
      
      
      
    ####### CS.SE.CIT SEM 1 + 2 
    if(input$pick_path =='CS/SE/CIT' && input$semester_num == 'Semester 1 + 2'){
      
      
      validate(
        
        need(input$student_upload, "Please load Qsis file"),
        need(input$csc1023_file, "Please upload CSC1023"),
        need(input$csc1028_file, "Please upload CSC1028"),
        need(input$csc1029_file, "Please upload CSC1029"),
        need(input$csc1030_file, "Please upload CSC1030"),
        need(input$csc1031_file, "Please upload CSC1031")
        
      )
      
        user_choice <- generate_s1_s2_cs()  
    }
     
    ######### BIT SEM 1 + 2
    if(input$pick_path =='BIT' && input$semester_num == 'Semester 1 + 2'){
      
      
      
      validate(
        
        need(input$student_upload, "Please load Qsis file"),
        need(input$csc1023_file_bit, "Please upload CSC1023"),
        need(input$mgt1012_file, "Please upload MGT1012"),
        need(input$csc1024_file, "Please upload CSC1024"),
        need(input$mgt1013_file, "Please upload MGT1013"),
        need(input$eco1007_file, "Please upload ECO1007")
      )
      
        user_choice <- generate_s1_2_bit()
    }  
      
      
    #############
    # Semester 3 
    ###############
      
   
    if(input$pick_path =='BIT' && input$semester_num == 'Semester 3'){
      
      validate(
        need(input$student_upload, "Please load Qsis file")
      )
        user_choice <- generate_s3_bit()
    }  
      
      
    if(input$pick_path =='CS/SE/CIT' && input$semester_num == 'Semester 3'){
      
      validate(
        need(input$student_upload, "Please load Qsis file")
      )
      
        user_choice <- generate_s3_cs()
      }  
      
      #Sem1_output[is.na(Sem1_output)] <- '-'
    })
    
    
    #testing input here
    #input$generate
    

    return(user_choice)

  })

  #https://stackoverflow.com/questions/63255166/shiny-reactive-data-plotting-using-ggplot
  
  
# box on progress tab page  - shiny box widget 
  observeEvent(
    
  input$mybox$collapsed, {
      visible <- if (input$mybox$visible) "visible" else "hidden"
      collapsed <- if (input$mybox$collapsed) "collapsed" else "uncollapsed"


  })


  #title for progress table
  output$table_title <- renderText({
    
    
    validate(
      need(input$generate, " ")
    )
    

 
    isolate({
      paste("Queens University Stage One ",input$pick_path, input$semester_num)
    })

 
    
  })
  
  output$bar_title <- renderText({
    
    
    validate(
      need(input$generate, " ")
    )
    
    
    
    isolate({
      paste("Queens University Stage One ",input$pick_path, input$semester_num)
    })
    
    
    
  })
  
  
  # output$progress_results <-
  output$progress_results <-
      renderDataTable(
        picked_pathway() 
    )
  
  
  
  
  ######  download CSV Progression report 
  output$downloadReport <- downloadHandler(
    
    filename = function() {
      paste(currentDate, input$pick_path, input$semester_num, ".csv", sep = " ")
    },
    
    content = function(file) {
      write.csv(picked_data(), file, row.names = FALSE)
    }
  )
  
  
  
  
  
  
  ##plot for the bar chart results
  output$plot <- renderPlot({ 
    gg <-  ggplot(picked_data(), aes(y=Plan_Descr, fill = `Progressed?`)) + geom_bar() + #position=position_dodge()
      #theme_bw() + #white background
      labs(y = "Pathway", x = "Count of Students", title = "Progression Count") +
      #subtitle = "Count of students that progressed") +
      theme(plot.title = element_text(hjust = 0.5))  + #centres heading 
      #theme(plot.title = element_text(vjust = 2)) + #changes heading prosition
      
      theme(plot.title = element_text(size=18, margin=margin(0,0,30,0))) + 
      theme(legend.text = element_text(size=14)) +
      theme(legend.title = element_text(size=14)) +
      theme(axis.text.y = element_text(size=14)) +
      theme(axis.text.x = element_text(size=14)) +
      theme(axis.title.y = element_text(size=14)) +
      theme(axis.title.x = element_text(size=14)) +
      stat_count(geom = "text", colour = "black", size = 6,
      aes(label = ..count..),position=position_stack(vjust=0.5)) +
      #changes size of heading 
      scale_fill_manual(values=c("#D61A46", #not progressed color
                                 "#98CA32" #progressed color
      ))# + g+theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))
    
    
    vals$gg <- gg
    
    print(gg)
    
  })
  
  
  ##download plot of progression results
  output$downloadPlot <- downloadHandler(
    
    filename=  paste("Queens University Stage One Progression summary",input$pick_path, input$semester_num),
    content = function(file){
  
      
      png(file)
      print(vals$gg)
      dev.off()
    }
  )   

  
}






