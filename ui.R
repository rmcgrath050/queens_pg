
library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(shinyjs)
library(shinyWidgets)
#library(shiny.router)
library(formattable)
library(shinycssloaders)



#customer color for formatable 
customRed = "#ff7f7f"

#increase size of file uploads to 60mb
options(shiny.maxRequestSize = 60*1024^2)

#-----------------------------------------------
# Dashboard UI
#-------------------------------------------------


#Start of UI app 
dashboardPage( 
  
  #slogan of app
  
  dashboardHeader(title = "Queens Progress Generator"), ##title = logo, titleWidth =  400),

  #sidebar of shinyapp
  dashboardSidebar(
  
    #sidebar options
    sidebarMenu (
      #class = "navbar-light bg-light", 
      menuItem("Welcome", tabName = "welcome", icon = icon("th")),
      menuItem("Generator", tabName = "generator", icon = icon("cogs")),
      menuItem("Progress results", tabName = "progress", icon=icon("table")),
      menuItem("Summary", tabName = "summary", icon = icon("chart-bar")),
      #get graphs working 
      menuItem("About", tabName = "about", icon = icon("question"))
    )
  ),
  
  
  #<FontAwesomeIcon icon="fa-solid fa-file-chart-column" />

  #main body of app 
  dashboardBody(
    #theme = shinytheme("flatly"),
    class= "progress_app",
    
    #this function needs called inside UI for shiny JS to work
    useShinyjs(),
    
    #custom css 
    tags$head(
      
    #css custom file
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    
    #JS custom file 
    tags$script(src = "js_handler.js"),
    
    #progress bars for uploading files -  color determined here -  background-color: #ffffcc;  #EEE8AA ##228B22 #8FBC8F
    tags$style(".btn-file{background-color: #004d00;    
    color: white;
            }
    .progress-bar{background-color:#3c763d;},"),
    
    
    #generate button color 
    tags$style(HTML('#generate{
                    background-color: #ffd480;
                    border: 2px solid grey;
                    font-size: 16px;
                    font-weight : bold ;
                    
                    }')),
    
    
    tags$style(HTML('#clear{
                    background-color:#FA8072;
                    border: 2px solid grey;
                    font-size: 16px;
                    font-weight : bold ;
                    }')),
    
    
    tags$style(HTML('#about{
                    background-color:#D2B48C;
                    font-size: 20px;
                    font-weight : bold ;
                    }')),
    
    tags$style(HTML('#rules{
                    background-color:#D2B48C;
                    font-size: 20px;
                    font-weight : bold ;
                    width: 300px;
                    }')),
    
    
    
    
    
    #styling of table header
    tags$style(".startButton{
                 background-color:#BDB76B;
                  font-size: 20px;}"
    ),
    
    
    # #styling of table header
    tags$style(".startButton:hover{
                 background-color:#ffeecc; 
                  font-size: 20px
                 }"
    ),

    
    #styling of table header - test this 
    tags$style("#table_title{
                font-size: 20px;
                color: #8b0000;
                font-style: Verdana}"
               
    ),
    
    
    tags$style("#bar_title{
                font-size: 20px;
                color: #8b0000;
                font-style: Verdana}"
               
    ),
    
    tags$style(".about_page{
                background-color: white;
               }"),
    
    
    
    
    ##javascript for page navigation - links
    tags$script(
      HTML(
        '
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      '
      )
    ),
    
    #error messages
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #6B8E23;   
        font-size: 16px;
        
      }
    "))
    
    
    ),
    
    
    
    #main tabs of shiny app
    fluidPage(
      tabItems(

    #=====================================
        # WELCOME PAGE
    #======================================
    
    
    #welcome page - design this next 28/02
    tabItem(
      
      tabName = "welcome",
                  fluidPage(
                  class = "welcome_page",
                  
                  fluidRow(
                  column(4,),
                  column(5,
                  tags$image(src = "pg.png", align="center"),)),
                                  #tags$image(src = "queens_logo.jpeg", align="center"),)),
                  
                  
                  fluidRow(
                    column(3,),
                    column(6,           
                           h2("Welcome to the Queens Progress Generator"),
                           br()
                           )
                  ),
                  
                  
                  fluidRow(column(1, ),
                           column(
                             10,
                             h4(
                               "An application to analyse students progression in
                             Stage One for Queens University"
                             ),
                             br(),
                             h4("For Pathways -Business Information Technology (BIT)
                                Computing and Information Technology (CIT),
                                Computer Science and Engineering (CS) &
                                Master’s in Engineering (MEng)
                                "),
                             br(),
                             br(),
                          
                           )
                    #tags$code(""), displays like computer code
                  ),
                  
                  
                  fluidRow(class = "page_instructions",
                           br(),
                           br(),
                           column(2,),
                           column(2,
                                  tags$image(src = "num1.png"), ),
                           column(
                             5,
                             tags$blockquote(
                               "Choose Pathway - Please note that all other pathways apart from
                                      Business Information Technology has been grouped together as these
                                      have the same modules to progress"
                             )
                             
                           )),
                  
                  br(),
                  
                  
                  fluidRow(class = "page_instructions",
                           column(2, ),
                           column(2,
                                  tags$image(src = "num2.png"),),
                           column(
                             5,
                             tags$blockquote(
                               "Choose Semester - Semester three will generate a full Stage One progress report for
                                           the year including repeated modules in the summer"
                             )
                             
                           )), 
                  
                  br(),
                  
                  fluidRow(class = "page_instructions",
                           column(2,),
                           column(2,
                                  tags$image(src = "num3.png"), ),
                           column(
                             5,
                             tags$blockquote(
                               "Choose files - Please upload all files in a CSV format relevant to the Pathway displayed. Every upload will require a QSIS
                           file. This is uploaded with specific module files requested on the generator form"
                             )
                             
                           )),
                  
                  br(),
                  
                  
                  fluidRow(class = "page_instructions",
                           column(2, ),
                           column(2,
                                  tags$image(src = "num4.png"),),
                           column(
                             5,
                             tags$blockquote(
                               'Generate - All thats left to do his hit the "Generate" button to display results!!'
                             )
                             
                           )), 

                  #cant get action button to move onto another tab  - due to shiny reacitvity on server 
                  fluidRow(
                    br(),
                    br(),
                    br(),
                    
                    #come back to this generate to another tab page 10/03
                    actionButton(inputId = "nav_generator", label = "Lets get Started", width = '250px', onclick="fakeClick('generator')", class = "startButton"),
                    br(),
                    br()
                  )

              )
          ),
        
        #-----------------------------------------
        # Generator tab
        #------------------------------------------
        
      
        tabItem(
          tabName = "generator",
        
         
          
          fluidPage(
            class = "gen_tab",
            
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            
            fluidRow(
              #color this class a color ?? 04/04
              class = "generator_hero",
              
              column(2,),
              column(8,
                     h3("Lets start creating your Progression Report", class =
                          "gen_sub_title"),
                     br(),
                     br()
                     
                     ),
              column(2,),
            ),
            
        
            br(),
            br(),
            br(),
            br(),
            
            #Choose a pathway option
            fluidRow(
              class = "form_page",
            
              fluidRow(
              br(),
              br(),
              br(),
              br(),
              
              hr(class = "break"),
        
              #   #large button - btn-lg
              #   #selection of course path
              #   #titlePanel(textOutput("pick_course")),
              
              column(1,),
              column(3,
                     
                     #tags$style("pick_path {background-color:blue;}"),
                     #inputs to generate output table g
                     tags$div(
                       class= "path_picker",
                       selectInput("pick_path", label = h3("Choose Pathway"), 
                                   choices = list("BIT" = "BIT", "CS/SE/CIT" = "CS/SE/CIT"), 
                                   selected = "CS/SE/CIT"
                       )
                     ),
              ),
              
              
              # choosing a semester
              column(4,
                     selectInput("semester_num", label = h3("Select Semester"), 
                                 choices = list("Semester 1" = "Semester 1", "Semester 2" = "Semester 2", "Semester 1 + 2" = "Semester 1 + 2", "Semester 3 - Summer Results"= "Semester 3"), 
                                ),
                     
                     ),
              
              
              #Upload of student details
              tags$div(
                class = "qsis_upload_id",
                column(3,
                       h3("Upload QSIS details"),
                       fileInput(
                         "student_upload",
                         NULL,
                         buttonLabel = strong("Upload..."),
                         multiple = FALSE,
                         accept = ".csv",
                       ),
                )
                
              ),
      
              column(1,),
            ),
            
       

            #Upload of student marks
            fluidRow(
                br(),
                br(),
              
                #p(strong("text to explnation steps")), 
                
                #Conditional on basis of user - Semester one options for CS/CE/CIT files
                conditionalPanel(
                  condition = "input.pick_path =='CS/SE/CIT' && input.semester_num == 'Semester 1'",
                  fluidRow(column(12, align = "center",
                             h3("Upload Semester one modules"),
                           ),
                           ),
                  
                  br(),
                  br(),
                  
                  fluidRow(
                    column(2,),
                    column(
                      4,
                      p(strong("Architecture and Networks")),
                      fileInput(
                        "csc1022_file",
                        NULL,
                        buttonLabel = strong("CSC1022"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                      
                    ),
                    column(
                      4,
                      p(strong("Procedural Programming [MATHS+CS]")),
                      fileInput(
                        "csc1025_file",
                        NULL,
                        buttonLabel = strong("CSC1025"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                      
                    ),
                    column(2,),
                  ),
                  
                  br(),
                  br(),
             
                  
                  
                  fluidRow(
                    column(2, ),
                    
                    
                    column(
                      4,
                      p(strong("Fundamentals of Maths for Computing")),
                      fileInput(
                        "csc1026_file",
                        NULL,
                        buttonLabel = strong("CSC1026"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                      
                    ),
                    
                    
                    column(
                      4,
                      p(strong("Programming")),
                      fileInput(
                        "csc1027_file",
                        NULL,
                        buttonLabel = strong("CSC1027"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                      
                    ),
                    
                    column(2, ),
                    
                  )
          
                ), 
                
                #Conditional on basis of user - Semester one options for CS/CE/CIT files
                conditionalPanel(
                  condition = "input.pick_path =='BIT' && input.semester_num == 'Semester 1'",
                  # && input.student_upload != NULL
                  #fileInput("mark_uploads", NULL, buttonLabel = "CSC1022", multiple = TRUE, accept = ".csv"),
                  
                  fluidRow(
            
                           column(
                             12, align = "center",
                             h3("Upload Semester one modules "),
                           ),
                           
                           br(),
                           br()
                           
                     
                           ),
                  
                  
                  fluidRow(
                    
                    column(2,),
                    column(4,
                           p(strong("Accounting")),
                           fileInput(
                             "acc1022_file",
                             NULL,
                             buttonLabel = strong("ACC1022"),
                             multiple = FALSE,
                             accept = ".csv"
                           )
                           
                    ),
                    
                    
                    column(4,
                           p(strong("Organisational Behaviour")),
                           fileInput(
                             "mgt1009_file",
                             NULL,
                             buttonLabel = strong("MGT1009"),
                             multiple = FALSE,
                             accept = ".csv"
                           )
                    ),
                    
                  ),
                 
                ),
                
                
                
                #semester two conditions -CS/SE/CIT +BIT
                
                conditionalPanel(
                  condition = "input.pick_path =='CS/SE/CIT' && input.semester_num == 'Semester 2' || input.pick_path =='CS/SE/CIT' && input.semester_num == 'Semester 1 + 2'",
                  fluidRow(column(4, ),
                           column(
                             4,
                             h3("Upload Semester two modules"),
                           ),
                           column(4, )),
                  
                  br(),
                  br(),
                  
                  fluidRow(
                    column(2),
                    column(
                      4,
                      p(strong("Databases")),
                      fileInput(
                        "csc1023_file",
                        NULL,
                        buttonLabel = strong("CSC1023"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                      
                    ),
                    
                    
                    column(
                      4,
                      p(strong("Computer Science Challenges")),
                      fileInput(
                        "csc1028_file",
                        NULL,
                        buttonLabel = strong("CSC1028"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                      
                    )
                    
                  ),
                  
                  br(),
                  br(),
       
                  fluidRow(
                    column(2, ),
                    
                    
                    column(
                      4,
                      p(strong("Object Oriented Programming")),
                      fileInput(
                        "csc1029_file",
                        NULL,
                        buttonLabel = strong("CSC1029"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                    ),
                    
                    
                    column(
                      4,
                      p(strong("Web Technologies")),
                      fileInput(
                        "csc1030_file",
                        NULL,
                        buttonLabel = strong("CSC1030"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                    ),
                    
                  ),
                  
                  
                  br(),
                  br(),
                  
                  fluidRow(
                    column(2,),
                    
                    column(
                      4,
                      p(strong("Software Design Principles")),
                      fileInput(
                        "csc1031_file",
                        NULL,
                        buttonLabel = strong("CSC1031"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                      
                    ),
                    
                  )
                  
                ), 
                
                
   
           #semsester 2 BIT uploads
           conditionalPanel(
             condition = "input.pick_path =='BIT' && input.semester_num == 'Semester 2' || input.pick_path =='BIT' && input.semester_num == 'Semester 1 + 2'",
             # && input.student_upload != NULL
             #fileInput("mark_uploads", NULL, buttonLabel = "CSC1022", multiple = TRUE, accept = ".csv"),
             fluidRow(column(4, ),
                      column(
                        4,
                        h3("Upload Semester two modules"),
                      ),
                      column(4, )),
             
             br(),
             br(),
             
             
             fluidRow(
               #row one
               column(2,),
               
               
               ##bug fixed not uploading file
               #changed file name - same input name used in another function 
               column(4,
                      p(strong("Databases")),
                      fileInput(
                        "csc1023_file_bit",
                        NULL,
                        buttonLabel = strong("CSC1023"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                      
               ),
               

               column(4,
                      p(strong("Marketing")),
                      fileInput(
                        "mgt1013_file",
                        NULL,
                        buttonLabel = strong("MGT1013"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
               ),
               
               
               
             ),
             
             br(),
             br(),
             
             
             
             fluidRow(
               #row one
               column(2,),
               column(4,
                      p(strong("Business, Government and Society")),
                      fileInput(
                        "mgt1012_file",
                        NULL,
                        buttonLabel = strong("MGT1012"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                      
               ),

               
               column(4,
                      p(strong("Programming and Systems Development")),
                      fileInput(
                        "csc1024_file",
                        NULL,
                        buttonLabel = strong("CSC1024"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                      
               ),
               
               
             ),
             
             br(),
             br(),
             
             fluidRow(
               column(2,),
               
               column(4,
                      p(strong("An Introduction of Economics")),
                      fileInput(
                        "eco1007_file",
                        NULL,
                        buttonLabel = strong("ECO1007"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                      
               ),
               
             )

             
             
           ),
           
           
           #####semester 3 uploads
            conditionalPanel(
              
             condition = "input.pick_path =='BIT' && input.semester_num == 'Semester 3'",
             fluidRow(column(4, ),
                      column(
                        4,
                        h3("Upload Semester Three Modules"),
                      ),
                      column(4, )),
             
             br(),
             br(),
             
             
             ####semester one uploads for BIT S3
             fluidRow(

               column(2,),
               column(4,
                      p(strong("Accounting")),
                      fileInput(
                        "acc1022_repeat",
                        NULL,
                        buttonLabel = strong("ACC1022"),
                        multiple = FALSE,
                        accept = ".csv"
                      )

               ),


               column(4,
                      p(strong("Organisational Behaviour")),
                      fileInput(
                        "mgt1009_repeat",
                        NULL,
                        buttonLabel = strong("MGT1009"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
               )

             ),

             
             br(),
             br(),
             
             
             
             fluidRow(
               #row one
               column(2,),
               column(4,
                      p(strong("Business, Government and Society")),
                      fileInput(
                        "mgt1012_repeat",
                        NULL,
                        buttonLabel = strong("MGT1012"),
                        multiple = FALSE,
                        accept = ".csv"
                      )

               ),

               
               column(4,
                      p(strong("Programming and Systems Development")),
                      fileInput(
                        "csc1024_repeat",
                        NULL,
                        buttonLabel = strong("CSC1024"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
                      
               ),
               
               
             ),
             
             
             br(),
             br(),

                    
             fluidRow(
               #row one
               column(2,),


               ##bug fixed not uploading file
               #changed file name - same input name used in another function
               column(4,
                      p(strong("Databases")),
                      fileInput(
                        "csc1023_repeat",
                        NULL,
                        buttonLabel = strong("CSC1023"),
                        multiple = FALSE,
                        accept = ".csv"
                      )

               ),


               column(4,
                      p(strong("An Introduction of Economics")),
                      fileInput(
                        "eco1007_repeat",
                        NULL,
                        buttonLabel = strong("ECO1007"),
                        multiple = FALSE,
                        accept = ".csv"
                      )

               ),

             ),
             
             br(),
             br(),
             
             fluidRow(
               column(2,),
               
               column(4,
                      p(strong("Marketing")),
                      fileInput(
                        "mgt1013_repeat",
                        NULL,
                        buttonLabel = strong("MGT1013"),
                        multiple = FALSE,
                        accept = ".csv"
                      )
               )
               
             )
             
             
           ),
           
           
           
           #semester 3 CS students 
           conditionalPanel(
             condition = "input.pick_path =='CS/SE/CIT' && input.semester_num == 'Semester 3'",
             
             fluidRow(
                      column(
                        12, align = "center",
                        h3("Upload Semester Three modules"),
                      )
                     ),
             
             br(),
             br(),
             
             #semester 1 mods
             fluidRow(
               column(2,),
               column(
                 4,
                 p(strong("Architecture and Networks")),
                 fileInput(
                   "csc1022_repeat_cs",
                   NULL,
                   buttonLabel = strong("CSC1022"),
                   multiple = FALSE,
                   accept = ".csv"
                 )
                 
               ),
               column(
                 4,
                 p(strong("Procedural Programming [MATHS+CS]")),
                 fileInput(
                   "csc1025_repeat",
                   NULL,
                   buttonLabel = strong("CSC1025"),
                   multiple = FALSE,
                   accept = ".csv"
                 )
                 
               ),
               column(2,),
             ),
             
             br(),
             br(),
             
             #semester one mods
             fluidRow(
               column(2, ),
               
               
               column(
                 4,
                 p(strong("Fundamentals of Maths for Computing")),
                 fileInput(
                   "csc1026_repeat",
                   NULL,
                   buttonLabel = strong("CSC1026"),
                   multiple = FALSE,
                   accept = ".csv"
                 )
                 
               ),
               
               
               column(
                 4,
                 p(strong("Programming")),
                 fileInput(
                   "csc1027_repeat",
                   NULL,
                   buttonLabel = strong("CSC1027"),
                   multiple = FALSE,
                   accept = ".csv"
                 )
                 
               ),
               
               column(2, ),
               
             ), 
             
             
             br(),
             br(),
             
             fluidRow(
               column(2),
               column(
                 4,
                 p(strong("Databases")),
                 fileInput(
                   "csc1023_repeat",
                   NULL,
                   buttonLabel = strong("CSC1023"),
                   multiple = FALSE,
                   accept = ".csv"
                 )
                 
               ),
               
               
               column(
                 4,
                 p(strong("Computer Science Challenges")),
                 fileInput(
                   "csc1028_repeat",
                   NULL,
                   buttonLabel = strong("CSC1028"),
                   multiple = FALSE,
                   accept = ".csv"
                 )
                 
               ),
             ),
             
             
             br(),
             br(),
             
             
             fluidRow(
               column(2, ),
               
               column(
                 4,
                 p(strong("Object Oriented Programming")),
                 fileInput(
                   "csc1029_repeat",
                   NULL,
                   buttonLabel = strong("CSC1029"),
                   multiple = FALSE,
                   accept = ".csv"
                 )
               ),
               
               
               column(
                 4,
                 p(strong("Web Technologies")),
                 fileInput(
                   "csc1030_repeat",
                   NULL,
                   buttonLabel = strong("CSC1030"),
                   multiple = FALSE,
                   accept = ".csv"
                 )
                 
               ),
               
               column(2, ),
               
             ), 
             
             
             
             br(),
             br(),  
             
             
             
             
             
             fluidRow(
               
               column(2, ),
               

               
               column(
                 4,
                 p(strong("Software Design Principles")),
                 fileInput(
                   "csc1031_repeat",
                   NULL,
                   buttonLabel = strong("CSC1031"),
                   multiple = FALSE,
                   accept = ".csv"
                 )
                 
               )
               
               
               
             )
             
             
           ) 
           
           
           
           
           
           
           
           
           
           
           
           
             
            ),
            ), #test of row for form
            
            
            br(),
            br(),
            br(),
           
           
            
            #Upload of student details
            fluidRow(
              column(3,),
              column(
              2,
              actionButton("generate", "Generate", class =
                             "btn-block", onclick="fakeClick('progress')"), #style="background-color: #90EE90;")
              #action to upload files to temp server here?
              ),
              column(1,),
              column(2,
                   actionButton("clear", "Reset", class =
                                  "btn-block"), 
                   ),
              
              br(),
              br(),
              br(),
              br()
            
              ),
           
           
           
           
           
           
              ),
          
        ),
        
       
   #======================================
   #Output page of results - main table
   #======================================
            
   
          #prgress test item
          tabItem(tabName = "progress",
                  class = "progression",
                  
                  
              
                  
                  fluidPage(
                    #class = "progression_hero",
                    #textOutput("title_table"),
                    align = "center",
                    
                    br(),
                    br(),
                    
                    div(
                      class = "progression_header",
                      h2("Progression Results"),
                      br(),
                      p("A report created to show module results for each Stage 1 Student"),
                
                    ),
                    
           
                    fluidRow(
                      column(12,
                             
                             br(),
                             br(),
                             
                             br(),
                             br(),
                             
                             p("To progress from one Stage to the next, students must have 
                               passed a minimum of five modules (100 credit points) in the 
                               current stage of study."),
                             
                             br(),
                             
                             p("The formal mechanism for providing marks to students shall be through the Queen’s Student Information System (Qsis)"),
                             
                             br(),
                             
                             p(strong(class = "warning", "***Please consider Students which aren’t enrolled on 120 CATS manually 
                               as progression rules are currently based on the current academic year")
                             )),
                        
                      
                            hr(class = "break"),
                      
                            br(),
                            br(),
                            br(),
                            br(),
                            br(), 
                        
                      
                    ),
                    
                    
                    fluidPage(
                      br(),
                      br(),
                      br(), 
                     # column(3,),
                      column(
                        width = 12,
                             box(id = "mybox", width = NULL, solidHeader = TRUE, 
                                 title = "Table Observations", collapsible = TRUE, status = "warning",
                                 p(code("Marks"), "- Failed marks (under 40) loaded into system will be highlighted red"),
                                 p(code("Grades"), "- Failed grades loaded into system will be highlighted red, Absent grades will
                                   be highlighted grey"),
                                 br(),
                                 br(),
                                 
                                  p(code("Module-Fail"), "- Total amount of modules failed - Orange will be darker the more failed"),
                                 p(code("CAT-Fail"),"- Total amount of CAT fails -  Orange will be darker the more CATS failed"),
                                 p(code("Year_Avg"), "- Year Average - a average under 60 will be highlighted red (useful for MEng students)"),
                                 p(code("Invite to SSM"), "A meeting required will be highlighted in red, if
                                 a letter of concern is required - this will be highlighted yellow"),
                                 
                                 br(),
                                 p(strong("All modules are worth 20 CATs each - except CSC1024 which is worth 40 CATs")),
                                 p(strong("For more progression rules on how data was calculated - see About Page"))
                               
                             )
                      ),
                      
                      
                      br(),
                      br(),
                      br(), 
                      
                    ),
                    
             
                    #shinycssloaders::withSpinner(
                    
                   # table output of progression for each student
                    fluidRow(
                      align = 'center',
                      column(12,
                        
                        br(),
                        br(),
                        br(),
                        br(), 
                        
                        
                        textOutput("table_title"),
                        
                        br(),
                        br(),
                        br(),
                        br(), 
                        
                        downloadButton("downloadReport", "Download Report"),
                  
                        id="progress_table",
                        shinycssloaders::withSpinner(DTOutput("progress_results"), size= getOption("spinner.size", default = 2)
                                                    )#, width = 12
                        ),
                      
                      
                      br(),
                      br(),
                      br(),
                      br()
                    ),


                   
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br()
                  )
          ),
           
           
           ######### SUMMARY PAGE

                tabItem(tabName = "summary",
                        align = "center",
                        
                        fluidPage(
                          class = "summary_page",
                          
                          fluidRow(
                            br(),
                            br(),
                            br(),
                            
                        
                            
                            div(
                              class = "summary_header",
                              h2("Summary of Progression"),
                              br(),
                              p("Some more generated statistics from data")
                            ),
                          
                           
                            
                            br(),
                            br(),
                            br()
                            
                            
                          ),
                          
                          fluidRow(
                            #textOutput("table_title"),
                            textOutput("bar_title"),
                            br(),
                            br(),
                            br(),
                            br()
                            
                          ),
                          
                          
                          
                          div(
                            plotOutput("plot"),
                            br(),
                            br()
                          ),

                          fluidRow(
                            align = 'center',
                                
                            # div(
                            #   plotOutput("plot"),
                            #   br(),
                            #   br()
                            # ),
                                     
                            
                                        #download of bar chart 
                          downloadButton('downloadPlot','Download Plot')
                            
                          ),
                          
                          fluidRow(
                            # column(6,
                            #        box(solidHeader = TRUE,
                            #            title = "Rules built into the System", collapsible = TRUE,)
                            #        ),

                          )
                          
                          
                          
                        
                          
                        )
                       # h2("Summary of Progression"),
                        
                        # br(),
                        # br(),
                        # textOutput("table_title"),
                        # br(),
                        # br(),
                        # 
                        # plotOutput("plot")
                        # 
                          

                        # tabsetPanel(
                        #   tabPanel("Progression Bar Chart",
                        #            br(),
                        #            br(),
                        #            p("The Bar chart below shows how many students progressed for each pathway in Stage One"),
                        #            br(),
                        #            br(),
                        #            textOutput("table_title"),
                        #            br(),
                        #            br(),
                        #            plotOutput("plot")
                        #            ),
                        #   tabPanel("Summary"),
                        #   tabPanel("Table",
                        #            
                        #            
                        # 
                        #   
                        #            
                        #            )
                        # )
                        # 
                        ###testing data 21/04
                        
                     
                ),

              
              
   tabItem(tabName = "about",
           #useShinyjs(),
           
           
           fluidPage(
             
             class = "about_page",
         
             #about header + queens logo
             fluidRow(
               div(class = "about_header",
                   tags$image(src = "final_log.png", align =
                                "center"),
                   
                   
                   h3(strong("Created by Rosie Mcgrath")),
                   h3(strong("Project Idea : Dr Reza Rafiee")),
                   br(),
                   br(),
                   br()
                   ),
               
               
           
               
             ),
             
     
             #buttons for what user can choose to see 
             fluidRow(
               br(),
               br(),


               column(2,
                      actionButton("about", "About", class =
                                     "btn-block"),
               ),


               column(2,
                      actionButton("rules", "Rules of System", class =
                                     "btn-block"),
               ),
               
               
               br(),
               br()

             ),
   
          div(
            id = "aboutInfo",

            fluidRow(
              
              br(),
              br(),
              br(),
              column(2, ),
                   
                     
                     column(
                       8,
                       h3(strong("What is Queens Progress Generator?")),
                       br(),
                       p("The Queens Progress Generator is a data analytics tool that extracts input
                         data provided by the user. ",
                         
                         br(),
                         br(),
                    
                         "The system needs input files from QSR student enrollment reports and internal
                         broadsheets with the module data from Queens univeristy in a CSV format.
                         The software has been mainly designed for the use of a Queens University Stage One co-ordinator." ,
                         br(),
                         br(),
                          
                        "Once the data is uploaded, classification data mining techniques will group module data together for each 
                        student to analyse progression by the end of each semester 
                        "),
                       
                       
                       
                       h3(strong("What is the aim of Queens Progress Generator?")),
                       br(),
                        p( "The aim of this software is to create a visual representation of students’
                              progression in stage one students of QUB.  By developing this application,
                              we are able to identify students with less or no progress in any levels/stages
                              of their education. The level of each progress will be based on QUB regulations and
                              Educational Assessment rules focusing on students enrolled in:"
                       ),
                       
                       
                       tags$ol(
                         tags$li("Business Information Technology"),
                         tags$li("Computing and Information Technology"),
                         tags$li("Computer Science and Engineering"),
                         tags$li("Master’s in Engineering (MEng students)"),
                         br(),
                         br()
                         
                       ),

                       
                       
                      # p("The progress results are acehived through data mining and analyics of ")
                       
                     )
              )
      
        ),
            
            
 
            
            div(
              id = "rulesInfo",
              br(),
              br(),
              
              
              fluidRow(column(2, ),
                       column(
                         8,
                         h3(strong("Guidlines and Rules incorporated into Progression Report")),
                         
                         br(),
                         
                         tags$ul(
                           tags$li("Students must pass at least 100 CATS to progress to Stage 2"),
                           br(),
                           tags$li(
                             "Students are allowed to progress with 20 CATs but will get a",
                             strong("letter of Concern"),
                             "about their progression"
                           ),
                           br(),
                           tags$li(
                             "MEng students are required to acheieve at least 60% to progress. Will receive a ",
                             strong("letter of Concern"),
                             "if year average is below this"
                           ),
                           br(),
                           tags$li(
                             "Students must complete 6 modules in all pathways except BIT (Business Information Technology) which
                                   requires only 5 modules - Students will receive letter of concern if criteria not met"
                           ),
                           br(),
                           tags$li(
                             "Students scored under 100 CATS will be invited to attend a Student Support Meeting "
                           ),
          
                           
                           br(),
                           br(),
                           
                           p(strong("For more guidance on general regulations for Queens University progression click" ,
                           tags$a(href = "https://www.qub.ac.uk/directorates/AcademicStudentAffairs/AcademicAffairs/GeneralRegulations/StudyRegulations/StudyRegulationsforUndergraduateProgrammes/",
                                  "here"
                                  ))),
                           
                           
                           p(strong("For more Specific Program Regulations click" ,
                                    tags$a(href = "https://www.qub.ac.uk/directorates/AcademicStudentAffairs/AcademicAffairs/ProgrammeSpecifications/2021/ug/",
                                           "here"
                                    ))), 
                         )
                         
                         
                       ))
              
            ),
        
              br(),
              br(),
              #end of about page 
              

           )
   
           
           
           
           
           
   )
   
   
   
   
   
      )
   
    )
   
   
  )
  
)