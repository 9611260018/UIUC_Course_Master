# All packages needed should be loaded in this chunk
pkg_list = c('shiny', 'dplyr', 'ggplot2', 'tidyr')

# Determine what packages are NOT installed already.
to_install_pkgs = pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]

# Install the missing packages
if(length(to_install_pkgs)) {
  install.packages(to_install_pkgs, repos = "https://cloud.r-project.org")
}
# Load all packages
pkgs_loaded = sapply(pkg_list, require, character.only = TRUE)

uiuc_gpa_dataset = readRDS("uiuc_gpa_dataset.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("UIUC Course Master"),
  
  tabsetPanel(
    tabPanel('Search by Course',fluid=TRUE,
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 # select course subject
                 selectInput(
                   inputId = "subject", 
                   label = "Course Subject", 
                   choices = uiuc_gpa_dataset$Subject,
                   selected = uiuc_gpa_dataset$Subject[1]
                 ),
                 
                 # update the selections for course number by selected course subject
                 uiOutput("reactive_number"),
                 
                 actionButton(
                   inputId = "action", 
                   label = "Show Result"
                 )
                 
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Overview", verticalLayout(plotOutput("gpa_over_term"), 
                                                       plotOutput("size_over_term"),
                                                       plotOutput("term_gpa"))),
                   tabPanel("Grade Distribution", plotOutput("plot_grade_pctg")),
                   tabPanel("Instructor Information", tableOutput("tbl_inst_yt")),
                   tabPanel("Model", sidebarLayout(
                     sidebarPanel(
                       # select year, term, instructor for prediction
                       titlePanel("Prediction"),
                       selectInput(
                         inputId = "newyear",
                         label = "Year",
                         choices = c(2019:2024)
                       ),
                       uiOutput("reactive_term"),
                       uiOutput("reactive_instructor"),
                       actionButton(
                         inputId = "predict", 
                         label = "Predict GPA"
                       ),
                       textOutput("prediction_text")
                     ),
                     mainPanel(
                       titlePanel("Summary of the Linear Fit"),
                       # show summary of this model
                       verbatimTextOutput("summary")
                     )
                   ))
                 )
               )
             )   
             
    ),  
    tabPanel('Search by Professor',fluid=TRUE,
             sidebarLayout(sidebarPanel(
               # Text input with Professor's Name
               textInput(
                 inputId = 'name1',
                 label = 'First Name',
                 value = ''
               ),
               textInput(
                 inputId = 'name2',
                 label = 'Middle Initial Name',
                 value = ''
               ),
               textInput(
                 inputId = 'name3',
                 label = 'Last Name',
                 value = ''
               ),
               
               actionButton(
                 inputId = "submit",
                 label = "Start Searching"
               )
             ),
             
             # The tabset panel contains a graph tab and a model summary tab
             mainPanel(tabsetPanel(
               tabPanel("Overview", tableOutput("overview")),
               tabPanel("GPA", plotOutput("lineplot")),
               tabPanel("Grade Distribution", plotOutput("barplot"))
             ))
             )
             
    )
    
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # reactive course number list depending on the selected course subject 
  number_list = reactive({
    unique(subset(uiuc_gpa_dataset, uiuc_gpa_dataset$Subject == input$subject)$Number)
  })
  
  # reactive exclusive terms depending on the selected course - for prediction use
  terms = reactive({
    unique(subset(uiuc_gpa_dataset,
                  uiuc_gpa_dataset$Subject == input$subject &
                    uiuc_gpa_dataset$Number == input$number)$Term)
  })
  
  # reactive exclusive instructors depending on the selected course - for prediction use
  instructors = reactive({
    unique(subset(uiuc_gpa_dataset,
                  uiuc_gpa_dataset$Subject == input$subject &
                    uiuc_gpa_dataset$Number == input$number)$Primary.Instructor)
  })
  
  # reactive course title based on selected course 
  course_title = reactive({
    unique(subset(uiuc_gpa_dataset,
                  uiuc_gpa_dataset$Subject == input$subject &
                    uiuc_gpa_dataset$Number == input$number)$Course.Title)
  })
  
  # update the choices for course number based on course subject
  output$reactive_number = renderUI({
    selectInput(
      inputId = "number", 
      label = "Course Number", 
      choices = number_list()
    )
  })
  
  # update the term choices for prediction
  output$reactive_term = renderUI({
    selectInput(
      inputId = "newterm",
      label = "Term",
      choices = terms()
    )
  })
  
  # update the instructor choices for prediction
  output$reactive_instructor = renderUI({
    selectInput(
      inputId = "newinstructor",
      label = "Primary Instructor",
      choices = instructors()
    )
  })
  
  # plot1 for the overview tab
  gpa_over_term = eventReactive(input$action, {
    # find the average gpa for each year-term
    gpa_subset = subset(uiuc_gpa_dataset, 
                        uiuc_gpa_dataset$Subject == input$subject
                        & uiuc_gpa_dataset$Number == input$number) %>% 
      group_by(YearTerm) %>%
      summarise(avg_gpa = mean(gpa))
    
    gpa_subset$YearTerm = factor(gpa_subset$YearTerm)
    
    # line plot for average gpa over year-term
    ggplot(gpa_subset) +
      aes_string(x = 'YearTerm', y = 'avg_gpa', group = 1) +
      geom_line() +
      labs(x = "Year-Term", y = "Average GPA", title = "Average GPA Over Year-Term", subtitle = course_title()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  })
  
  # plot2 for the overview tab
  size_over_term = eventReactive(input$action, {
    # find the average size for each year-term
    size_subset = subset(uiuc_gpa_dataset, 
                         uiuc_gpa_dataset$Subject == input$subject
                         & uiuc_gpa_dataset$Number == input$number) %>% 
      group_by(YearTerm) %>%
      summarise(avg_size = mean(size))
    
    size_subset$YearTerm = factor(size_subset$YearTerm)
    
    # line plot for average size over year-term
    ggplot(size_subset) +
      aes_string(x = 'YearTerm', y = 'avg_size', group = 1) +
      geom_line() +
      labs(x = "Year-Term", y = "Average Size", title = "Average Course Size Over Year-Term", subtitle = course_title()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # plot3 for the overview tab
  term_gpa_boxplot = eventReactive(input$action, {
    gpa_subset = subset(uiuc_gpa_dataset, 
                        uiuc_gpa_dataset$Subject == input$subject
                        & uiuc_gpa_dataset$Number == input$number)
    ggplot(gpa_subset) +
      aes_string(x = 'Term', y = 'gpa', fill = 'Term') +
      geom_boxplot() +
      geom_jitter() +
      labs(x = "Term", y = "GPA", title = "GPA Distribution for Terms", subtitle = course_title())
  })
  
  # count the percentage of each grade given by each professor
  grade_pctg = eventReactive(input$action, {
    pctg_subset = subset( uiuc_gpa_dataset, 
                          uiuc_gpa_dataset$Subject == input$subject &
                            uiuc_gpa_dataset$Number == input$number ) %>%
      group_by(Primary.Instructor) %>%
      summarise(totalaplus = sum(Aplus),
                totala = sum(A),
                totalaminus = sum(Aminus),
                totalbplus = sum(Bplus),
                totalb = sum(B),
                totalbminus = sum(Bminus),
                totalcplus = sum(Cplus),
                totalc = sum(C),
                totalcminus = sum(Cminus),
                totaldplus = sum(Dplus),
                totald = sum(D),
                totaldminus = sum(Dminus),
                totalf = sum(F),
                totalw = sum(W)) %>%
      mutate(overalltotal = rowSums(.[,2:15])) %>%
      mutate_at(vars(-Primary.Instructor, -overalltotal), function(x) 100*x/.$overalltotal) %>%
      .[,1:15]
    # change the column names to make the plot prettier
    colnames(pctg_subset) = c('Primary.Instructor', 'A+', 'A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D', 'D-', 'F', 'W')
    
    pctg_subset = gather(pctg_subset, key = 'grade', value = 'pctg', `A+`:`W`)
    
    ggplot(pctg_subset) +
      aes(x = grade, y = pctg, fill = Primary.Instructor) +
      geom_bar(stat = "identity", position = 'dodge') +
      labs(x = "Grade", y = "Percentage", title = "Grade Distribution of Each Professor", subtitle = course_title())
    
  })
  
  # instructor names and the terms they teach
  instructor_yearterm_table = eventReactive(input$action, {
    subset(uiuc_gpa_dataset, 
           uiuc_gpa_dataset$Subject == input$subject & 
             uiuc_gpa_dataset$Number == input$number) %>%
      group_by(Primary.Instructor) %>%
      distinct(YearTerm)
  })
  
  # the model part
  # construct a formula and a linear regression model fit using the selected
  formula_dynamic = eventReactive(input$action, {
    as.formula("gpa ~ Year + Term + Primary.Instructor")
  })
  
  fit = eventReactive(input$action, {
    course_subset = subset(uiuc_gpa_dataset, 
                           uiuc_gpa_dataset$Subject == input$subject & 
                             uiuc_gpa_dataset$Number == input$number)
    lm(formula_dynamic(), data = course_subset)
  })
  
  # construct the prediction text outcome for the year, term, and instructor user chose
  prediction_text = eventReactive(input$predict, {
    estimated_gpa = predict(fit(),newdata = data.frame(
      Year = as.integer(input$newyear), Term = input$newterm, Primary.Instructor = input$newinstructor))
    paste("The estimated GPA for", course_title(), "in", input$newterm, input$newyear, "with",
          input$newinstructor, "is", round(estimated_gpa, 2))
    
  })
  
  #overview_reactive
  overview_reactive <- eventReactive(input$submit, {
    if ( input$name2=='') {
      pro_name = paste0(input$name3, ',',' ', input$name1)
    }
    if (input$name2!='') {
      pro_name = paste0(input$name3, ',',' ',input$name1,' ',input$name2 )
    }
    
    tosearch = tolower(pro_name)
    uiuc_gpa_dataset$`besearched`=tolower(uiuc_gpa_dataset$Primary.Instructor)
    instructor_subset = uiuc_gpa_dataset[uiuc_gpa_dataset$besearched ==
                                           tosearch,]
    if (nrow(instructor_subset)==0) {
      return('This Professor is not in our data base, please check your input')
    } else {
      course_id = paste(instructor_subset$Subject, instructor_subset$Number)
      course_title = instructor_subset$Course.Title
      course_term=instructor_subset$YearTerm
      overview = data.frame('Course ID' = course_id, 'Course Title' = course_title,'Course Term'=course_term)
      unique(overview)
    }})
  
  #lineplot_reactive
  lineplot_reactive=eventReactive(input$submit,{
    if ( input$name2=='') {
      pro_name = paste0(input$name3, ',',' ', input$name1)
    }
    if (input$name2!='') {
      pro_name = paste0(input$name3, ',',' ',input$name1,' ',input$name2 )
    }
    
    tosearch = tolower(pro_name)
    uiuc_gpa_dataset$`besearched`=tolower(uiuc_gpa_dataset$Primary.Instructor)
    instructor_subset = uiuc_gpa_dataset[uiuc_gpa_dataset$besearched ==
                                           tosearch,]
    
    course_id = paste(instructor_subset$Subject, instructor_subset$Number)
    course_term=instructor_subset$YearTerm
    course_grade=instructor_subset[,7:20]
    colnames(course_grade)=c('A+','A','A-','B+','B','B-','C+','C','C-','D+','D','D-','F','W')
    gpa=(4*course_grade$`A+`+4*course_grade$A+3.7*course_grade$`A-`+3.3*course_grade$`B+`+3*course_grade$B+2.7*course_grade$`B-`+2.3*course_grade$`C+`+2*course_grade$C+1.7*course_grade$`C-`+1.3*course_grade$`D+`+1.0*course_grade$D)/(course_grade$`A+`+course_grade$A+course_grade$`A-`+course_grade$`B+`+course_grade$B+course_grade$`B-`+course_grade$`C+`+course_grade$C+course_grade$`C-`+course_grade$`D+`+course_grade$D+course_grade$`D-`+course_grade$F)
    line_table = cbind('Course_ID' = course_id,'Course_term'= course_term, course_grade,gpa)
    
    ggplot(line_table)+
      aes(x=Course_term,y=gpa)+
      geom_point()+
      facet_wrap(~Course_ID)+
      theme(axis.text.x =
              element_text(
                angle = 45, hjust = 1
              )
      )
  })
  
  #barplot_reactive
  barplot_reactive=eventReactive(input$submit, {
    if ( input$name2=='') {
      pro_name = paste0(input$name3, ',',' ', input$name1)
    }
    if (input$name2!='') {
      pro_name = paste0(input$name3, ',',' ',input$name1,' ',input$name2 )
    }
    
    tosearch = tolower(pro_name)
    uiuc_gpa_dataset$`besearched`=tolower(uiuc_gpa_dataset$Primary.Instructor)
    instructor_subset = uiuc_gpa_dataset[uiuc_gpa_dataset$besearched ==
                                           tosearch,]
    course_id = paste(instructor_subset$Subject, instructor_subset$Number,instructor_subset$YearTerm)
    course_grade=instructor_subset[,7:20]
    
    grades = c('A+','A','A-','B+','B','B-','C+','C','C-','D+','D','D-','F','W')
    colnames(course_grade)= grades
    bar_table = cbind('Course_ID' = course_id, course_grade)
    bar_table=gather(bar_table,letter_grade,count,'A+':'W')
    bar_table$letter_grade = factor(bar_table$letter_grade, levels = grades)
    ggplot(bar_table,
           aes(
             x = letter_grade,
             y = count,
             fill = Course_ID
           ))  +
      geom_bar(stat = "identity", position = 'dodge')
    
  })
  
  # outputs
  output$overview <- renderTable({
    overview_reactive()
  })
  output$lineplot <- renderPlot({
    lineplot_reactive()
  })
  output$barplot <- renderPlot({
    barplot_reactive()
  })
  
  
  
  output$gpa_over_term = renderPlot({
    gpa_over_term()
  })
  output$size_over_term = renderPlot({
    size_over_term()
  })
  output$plot_grade_pctg = renderPlot({
    grade_pctg()
  })
  output$tbl_inst_yt = renderTable({
    instructor_yearterm_table()
  })
  output$term_gpa = renderPlot({
    term_gpa_boxplot()
  })
  output$summary = renderPrint({
    summary(fit())
  })
  output$prediction_text = renderText({
    prediction_text()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)