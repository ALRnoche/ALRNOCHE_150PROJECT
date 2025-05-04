##########################################
# Noche, André Luis R.
# SN: 2022-08891
#
# CMSC 150 - B1L
#
# Application UI
# Date started: December 04, 2023
# Date accomplished: December 18, 2023
##########################################
source("../ALRNOCHE_150PROJECT/NocheQSI_project.R") #import QSI, SIMPLEX, and PR
source("../ALRNOCHE_150PROJECT/NochePR_project.R")
# source("../ALRNOCHE_150PROJECT/NocheSMDP_project.R")

library(shiny) #import shiny, shinydashboard, shinyMatrix, shinythemes, and DT features
library(shinydashboard)
library(shinyMatrix)
library(shinythemes)
library(DT)

data_sm = read.csv("../ALRNOCHE_150PROJECT/Nutritional values for the foods.csv",
                header = T, row.names = 1, sep = ",") #import simplex csv file

#define ui for the application
  ui <- dashboardPage(skin = "purple",
    dashboardHeader(title = "CMSC 150 Project"), #change dashboard page skin and name dashboard header
    dashboardSidebar(
      sidebarMenu(
        menuItem("SIMPLEX IMPLEMENTATION", tabName = "SM", icon = icon("table"), #make menu items and subitems
                 menuSubItem("Diet Problem Solver", tabName = "min", icon = icon("minimize"))
        ),
        
        menuItem("GENERIC SOLVERS", tabName = "QSI&PR", icon = icon("chart-simple"),
                 menuSubItem("Quadratic Spline Interpolation", tabName = "QSI", icon = icon("timeline")),
                 menuSubItem("Polynomial Regression", tabName = "PR", icon = icon("timeline"))
                 )
      )
    ),
    dashboardBody(
      tabItems(#accesses menuSubItem of SM
        tabItem("min",
                sidebarPanel(
                  h2(strong("Select Food choices"), style = "color:#FFFFFF;"),
                  style = "background: #2A3950;",
                  
                  fluidRow(#checkbox options
                      checkboxGroupInput("choices_sm", strong("Foods"), choices = rownames(data_sm)),
                      style = "overflow-y: scroll; max-height: 700px; text-align: left;",
                      style = "font-size:20px; background:#2A3950; color:#FFFFFF;"
                          ),
                      h5(textOutput("choices_checker"), style = "color:#FF0000; font-style:italic;"), #checkbox checker
                  
                  fluidRow(#buttons for check all, reset all, and calculate
                      actionButton(inputId = "check_all", label = "Check All", style = "background:#4E5D6C; color:#FFFFFF;"),
                      actionButton(inputId = "reset_all", label = "Reset All", style = "background:#4E5D6C; color:#FFFFFF;"),
                      actionButton(inputId = "calculate_sm", label = "Calculate SM", style = "background:#DF6919; color:#FFFFFF;"),
                      h5(strong(textOutput("wrong_input")), style = "color:#FF0000; font-style:italic;") #check for error input
                          )
                ),
                
                mainPanel(
                  fluidRow(
                    column(10, h1("Diet Problem Solver"), style = "font-size: 20px; background:#2A3950; color:#FFFFFF;")), br(),
                  
                  h5(strong(textOutput("checkbox_indicator")), style = "color:#088408; font-style:italic;"),#outputs user's chosen fields
                  fluidRow(style="overflow-y: scroll; max-height: 200px; text-align: left;",
                           strong(verbatimTextOutput("selected_rows"))), br(), br(),
                  
                  h5(strong(textOutput("infeasible")), style = "color:#FF0000; font-style:italic;"),
                  
                  h5(strong(textOutput("choices_indicator")), style = "color:#DF6919; font-style:italic;"), #outputs user's number of choices
                  fluidRow(
                    tableOutput("serving_mat"), #outputs initial tableau
                    style = "background:#F5F5F5; overflow-y:scroll; max-height:460px; text-align:left;",
                  ),
                  br(), br(), br(),
                  ),
                
                fluidPage(
                  # Create a custom HTML table to display soln_cost and optimal in separate columns
                  tags$table(
                    style = "width: 65%; border-collapse: collapse;",
                    tags$tr(
                      tags$td(
                        h5(strong(textOutput("Soln")), style = "color:#DF6919; font-style:italic;"),
                        div(
                          tableOutput("soln_cost"),
                          style = "text-align:left; background:#F5F5F5; max-width: 340px;"
                        ),
                        style = "border: 0px solid #ddd; vertical-align: top;"
                      ),
                      
                      tags$td(
                        h5(strong(textOutput("Menu")), style = "color:#DF6919; font-style:italic;"),
                        div(
                          h4(strong(textOutput("optimal"))),
                          style = "text-align:left;"
                        ), br(),
                        
                        conditionalPanel(
                          condition = "input.choices_sm.length > 0 && output.soln_cost != null",
                          fluidRow(
                            h5(strong(textOutput("Iteration_num")), style = "color:#DF6919; font-style:italic;"),
                            numericInput(inputId = "iterate_i", label = NULL, value = 0),
                            actionButton(inputId = "check_ite", label = "Check Iteration", style = "background:#4E5D6C; color:#FFFFFF; border: 1px solid black;"),
                            h5(strong(textOutput("wrong_input_ite")), style = "color:#FF0000; font-style:italic;") #check for error input
                          ),
                          style = "border: 0px solid #ddd; vertical-align: top;"
                        ),
                      ),
                    ),
                  ),
                  br(), br(),
                  
                  tags$table(
                    style = "width: 100%; border-collapse: collapse;",
                    
                    tags$tr(
                      tags$td(
                        h5(strong(textOutput("PR_text")), style = "color:#DF6919; font-style:italic;"),
                        div(
                          tableOutput("pivot_row"),
                          style = "overflow-y:scroll; text-align:left; background:#F5F5F5; max-width: 1500px;"
                        ),
                        style = "border: 0px solid #ddd; vertical-align: top;"
                      ),
                    ),
                  ),
                  br(), br(),
                  
                  tags$table(
                    style = "width: 85%; border-collapse: collapse;",
                    
                    tags$tr(
                      tags$td(
                        h5(strong(textOutput("tab_per_i")), style = "color:#DF6919; font-style:italic;"),
                        div(
                          tableOutput("tableau_per_i"),
                          style = "overflow-y:scroll; text-align:left; background:#F5F5F5; max-height:460px; max-width: 1000px;"
                        ),
                        style = "border: 0px solid #ddd; vertical-align: top;"
                      ),
                      
                      tags$td(
                        h5(strong(textOutput("PC_text")), style = "color:#DF6919; font-style:italic;"),
                        div(
                          tableOutput("pivot_col"),
                          style = "overflow-y:scroll; text-align:left; background:#F5F5F5; max-height:460px; max-width: 1000px;"
                        ),
                        style = "border: 0px solid #ddd; vertical-align: top;"
                      ),
                    ),
                  ),
                br(), br(),
                ),
        ),
        
        tabItem("QSI", #accesses menuSubitem of QSI&PR
                sidebarPanel(#ask user for csv file
                  fileInput(inputId = "csv_quadspline",
                            label = "Upload CSV file:",
                            multiple = F,
                            accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv")),
                  
                  h5(textOutput("csv_checker"), style = "color:#FF0000; font-style:italic;"), br(),#checks for inconsistency within data
                  
                  h3(strong("Enter x to estimate desired value:")),#ask user to input estimate x
                  numericInput(inputId = "x", label = NULL, value = 0),
                  
                  actionButton(inputId = "calculate_btn", label = "Calculate QSI", style = "background:#DF6919; color:#FFFFFF;"),#button for calculating qsi
                  
                  h5(textOutput("compute_checker"), style = "color:#FF0000; font-style:italic;"),#checks if all inputs are valid
                  style = "font-size: 20px; background:#2A3950; color:#FFFFFF;"
                ),
                br(),
                
                mainPanel(style = "color:000000;",#outputs user input in QSI
                          fluidRow(
                            column(10, h1("Quadratic Spline Interpolation"), style = "font-size: 20px; background:#2A3950; color:#FFFFFF;")), br(),
                          
                          h2(strong("Resulting Equations with corresponding intervals:")),#outputs equations
                          fluidRow(column(10, tableOutput("qsi_fxns"),
                                          style = "overflow-y:scroll; max-height:200px; font-size: 20px; background:#2A3950; color:#FFFFFF;")), br(),
                          
                          h2(strong("Estimate of x at f(x):")),
                          fluidRow(column(10, textOutput("qsi_est_str"),#outputs qsi estimate
                                          style = "overflow-y:scroll; max-height:200px; font-size: 20px; background:#2A3950; color:#FFFFFF;"))
                )
        ),
        
        tabItem("PR", #accesses menuSubitem of QSI&PR
                sidebarPanel(#ask user to place csv file
                  fileInput(inputId = "csv_polyreg",
                            label = "Upload CSV file:",
                            multiple = F,
                            accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv")),
                  
                  h5(textOutput("csv_pr_check"), style = "color:#FF0000; font-style:italic;"), br(), #checking for inconsistencies within given data
                  
                  h3(strong("Polynomial Degree:")),
                  numericInput(inputId = "deg", label = NULL, value = 0), #ask user numeric input
                  h5(textOutput("deg_checker"), style = "color:#FF0000; font-style:italic;"),#checks for error
                  
                  h3(strong("Enter x to estimate desired value:")),
                  numericInput(inputId = "pr_x", label = NULL, value = 0), #ask user numeric input
                  h5(textOutput("estx_checker"), style = "color:#FF0000; font-style:italic;"), #checks for error
                  
                  actionButton(inputId = "calculate_pr", label = "Calculate PR", style = "background:#DF6919; color:#FFFFFF;"), #calculate button
                  
                  h5(textOutput("compute_check_pr"), style = "color:#FF0000; font-style:italic;"), #double checks for errors in inputs
                  style = "font-size: 20px; background:#2A3950; color:#FFFFFF;"
                ),
                br(),
                
                mainPanel(style = "color:000000;", #outputs user input if valid
                          fluidRow(
                            column(10, h1("Polynomial Regression"), style = "font-size: 20px; background:#2A3950; color:#FFFFFF;")), br(),
                          
                          h2(strong("Generated Polynomial Function:")), #pr functions
                          fluidRow(column(10, tableOutput("pr_fxn"),
                                          style = "overflow-y:scroll; max-height:200px; font-size: 20px; background:#2A3950; color:#FFFFFF;")), br(),
                          
                          h2(strong("Estimate of x at f(x):")),
                          fluidRow(column(10, textOutput("pr_est_x"), #pr estimate
                                          style = "overflow-y:scroll; max-height:200px; font-size: 20px; background:#2A3950; color:#FFFFFF;"))
                )
        )
      ),
      
      absolutePanel( #placing watermark
        tags$div("This application was created by none other than A. Noche aka. artist2tell. ©2023 All Rights Reserved.",
                 style = "position:fixed; bottom:0; right:0; margin-right:10px; margin-bottom:10px; font-size:12px; color:#888;"
                 )
        )
    )
  )

#define server logic
server <- function(input, output, session)
{
  # simplex method diet problem solver
  output$selected_rows = renderPrint(
  {
    #assign user's choices to variable
    selected_rows = input$choices_sm 
    subset_of_data = data_sm[selected_rows, , drop = F]#access every user's choices from data_sm dataframe
    
    if(length(input$choices_sm) > 0)#check if choices length is greater than 0
    {
      output$choices_checker = renderText(NULL)#render text signifying number of foods selected
      output$checkbox_indicator = renderText(paste("**You have selected ", length(selected_rows), " food/s to consider for your diet.**", sep = "", collapse = NULL))
    }
    
    return(subset_of_data)#reads as data frame
  })
  
  observeEvent(input$check_all, #for check all button
  {
    updateCheckboxGroupInput(session, "choices_sm", selected = rownames(data_sm))#updates selected food choices
    output$choices_indicator = renderText(NULL) #renders null to output new table and number of selected foods
    output$serving_mat = renderTable(NULL)
    output$infeasible = renderText(NULL)
    output$Menu = renderText(NULL)
    output$optimal = renderText(NULL)
    output$Soln = renderText(NULL)
    output$soln_cost = renderTable(NULL)
    output$Iteration_num = renderText(NULL)
    output$wrong_input_ite = renderText(NULL)
    output$PR_text = renderText(NULL)
    output$pivot_row = renderTable(NULL)
    output$PC_text = renderText(NULL)
    output$pivot_col = renderTable(NULL)
    output$tab_per_i = renderText(NULL)
    output$tableau_per_i = renderTable(NULL)
  })
  
  observeEvent(input$reset_all, #for reset button
  {
    output$checkbox_indicator = renderText(NULL) #resets all previous user inputs
    output$choices_indicator = renderText(NULL)
    output$serving_mat = renderTable(NULL)
    output$infeasible = renderText(NULL)
    output$Menu = renderText(NULL)
    output$optimal = renderText(NULL)
    output$Soln = renderText(NULL)
    output$soln_cost = renderTable(NULL)
    output$Iteration_num = renderText(NULL)
    output$wrong_input_ite = renderText(NULL)
    output$PR_text = renderText(NULL)
    output$pivot_row = renderTable(NULL)
    output$PC_text = renderText(NULL)
    output$pivot_col = renderTable(NULL)
    output$tab_per_i = renderText(NULL)
    output$tableau_per_i = renderTable(NULL)
    updateCheckboxGroupInput(session, "choices_sm", selected = character(0)) #removes all ticked checkboxes
  })
  
  observeEvent(input$calculate_sm, #for calculate button of sm
  {
    output$choices_checker = renderText(NULL) #render as NULL first
    output$choices_indicator = renderText(NULL)
    output$serving_mat = renderTable(NULL)
    output$infeasible = renderText(NULL)
    output$Menu = renderText(NULL)
    output$optimal = renderText(NULL)
    output$Soln = renderText(NULL)
    output$soln_cost = renderTable(NULL)
    output$Iteration_num = renderText(NULL)
    output$wrong_input_ite = renderText(NULL)
    output$PR_text = renderText(NULL)
    output$pivot_row = renderTable(NULL)
    output$PC_text = renderText(NULL)
    output$pivot_col = renderTable(NULL)
    output$tab_per_i = renderText(NULL)
    output$tableau_per_i = renderTable(NULL)
    chosen_foods = input$choices_sm #assign user choices to variable
    raw_data = data_sm[chosen_foods, -2, drop = F] #access all details of chosen foods from raw data except column with string values
    raw_data = as.matrix(raw_data)

    if(length(chosen_foods) == 0) #check if there are no ticked options
    { #render error message
      output$choices_checker = renderText("Error! You have not selected any food/s for your diet!")
      output$checkbox_indicator = renderText(NULL)
      output$choices_indicator = renderText(NULL)
      output$serving_mat = renderTable(NULL)
      output$infeasible = renderText(NULL)
      output$Menu = renderText(NULL)
      output$optimal = renderText(NULL)
      output$Soln = renderText(NULL)
      output$soln_cost = renderTable(NULL)
      output$Iteration_num = renderText(NULL)
      output$wrong_input_ite = renderText(NULL)
      output$PR_text = renderText(NULL)
      output$pivot_row = renderTable(NULL)
      output$PC_text = renderText(NULL)
      output$pivot_col = renderTable(NULL)
      output$tab_per_i = renderText(NULL)
      output$tableau_per_i = renderTable(NULL)
      return()
    }

    else
    { #initialize vectors with minimum and maximum nutrients
      minNutrient = c(2000, 0, 0, 0, 0, 25, 50, 5000, 50, 800, 10)
      maxNutrient = c(2250, 300, 65, 2400, 300, 100, 100, 50000, 20000, 1600, 30)
      
      #create a matrix containing the unknown nutrients x only
      A = t(raw_data[chosen_foods, -1, drop = F])#remove cost column and take nutrients matrix. Then transpose

      minA = cbind(A, RHS =  minNutrient) #bind columns to minNutrient to create 1st constraint
      minA = -(minA) #turn as negative to flip inequality sign
      maxA = cbind(A, RHS = maxNutrient)  #bind columns to maxNutrient to create 2nd constraint

      cost_vector = t(raw_data[chosen_foods, 1, drop = F]) #get cost vector
      cost_vector = cbind(cost_vector, RHS = rep(1, nrow(cost_vector))) # add 1 for Z value

      #step 1: matrix construction
      untransposed = rbind(minA, maxA, cost_vector) #bind rows of constraints and primal problem 

      #step 2: transpose of the matrix
      transposed = t(untransposed) #transpose

      #step 3: set-up the constraints with slack variables,
      maximize = t(as.matrix(transposed[nrow(transposed),])) #convert to matrix and transpose
      s_t = as.matrix(transposed[-nrow(transposed),]) #convert vector to matrix
      
      if(length(chosen_foods) == 1) #check if chosen foods is equal to 1
      {
        s_t_s = t(as.matrix(transposed[-nrow(transposed), -ncol(transposed)])) #if it is, transpose s_t_s and solution_s_t for consistency of matrix
        solution_s_t = t(as.matrix(transposed[-nrow(transposed),ncol(transposed)]))
      }
      else
      {
        s_t_s = as.matrix(transposed[-nrow(transposed), -ncol(transposed)])
        solution_s_t = as.matrix(transposed[-nrow(transposed),ncol(transposed)])
      }
      # alternating dapat yung matrix. SO... min max and so on. Tapos maglalagay pa ng -1, 10 na extra matrix
      slack_var = diag(nrow(s_t_s)) #get unit matrix in line with number of x's
      
      z_solution = cbind(Z = rep(0, nrow(solution_s_t)), Solution = solution_s_t) #bind z and solution
      max_s = t(maximize[,-ncol(maximize)]) #get s of maximize expression then transpose
      z_max = as.matrix(maximize[,ncol(maximize)])#get last element of maximize for z value. Then convert as matrix
      
      slack_var_max = matrix(0, nrow = 1, ncol = ncol(slack_var))#get slack variables of dual problem
      slack_var_min = matrix(10, nrow = 1, ncol = ncol(slack_var))#get slack variables of dual problem
      
      obj_func = cbind(max_s, slack_var_min, slack_var_max, Z = z_max, Solution = 0) #binding to get objective function
      constraints = cbind(-s_t_s, -slack_var, slack_var, z_solution) #binding to get constraints with slack variables
      initial_Tab = rbind(constraints, obj_func) #create initial tableau
      
      s_names = c()#initialize vectors
      x_names = c()
      
      Tab_names_i = c()
      Tab_names_j = c()

      s_names = c(s_names, paste("s", 1:ncol(s_t_s), sep = "", collapse = NULL)) #setup slack variables colnames
      x_names = c(x_names, paste("x", 1:(ncol(slack_var)), sep = "", collapse = NULL))
      
      Tab_names_j = c(Tab_names_j, s_names, x_names, chosen_foods, "Z", "Solution")
      Tab_names_i = c(Tab_names_i, seq(nrow(initial_Tab)-1), "COST")

      #step 5: set-up initial tableau,
      colnames(initial_Tab) = Tab_names_j #rename columns with the generate variables colnames
      rownames(initial_Tab) = Tab_names_i
      
      #step 6: solve per iteration using GJE algorithm
      dual_prob = Gauss_Jordan_Elimination_v3(initial_Tab)
      
      # print(dual_prob)
      last.e = length(dual_prob)
      
      prices = t(raw_data[,"Price.Serving", drop = F])
      intersection_v = NULL
      
      if(last.e != 0)
      {
        output$Iteration_num = renderText("Iteration Number")
        output$choices_indicator = renderText("Initial Tableau")#renders successful user input
        output$serving_mat = renderTable(initial_Tab)
        output$Menu = renderText("The Optimized Menu")
        
        output$optimal = renderText(paste("The cost of this optimal diet is $", round(dual_prob[[last.e]]$OPTIMAL_VALUE, digits = 4), " per day.", sep = "", collapse = NULL))
        output$Soln = renderText("The Solution and Cost Breakdown by Food")
        basic = dual_prob[[last.e]]$BASIC_SOLUTION
        
        c1 = colnames(basic)
        c2 = colnames(prices)
        intersection_v1 = intersect(c1, c2)
        
        Servings = basic[, intersection_v1, drop = F]
        Cost.S = round((Servings * prices), digits = 2)
        
        Food = as.matrix(colnames(Cost.S))
        Food = t(Food)
        colnames(Food) = colnames(Cost.S)
        
        Filtered.Cost.S = as.matrix(Cost.S[Cost.S != 0])
        rownames(Filtered.Cost.S) = colnames(Cost.S)[Cost.S != 0]
        
        Filtered.Cost.S = t(Filtered.Cost.S)
        
        c3 = colnames(Servings)
        c4 = colnames(Filtered.Cost.S)
        intersection_v2 = intersect(c3, c4)

        Filtered.Servings = round(Servings[, intersection_v2, drop = F], digits = 2)
        
        Filtered.Food = Food[, intersection_v2, drop = F]
        
        Filtered.Food = t(Filtered.Food)
        Filtered.Servings = t(Filtered.Servings)
        Filtered.Cost.S = t(Filtered.Cost.S)
        
        Final_soln = cbind(Filtered.Food, Filtered.Servings, Filtered.Cost.S)
        colnames(Final_soln) = c("Food", "Servings", "Cost($)")
        rownames(Final_soln) = seq(1, length(intersection_v2))
        
        output$soln_cost = renderTable(Final_soln)
        
        observeEvent(input$check_ite,
        {
          tryCatch(#catches error when user inputs
            {
              output$wrong_input_ite = renderText(NULL)
              output$PR_text = renderText(NULL)
              output$pivot_row = renderTable(NULL)
              output$PC_text = renderText(NULL)
              output$pivot_col = renderTable(NULL)
              output$tab_per_i = renderText(NULL)
              output$tableau_per_i = renderTable(NULL)

              value = input$iterate_i
              if(is.na(value))#checks for no input
              {
                output$wrong_input_ite = renderText("Put valid value!")
                output$PR_text = renderText(NULL)
                output$pivot_row = renderTable(NULL)
                output$PC_text = renderText(NULL)
                output$pivot_col = renderTable(NULL)
                output$tab_per_i = renderText(NULL)
                output$tableau_per_i = renderTable(NULL)
              }
              
              else if(!(is.numeric(value)))
              {
                output$wrong_input_ite = renderText("Put valid value!")
                output$PR_text = renderText(NULL)
                output$pivot_row = renderTable(NULL)
                output$PC_text = renderText(NULL)
                output$pivot_col = renderTable(NULL)
                output$tab_per_i = renderText(NULL)
                output$tableau_per_i = renderTable(NULL)
              }
              
              else if(is.numeric(value))
              {
                output$wrong_input_ite = renderText(NULL)
                if(value %in% seq(0, (last.e - 1)))
                {
                  if(value != 0)
                  {
                    extracted_row = dual_prob[[value + 1]]$PIVOT_ROW
                    extracted_col = dual_prob[[value + 1]]$PIVOT_COLUMN
                    output$PR_text = renderText(paste("Pivot Row (R", rownames(extracted_row), ")", sep = "", collapse = NULL))
                    output$pivot_row = renderTable(extracted_row)
                    
                    output$PC_text = renderText(paste("Pivot Column (", colnames(extracted_col), ")", sep = "", collapse = NULL))
                    output$pivot_col = renderTable(extracted_col)
                    
                    output$tab_per_i = renderText(paste("Tableau of Iteration ", value, sep = "", collapse = NULL))
                    output$tableau_per_i = renderTable(dual_prob[[value + 1]]$ACM)
                  }
                  
                  else
                  {
                    output$PR_text = renderText(NULL)
                    output$pivot_row = renderTable(NULL)
                    output$PC_text = renderText(NULL)
                    output$pivot_col = renderTable(NULL)
                    output$tab_per_i = renderText(paste("Tableau of Iteration ", value, sep = "", collapse = NULL))
                    output$tableau_per_i = renderTable(dual_prob[[value + 1]]$ACM)
                  }
                }
                
                else
                {
                  output$wrong_input_ite = renderText("Cannot Find iteration number!")
                  output$PR_text = renderText(NULL)
                  output$pivot_row = renderTable(NULL)
                  output$PC_text = renderText(NULL)
                  output$pivot_col = renderTable(NULL)
                  output$tab_per_i = renderText(NULL)
                  output$tableau_per_i = renderTable(NULL)   
                }
              }
            },
            error = function(e)
            { #also renders error message for NA values
              output$wrong_input_ite = renderText("Put valid value!")
              output$PR_text = renderText(NULL)
              output$pivot_row = renderTable(NULL)
              output$PC_text = renderText(NULL)
              output$pivot_col = renderTable(NULL)
              output$tab_per_i = renderText(NULL)
              output$tableau_per_i = renderTable(NULL)
            })
        })
        
      }
      
      else
      {
        output$choices_indicator = renderText(NULL)
        output$serving_mat = renderTable(NULL)
        output$infeasible = renderText("**The problem is infeasible.**")
        output$Menu = renderText(NULL)
        output$optimal = renderText(NULL)
        output$Soln = renderText(NULL)
        output$soln_cost = renderTable(NULL)
        output$Iteration_num = renderText(NULL)
        output$wrong_input_ite = renderText(NULL)
        output$PR_text = renderText(NULL)
        output$pivot_row = renderTable(NULL)
        output$PC_text = renderText(NULL)
        output$pivot_col = renderTable(NULL)
        output$tab_per_i = renderText(NULL)
        output$tableau_per_i = renderTable(NULL)
      }

    }

  })
  
  # quadratic spline interpolation
  observeEvent(input$csv_quadspline, #for uploading csv file
  {
    tryCatch(#catch errors
      {
        output$csv_checker = renderText(NULL) #renders null to avoid overlapping error messages
        output$compute_checker = renderText(NULL)
        output$qsi_fxns = renderTable(NULL)
        output$qsi_est_str = renderText(NULL)
        
        file = input$csv_quadspline #ask user to place csv file
        data = read.csv(file$datapath, header = F, sep = ",", quote = "") #disregards first row as header
        colnames(data) = c("x", "fx") #assigns data with x and f(x)
        
        x = data[[1]]#accesses values
        fx = data[[2]]
        n = ncol(data)

        print(x)
        print(fx)
                
        if(any(is.na(x)) || any(is.na(fx)))#checks if there are inconsistencies within csv file
        {
          output$csv_checker = renderText("Error! CSV file contains NA values.")
          output$compute_checker = renderText("Error! Cannot compute values.")
          output$qsi_fxns = renderTable(NULL)
          output$qsi_est_str = renderText(NULL)
        }
        
        sorted_indices = order(fx) # get indices of fx sorted in ascending order
        x = x[sorted_indices]
        fx = fx[sorted_indices]
        
        print(x)
        print(fx)
        
        if(n > 2) #checks if there are too many datapoints from csv file.
        {
          output$csv_checker = renderText("Error! Cannot determine x and f(x) datapoints.")
          output$compute_checker = renderText("Error! Cannot compute values.")
          output$qsi_fxns = renderTable(NULL)
          output$qsi_est_str = renderText(NULL)
        }
      },
      
      error = function(e)#error returns when csv is NA
      {
        output$csv_checker = renderText("Error! Your CSV file does not contain any datapoints!")
        output$compute_checker = renderText(NULL)
        output$qsi_fxns = renderTable(NULL)
        output$qsi_est_str = renderText(NULL)
      })
  })
  
  observeEvent(input$x, #for inputting x
  {
    tryCatch(#catches error when user inputs
      {
        output$csv_checker = renderText(NULL) #initialize as NULL to avoid overlapping error messages
        output$compute_checker = renderText(NULL)
        output$qsi_fxns = renderTable(NULL)
        output$qsi_est_str = renderText(NULL)
        
        value = input$x
        if(is.na(value))#checks for no inpur
        {
          output$csv_checker = renderText(NULL)#renders error message for NA values
          output$compute_checker = renderText("Error! unknown value x is missing.")
          output$qsi_fxns = renderTable(NULL)
          output$qsi_est_str = renderText(NULL)
        }
      },
      error = function(e)
      { #also renders error message for NA values
        output$csv_checker = renderText(NULL)
        output$compute_checker = renderText("Error! unknown value x is missing.")
        output$qsi_fxns = renderTable(NULL)
        output$qsi_est_str = renderText(NULL)
      })
  })
  
  observeEvent(input$calculate_btn, #for calculate button of qsi
  {
    tryCatch( #cataches error
      {
        output$csv_checker = renderText(NULL) #initialize as null to avoid overlapping error messages
        output$compute_checker = renderText(NULL)
        output$qsi_fxns = renderTable(NULL)
        output$qsi_est_str = renderText(NULL)
        
        file = input$csv_quadspline #gets uploaded csv file
        data = read.csv(file$datapath, header = F, sep = ",", quote = "") #disregards first row as labels
        colnames(data) = c("x", "fx") #assigns x and f(x) on the datapoints
        n = ncol(data)
        
        x = data[[1]] #accesses x and f(x)
        fx = data[[2]]
        
        value = input$x #accesses user input for x
        index = NA
        
        if(is.na(value)) #checks NA value
        {
          output$csv_checker = renderText(NULL)
          output$compute_checker = renderText("Error! unknown value x is missing.")
          output$qsi_fxns = renderTable(NULL)
          output$qsi_est_str = renderText(NULL)
        }
        
        if(any(is.na(x)) || any(is.na(fx)))
        { #checks if any of the datapoints are NA
          output$csv_checker = renderText("Error! CSV file contains NA values.")
          output$compute_checker = renderText("Error! Cannot compute values.")
          output$qsi_fxns = renderTable(NULL)
          output$qsi_est_str = renderText(NULL)
        }
        
        if(n > 2)
        { #checks if there are 2 columns of data points only
          output$csv_checker = renderText("Error! Cannot determine x and f(x) datapoints.")
          output$compute_checker = renderText("Error! Cannot compute values.")
          output$qsi_fxns = renderTable(NULL)
          output$qsi_est_str = renderText(NULL)
        }

        
        xu = NA #initialize variables
        xl = NA
        interval_checker = 0
        
        for(i in 1:(length(x) - 1))
        {
          xu = x[i + 1]
          xl = x[i]
          if(value >= xl && value <= xu)
          {
            interval_checker = 1
            break #checks if it is within interval
          }
          
          else
            interval_checker = 0
        }
        if(interval_checker == 0) #renders error message
        {
          output$csv_checker = renderText(NULL)
          output$compute_checker = renderText("Error! Cannot compute values. Unknown x should be within interval of datapoints.")
          output$qsi_fxns = renderTable(NULL)
          output$qsi_est_str = renderText(NULL)
        }

        else
        { #initialize outputs as NULL
          output$csv_checker = renderText(NULL)
          output$compute_checker = renderText(NULL)
          data_list = list(x, fx) #turning x and f(x) as a data
          result = poly.quadspline(data_list, value) #plugging in to the poly-quadspline function
          
          # gets every generated intervals and its corresponding function
          resulting_mat = matrix("", nrow = length(result[[1]]), ncol = 2, dimnames = list(c(), c("Interval", "Function at the given interval")))
          
          # Iterate each polynomials
          for(i in 1:length(result[[1]])) 
          {
            resulting_mat[i, 1] <- paste("[", x[i], ", ", x[i + 1], "]", sep = "", collapse = NULL)
            resulting_mat[i, 2] <- deparse(result[[1]][[i]])[2] #creates interval string in this form [i, j]
            
            if(value >= x[i] && value <= x[i + 1])#checks if value is within interval, if it is, update index
              index = i
          }
          
          output$qsi_fxns = renderTable({resulting_mat}, width = "1500px", align = "c") #renders table center aligned
          
          if(index > 0) #double checks if index is greater than 0
          {
            value_sub = gsub("x", value, resulting_mat[index, 2]) #generate function estimate
            output$qsi_est_str = renderText(paste("f(", value, ") = ", value_sub, " -> ", signif(result[[2]], digits = 4), sep = "", collapse = NULL))
          }
        }
        
      },
      error = function(e)
      { #render error if any errors were catch in the process.
        output$csv_checker = renderText(NULL)
        output$compute_checker = renderText("Unexpected error! You did not complete and satisfy needed input.")
        output$qsi_fxns = renderTable(NULL)
        output$qsi_est_str = renderText(NULL)
      })
  })
  
  # polynomial regression
  observeEvent(input$csv_polyreg, #for csv file
  {
    tryCatch(#catches error
    {
      output$csv_pr_check = renderText(NULL) #renders null to avoid error message overlapping
      output$compute_check_pr = renderText(NULL)
      output$pr_fxn = renderText(NULL)
      output$pr_est_x = renderText(NULL)
      
      file_pr = input$csv_polyreg #reads and accepts csv file
      data_pr = read.csv(file_pr$datapath, header = F, sep = ",", quote = "")
      colnames(data_pr) = c("x", "fx") #disregards 1st row as header and assigns values with x and f(x)
      n_pr = ncol(data_pr)#get ncolumn
      
      x_polyr = data_pr[[1]]#accesses x and f(x)
      fx_pr = data_pr[[2]]
      
      if(any(is.na(x_polyr)) || any(is.na(fx_pr)))
      {#rendertext if any datapoints are NA
        output$csv_pr_check = renderText("Error! CSV file contains NA values.")
        output$compute_check_pr = renderText(NULL)
        output$pr_fxn = renderText(NULL)
        output$pr_est_x = renderText(NULL)
      }
      
      if(n_pr > 2)
      {#check if csv file contains many datapoints
        output$csv_pr_check = renderText("Error! Cannot determine x and f(x) datapoints.")
        output$compute_check_pr = renderText(NULL)
        output$pr_fxn = renderText(NULL)
        output$pr_est_x = renderText(NULL)
      }
      
    },
    
    error = function(e)
    { #catch error for csv file with no datapoints
      output$csv_pr_check = renderText("Error! Your CSV file does not contain any datapoints!")
      output$compute_check_pr = renderText(NULL)
      output$pr_fxn = renderText(NULL)
      output$pr_est_x = renderText(NULL)
    })
  })
  
  observeEvent(input$deg,
  {#for any input in degree
    tryCatch(#catch errors
    {
      output$deg_checker = renderText(NULL) #initialize as null
      output$compute_check_pr = renderText(NULL)
      output$pr_fxn = renderText(NULL)
      output$pr_est_x = renderText(NULL)
      
      degree = input$deg
      
      if(is.na(degree))
      {#check if there are any NA values
        output$deg_checker = renderText("Error! Polynomial degree value is missing!")
        output$compute_check_pr = renderText(NULL)
        output$pr_fxn = renderText(NULL)
        output$pr_est_x = renderText(NULL)
      }
      
      if(degree < 1)
      {#check if less than 1 for degree
        output$deg_checker = renderText("Error! Polynomial degree should not be less than 1!")
        output$compute_check_pr = renderText(NULL)
        output$pr_fxn = renderText(NULL)
        output$pr_est_x = renderText(NULL)
      }
      
    },
    
    error = function(e)
    { #render text as error if missing degree input value
      output$deg_checker = renderText("Error! Polynomial degree value is missing!")
      output$compute_check_pr = renderText(NULL)
      output$pr_fxn = renderText(NULL)
      output$pr_est_x = renderText(NULL)
    })
  })
  
  observeEvent(input$pr_x,
  { #for x inpur
    tryCatch(
    {#catch errors
      output$estx_checker = renderText(NULL)
      output$compute_check_pr = renderText(NULL)
      output$pr_fxn = renderText(NULL)
      output$pr_est_x = renderText(NULL)
      
      value_pr = input$pr_x #assign user input to variable
      
      if(is.na(value_pr)) #check if x estimate is missing
      {
        output$estx_checker = renderText("Error! x estimate is missing!")
        output$compute_check_pr = renderText(NULL)
        output$pr_fxn = renderText(NULL)
        output$pr_est_x = renderText(NULL)
      }
      
    },
    
    error = function(e)
    { #render error output for missing x estimate
      output$estx_checker = renderText("Error! x estimate is missing!")
      output$compute_check_pr = renderText(NULL)
      output$pr_fxn = renderText(NULL)
      output$pr_est_x = renderText(NULL)
    })
  })
  
  observeEvent(input$calculate_pr,
  {#for calculate button in pr
    tryCatch(#catch errors
    {
      output$csv_pr_check = renderText(NULL) #initialize as NULL
      output$deg_checker = renderText(NULL)
      output$estx_checker = renderText(NULL)
      output$compute_check_pr = renderText(NULL)
      output$pr_fxn = renderText(NULL)
      output$pr_est_x = renderText(NULL)
      
      file_pr = input$csv_polyreg
      data_pr = read.csv(file_pr$datapath, header = F, sep = ",", quote = "")
      colnames(data_pr) = c("x", "fx") #get x and fx from raw input
      n_pr = ncol(data_pr)
      
      
      x_polyr = data_pr[[1]]
      fx_pr = data_pr[[2]]#access x and f(x) of csv file
      
      value_pr = input$pr_x #gets degree and x
      degree = input$deg
      
      if(is.na(degree)) #checks if user input is missing for degree
      {
        output$deg_checker = renderText("Error! Polynomial degree value is missing!") #render error message and set other outputs as null
        output$compute_check_pr = renderText(NULL)
        output$pr_fxn = renderText(NULL)
        output$pr_est_x = renderText(NULL)
      }
      
      if(degree < 1)
      { #check if degree is less than 1
        output$deg_checker = renderText("Error! Polynomial degree should not be less than 1!")
        output$compute_check_pr = renderText(NULL)
        output$pr_fxn = renderText(NULL)
        output$pr_est_x = renderText(NULL)
      }
      
      if(is.na(value_pr)) #check for na value in estimate x
      {
        output$estx_checker = renderText("Error! x estimate is missing!")
        output$compute_check_pr = renderText(NULL) #render outputs
        output$pr_fxn = renderText(NULL)
        output$pr_est_x = renderText(NULL)
      }
      
      if(any(is.na(x_polyr)) || any(is.na(fx_pr))) #check for any na values in for x and f(x)
      {#render error messages
        output$csv_pr_check = renderText("Error! CSV file contains NA values.")
        output$compute_check_pr = renderText("Error! Cannot compute values.")
        output$pr_fxn = renderText(NULL)
        output$pr_est_x = renderText(NULL)
      }
      
      if(n_pr > 2)
      {#check for more than 2 datapoints
        output$csv_pr_check = renderText("Error! Cannot determine x and f(x) datapoints.")
        output$compute_check_pr = renderText(NULL) #render outputs
        output$pr_fxn = renderText(NULL)
        output$pr_est_x = renderText(NULL)
      }
      
      if(value_pr < (degree + 1)) #checks if value_pr is less than degree
      {
        output$estx_checker = renderText(paste("Error! x estimate should be greater than or equal to ", (degree + 1), "!", sep = "", collapse = NULL))
        output$compute_check_pr = renderText("Error! Regression is impossible to calculate!")
        output$pr_fxn = renderText(NULL) #render outputs
        output$pr_est_x = renderText(NULL)
      }
      
      else if(value_pr >= (degree + 1)) #if not less than degree
      {
        data_list_pr = list(x_polyr, fx_pr)
        result_pr = PolynomialRegression(data_list_pr, degree, value_pr)
        poly_eq = sub("^function \\(x\\) ", "", result_pr[[1]]) #deconstruct function(x)
        poly_eq_x = gsub("x", value_pr, poly_eq) # get poly equation
        
        output$pr_fxn = renderText(paste("f(x) = ", poly_eq, sep = "", collapse = NULL)) #get generated expressions
        output$pr_est_x = renderText(paste("f(", value_pr, ") = ", poly_eq_x, " -> ", result_pr[[3]], sep = "", collapse = NULL))
        #find estimate
      }
    },
    
    error = function(e)
    {# catch error for any rejected conditions
      output$compute_check_pr = renderText("Error! You did not satisfy required input!")
      output$pr_fxn = renderText(NULL)
      output$pr_est_x = renderText(NULL)
    })
  })
  
}

shinyApp(ui = ui, server = server) #Run the application