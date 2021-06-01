library(tidyverse)
library(shiny)
library(DT)
library(rhandsontable)
library(stringr)
library(tokenizers)
library(rmarkdown)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(tictoc)

source("dishes_functions.R")



ui <- tagList(
  

    fluidPage(
  titlePanel("Curry Dishes"),
  navlistPanel(
    widths = c(2, 10),
    "Header",
    tabPanel("Choose Dish",
             column(6, 
                    h3("Choose a dish"),
                    DT::dataTableOutput("view_dishes"),
                    column(2, 
                           actionButton("view_dish", "View")), 
                    column(2, 
                           downloadButton("download_pdf", "PDF", icon = icon("file-download")))
             ), 
             column(6, 
                    h3("Preview dish"),
                    htmlOutput("view_html_dish"))
             
    ),
    tabPanel("Add Dish",
             column(7, 
                    fluidRow(
                      column(6, 
                             textInput("dish_name", "Dish Name", placeholder = "Butter Chicken")
                      ), 
                      column(6, 
                             selectizeInput("dish_author", "Author", 
                                            options = list(create = TRUE), 
                                            choices = c("Shrey"))
                      )
                    ),
                    fluidRow(
                      column(6, 
                             selectInput("dish_type", "Type", choices = c("Veg", "Non Veg")),
                             column(6,
                                    sliderInput("dish_spice", "Spice Level", min = 0, max = 5, step = 1, value = 3)),
                             column(6, 
                                    selectInput("cooking_time", "Approx Time", choices = c("10 min","15 min", "30 min", "45 min", "1hr", "1hr 15 min", "1hr 30 min", "2hr","2hr 30 min", "More than 3 hr")))
                      ), 
                      column(6, 
                             selectInput("meal_type", "Meal Type", choices = c("Breakfast", "Lunch", "Dinner", "Snack")), 
                             sliderInput("servings", "Serving", min = 1, max = 10, step = 1, value = 4)
                      )
                    ),
                    
                    h5("Start by adding ingredients"), 
                    
                    rHandsontableOutput("add_dish_table"),
                    actionButton("add_dish", "Add Dish")
             ),
             column(5, 
                    textAreaInput("steps", "Enter steps for making", 
                                  resize = "vertical", 
                                  height = '400px'))
    ),
    tabPanel("Third",
             h3("This is the third panel")
    )
  )
)
)

server <- function(input, output, session) {
  
  directory_name <<- as.numeric(Sys.time()) %>% as.character() %>% str_remove_all("\\.")
  
  dir.create(paste0("./downloaded_pdfs/",directory_name))
  
  # dishes_list <- readRDS("dishes_list.rds")
  dishes_list <- reactiveValues(data = readRDS("dishes_list.rds"))
  
  observe({
    updateSelectInput(session, "dish_author", choices = lapply(dishes_list$data, function(x) x$author) %>% 
                                                            unlist() %>% 
                                                            unique())
  })
  
  unit_list <- c("grams","kg", "spoon","cup", "piece")
  # ingredient_list <- readRDS("ingredient_list.rds")
  ingredient_list <- reactiveValues(data = readRDS("ingredient_list.rds"))
  
  add_dish_table_reactive <- reactive({
    
    tibble(ingredient = "shrey",
           quantity = 1, 
           unit = "grams") -> initial

  return(initial)
    
  })
  
  output$add_dish_table <- renderRHandsontable({
    
    rhandsontable(add_dish_table_reactive(), 
                  width = 400, 
                  height = 300,
                  stretchH = "all", 
                  selectCallback = TRUE) %>%
      hot_col(col = "ingredient", type = "autocomplete", source = ingredient_list$data, strict = F) %>% 
      hot_col(col = "unit", type = "dropdown", source = unit_list) %>% 
      hot_validate_numeric(col = "quantity", min = 0.25) %>% 
      hot_validate_character(col = "unit", choices = unit_list)
      
    
  })
  
  # Save a new dish
  observeEvent(input$add_dish, 
               {
                 # Gathering inputs
                 input$dish_name ->> name
                 input$dish_author ->> author
                 input$dish_type ->> veg_non
                 input$dish_spice -> spice
                 input$meal_type ->> meal_type
                 input$servings ->> servings
                 input$steps ->> steps
                 input$cooking_time ->> cooking_time
                 
                 # gather data 
                 input$add_dish_table ->> dish_table
                 map_dfr(dish_table$params$data, extract_data) ->> ingredient_details
                 
                 # tibble from inputs 
                 ingredient_details %>% 
                   mutate(dish_name = name, 
                          dish_author = author, 
                          dish_type = veg_non, 
                          spice_level = spice, 
                          meal_type = meal_type, 
                          servings = servings) ->> all_details
                 
                 # input validation 
                 validate_input(all_details, unit_list) -> input_check 
                 
                 # Checking and adding new ingredients to list 
                 add_new_ingredients(all_details, ingredient_list$data)
                 
                 if(input_check$flag == 1){
                   showModal({
                     modalDialog(title = "Error", 
                                 h4(input_check$error_msg), 
                                 size = "s", 
                                 easyClose = TRUE
                                 )
                   })
                 }else {
                   
                   dishes_list$data %>% length() -> numbering_of_dish
                  
                   # Making list of all items 
                   list(
                     s_no = numbering_of_dish+1, 
                     author = author, 
                     name = name, 
                     dish_type = veg_non, 
                     spice_level = spice, 
                     meal_type = meal_type, 
                     servings = servings,
                     ingredents = ingredient_details,
                     steps_to_make = steps,
                     cook_time = cooking_time
                   ) -> full_list
                   
                   full_list -> dishes_list$data[[numbering_of_dish+1]]
                   
                   dishes_list$data %>% saveRDS("dishes_list.rds")
                   
                   dishes_list$data <- readRDS("dishes_list.rds")
                 }
                 
                 show_alert(
                   title = "Success !!",
                   text = "All in order",
                   type = "success"
                 )
                 
               })
  
  
  get_dishes <- reactive({
    
    dishes_list$data %>% 
      map_dfr(extract_dishes)
    
  })
  # View the dishes 
  output$view_dishes <- renderDataTable({
    datatable(get_dishes(), 
              selection = "single")
  
  })
  
  # Extract parameters to be passed on to the RMD 
  
  parameter_list <- reactive({
    
    req(input$view_dishes_rows_selected)
    
    input$view_dishes_rows_selected -> dish_id
    
    extract_dish_data(dishes_list$data, dish_id) -> extracted_dish_data
    
    # Params for R Markdown
    params <- list(n = extracted_dish_data)
    
  })
  
  observeEvent(input$view_dish,{
  
   
    withProgress({
      
      rmarkdown::render("view_dish.Rmd", output_format = "html_document", output_file = 'www/preview_dish.html',
                        params = parameter_list(),
                        envir = new.env(parent = globalenv()),clean=F,encoding="utf-8"
      )
      
      incProgress(amount = 1.5, message = "Showing dish")
      
      output$view_html_dish <- renderUI({
        tags$iframe(style="height:740px; width:100%", src="preview_dish.html")
      })
      
    }, 
    
    min = 0, 
    max = 3, 
    value = 1, 
    message = "Fetching dish")
    
    show_toast(
      title = "Here's the yummy dish",
      text = "Enjoy",
      type = "success",
      timer = 2000,
      timerProgressBar = TRUE,
      position = "bottom-end",
    )
    
    
  })
  
  output$download_pdf <- downloadHandler(
    
    filename = function() {
      paste("")
    },
    content = function(file) {
      
      withProgress({
        
        rmarkdown::render("pdf_dish.Rmd", output_format = "pdf_document", output_file = paste0("downloaded_pdfs/",directory_name,"/","pdf_dish.pdf"),
                          params = parameter_list(),
                          envir = new.env(parent = globalenv()),clean=F,encoding="utf-8"
        )
        
        setProgress(value = 2, message = "Generation complete")
        
      }, 
      min = 0,
      max = 3, 
      value = 1, 
      message = "Generating pdf", 
      detail = "this may take a while")
     
      
    }
  )
  
  
}

shinyApp(ui, server)
