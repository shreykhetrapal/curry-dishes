library(tidyverse)
library(shiny)
library(DT)
library(rhandsontable)
library(stringr)
library(tokenizers)

source("dishes_functions.R")

ui <- fluidPage(
  titlePanel("Curry Dishes"),
  navlistPanel(
    widths = c(2, 10),
    "Header",
    tabPanel("Choose Dish",
             column(6, 
                    h3("This will have options to choose the dish and make customisations")
             ), 
             column(6, 
                    h3("Space for the html output"))
             
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
                             sliderInput("dish_spice", "Spice Level", min = 0, max = 5, step = 1, value = 3)
                      ), 
                      column(6, 
                             selectInput("meal_type", "Meal Type", choices = c("Breakfast", "Lunch", "Dinner", "Snack")), 
                             sliderInput("servings", "Serving", min = 1, max = 10, step = 1, value = 4)
                      )
                    ),
                    
                    h5("Start by adding ingredients"), 
                    
                    rHandsontableOutput("add_dish_table"),
                    actionButton("save_ingredient", "Add ingredient")
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

server <- function(input, output, session) {
  
  dishes_list <- readRDS("dishes_list.rds")
  
  unit_list <- c("grams","kg", "spoon","cup", "piece")
  ingredient_list <- readRDS("ingredient_list.rds")
  
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
      hot_col(col = "ingredient", type = "autocomplete", source = ingredient_list, strict = F) %>% 
      hot_col(col = "unit", type = "dropdown", source = unit_list) %>% 
      hot_validate_numeric(col = "quantity", min = 0.5) %>% 
      hot_validate_character(col = "unit", choices = unit_list)
      
    
  })
  
  
  observeEvent(input$save_ingredient, 
               {
                 # Gathering inputs
                 input$dish_name ->> name
                 input$dish_author ->> author
                 input$dish_type ->> veg_non
                 input$dish_spice -> spice
                 input$meal_type ->> meal_type
                 input$servings ->> servings
                 input$steps ->> steps
                 
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
                 add_new_ingredients(all_details, ingredient_list)
                 
                 if(input_check$flag == 1){
                   showModal({
                     modalDialog(title = "Error", 
                                 h4(input_check$error_msg), 
                                 size = "s", 
                                 easyClose = TRUE
                                 )
                   })
                 }else {
                   
                   dishes_list %>% length() -> numbering_of_dish
                  
                   # Making list of all items 
                   list(
                     author = author, 
                     name = name, 
                     dish_type = veg_non, 
                     spice_level = spice, 
                     meal_type = meal_type, 
                     servings = servings,
                     ingredents = ingredient_details,
                     steps_to_make = steps
                   ) -> full_list
                   
                   full_list -> dishes_list[[numbering_of_dish+1]]
                   
                   dishes_list %>% saveRDS("dishes_list.rds")
                   
                 }
                 
               })
  
  
  
  
  
}

shinyApp(ui, server)
