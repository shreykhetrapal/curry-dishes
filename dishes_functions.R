# Functions file

# Extracting data from rHandsonTable 
extract_data <- function(dish_data){
  
  # dish_data is a list of 3 with the first item 'data' containing all the info 
  
  dish_data %>% 
    unlist() -> values
  
  tibble(ingredient = values[1], 
         quantity = values[2],
         unit = values[3])
    
  
}

# validating dish inputs 
validate_input <- function(all_details, unit_list) {
  
  flag <- 0
  error_msg <- ""
  
  if(all_details$dish_name == ""){
    list(flag = 1,
         error_msg = "Please enter a Dish Name") -> error_list
    
    return(error_list)
  }
  
  if(all_details$unit %in% unit_list == F){
    list(flag = 1,
         error_msg = "Please enter a valid unit") -> error_list
    
    return(error_list)
  }
  
  error_list <- list(flag = flag, 
                     error_msg = error_msg)
  
  return(error_list)
}

# Checking and adding new ingredients to list 
add_new_ingredients <- function(all_details, ingredient_list){
  
  all_details$ingredient -> ingredients_new
  
  setdiff(ingredients_new, ingredient_list) -> new_items
  
  ingredient_list <- c(ingredient_list, new_items)
  
  ingredient_list %>% saveRDS("ingredient_list.rds")
}
