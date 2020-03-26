#===================================
#   ---  OUR FUNCTIONS  ----
#
# This R-Code contains a list of customized functions
#   that can be re-used for a particular purposes.
# 
# Repo: https://github.com/penmaypa/penuel-library/raw/master/R/our_functions.r
#
#=====================================
message(
  cat(
    "\nImporting Our_Function.r ....\n"
  )
)
message(
  cat(" \n==========================================================
      \n Our_Functions.r -- created by Penuel Maypa 
     \nThis R file contains a collection of customized functions
      \n ========================================================= \n"  
  )
)

#==========================
#  PACKAGES & LIBRARIES
#--------------------------
library(crayon)



get_message__column_name_warning <- function(){
  message(
    cat("\n-------------------------------------------------------------------------------------"),
    cat("\n  ATTENTION !:"),
    cat("\n  Please input column by their header name i.e \"Name\" instead of \"data$Name\" "),
    cat("\n---------------------------------------------------------------------------------------")
  )
}

# DESC:  Cuts down the amount of data
# INPUT: df = dataframe | start_row | end_row
# OUTPUT:  dataframe - a cut trimmed/cutdown version of the dataframe
cutdown_data <- function(df, start_row, end_row){
  df_output <- df[start_row:end_row,]
  
  return(df_output)
}


# Check for missing/invaled values:
#   if the result is: integer(0), then there is no missing value.
#
check_invalid_values <- function(df){
  the_output <- grep(
    TRUE,
    is.na(df)
  )
  
  return(the_output)
}


# DESC: Compare two dataframe and check if their column are similar
# INPUT: df_1 , df_2 
# OUTPUT: Returns Boolean; True - if similar and False - if not similar
#
compare_two_dataframe_if_colnames_are_similar <- function(df_1, df_2){

  # Input Validation Check
  if(
    class(df_1)=="data.frame"
    &&
    class(df_2)=="data.frame"
  ){
    
    df1_names <- names(df_1)
    df2_names <- names(df_2)
    is_names_similar <- TRUE
    
    n1 <- 1
    for(name in df1_names){
      
      if(name == df2_names[n1] && is_names_similar == TRUE){
        is_names_similar <- TRUE
      }else{
        is_names_similar <- FALSE
        message("\n Column name are not similar \n")
        break
      }
      
      n1 <- n1 + 1
    }
    
    return_x <- is_names_similar
  }else{
    
    message(
      cat("\n----------------------------\n"),
      cat("   Attetion ! \n\n"),
      cat("  The inputs are not data frame."),
      cat("\n  Please make sure to input a \"data.frame\" type"),
      cat("\n\n-----------------------------")
    )
    
  }
  
  return(return_x)
}


# DESC: Random rows
# INPUT: df , n
randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}

# DESC: Checks if date is formatted in ISO 
# INPUT: columns that contains Dates
# OUTPUT: Boolean - If dates is ISO format
check_date_iso <- function(date){
  
  if(class(date)== "data.frame"){
    date <- date[,1]
  }
  
  #date <- sample(date,3, replace=TRUE)
  
  cont <- TRUE
  n_item =1 
  for(item in date){
    value <- date[n]
    value <- as.character(value)
    value <- strsplit(value,"")[[1]]
    bool_iso <- NULL
    
    if(cont == TRUE){
      
      char_nx <- 1
      for(char in value){
        
        # Checking year
        if( (char_nx <= 4) && (cont==TRUE) ){
          if((is_char_a_number(char)==TRUE) && (cont == TRUE)){
            cont <-  TRUE
          }else{
            cont <- FALSE
          }
        }
        
        
        if((char_nx == 5) && (cont == TRUE)){
          if(char == "-"){
            cont <- TRUE
          }else{
            cont <- FALSE
          }
        }
        
        if((char_nx == 6 || char_nx == 7) && (cont==TRUE) ){
          if((is_char_a_number(char)==TRUE) && (cont == TRUE)){
            cont <- TRUE
          }else{
            cont <- FALSE
          }
        }
        
        if( (char_nx == 8) && (cont==TRUE) ){
          if(char=="-"){
            cont <- TRUE
          }else{
            cont <- FALSE
          }
        }
        
        if( (char_nx == 9 || char_nx == 10) && (cont==TRUE) ){
          if((is_char_a_number(char)==TRUE)){
            cont <- TRUE
            bool_iso <- TRUE
          }else{
            cont <- FALSE
            bool_iso <- FALSE
          }
        }
        print(cont)
        char_nx <- char_nx+1
      }
      
    }else{
      bool_iso <- FALSE
    }
    
    n_item <- n_item+1
  }
  
  return_x <- bool_iso
  return (return_x)
}

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# DESC: !!!
compare_two_tables_and_find_difference_in_value <- function(df_1, df_2, df1_column, df_column){
  
  #-------------
  df_1 <- afinn_sent_v2
  df_2 <- bing_sent
  df1_column <- "Word"
  df2_column <- "Word"
  
  #-------------
  
  df1_key <- df1_column
  df2_key <- df2_column
  
  
  if(
    (class(df1_key)== "character") &&
    (class(df2_key)== "character")
  ){
    
    
    
  }else{
    message(
      cat("-------------------------------------------------------------------------------"),
      cat("\n   WARNING:"),
      cat("\n  Please input the name of column \"Date\" in instead of \"data$DATE\" "),
      cat("\n------------------------------------------------------------------------------")
    )
  }
}

compare_two_tables_by_similar_value(){
  
}

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# DESC: Find rows where value is equals = x 
# INPUT: 
# OUPUT: 
find_row_where_value <- function(df,column_name, value){
  
  if( class(value) == "character" ){
    return_df <- df[which(df[,column_name] == value), ]
  }else{
    message(
      cat(yellow("\n  ----------------------------------------------------------")),
      cat(yellow("\n\t Please make sure to type name of the column: ")),
      cat(yellow("\n\t \"Heading\" instead of \"data$name\" ")),
      cat(yellow("\n  --------------------------------------------------------------"))
    )
  }
  
  return(return_df)
}

# FUNCTIONS:
#   DESC: Checks if variable is numerical
#   INPUTS: Character
#   RETURNS : Boolean - True if its a number, False if its not
#
is_char_a_number <- function(x){
  numbers = c("0","1","2","3","4","5","6","7","8","9")
  return(x %in% numbers)
}


# FUNCTIONS:
#   INPUT: Variable (cell)
#   OUTPUT: Boolean
#
is_var_numeric <- function(x){
  if ((length(x)>0 || length(x)<0) & is.numeric(x)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

is_var_number <- function(x){
  sep_char_value <- split_character(x)
  char_n <- 1
  while(char_n <= length(sep_char_value)){
    char <- sep_char_value[char_n]
    if(char=="." || char=="-"){
      sep_char_value <- sep_char_value[-char_n]
      char_n <- char_n-1
    }
    
    if(is_char_a_number(char) == TRUE){
      sep_char_value <- sep_char_value[-char_n]
      char_n <- char_n-1
    }
    
    char_n <- char_n + 1
  }
  
  if(length(sep_char_value) == 0){
    return_x = TRUE
  }else{
    return_x = FALSE
  }
  
  return(return_x)
}

# DESC: Split characters in a string and seperate them into a vectore
# INPUT:  String
# OUTPUT: Returns a vector 
#
split_character <- function(value){
  value <- as.character(value)
  value <- strsplit(value,"")[[1]]
  return(value)
}




#===================================
#   FUNCTIONS with co-dependency


# FUNCTIONS:
#   DESC:  Finds a row that has a non-numeric value and list them 
#   INPUT: Selected column df
#   OUTPUT: Vector - list of row indes of non-numeric value
#
list_non_numeric_val_in_col <- function(sel_col){
  n <- 1
  list_nrow <- c()
  for (val in sel_col){
    
    is_numeric <- is_var_numeric(val)
    
    if(is_numeric == FALSE){
      list_nrow <- append(list_nrow, n)
    }
    n <- n+1
  }
  
  return(list_nrow) 
}

# DESC: Finds the column number that contain non value
#
list_non_number_val_in_col <- function(sel_col){
  n <- 1
  list_nrow <- c()
  for (val in sel_col){
    if(is_var_number(val) == FALSE){
      list_nrow <- append(list_nrow, n)
    }
    
    n <- n + 1
  }
  return(list_nrow) 
}

# FUNCTION
# DESC: Selects a date range
# INPUT: data = dataframe, col_date = column that contains date, start_date = start range, end_date = end range
# OUTPUT: dataframe: A subset of the dataset by range date
#
select_date_range <- function(data, col_date, start_date, end_date){
  
  cont <- TRUE
  
  message(
    cat(
      "\n------------------------------------------------------------
      \n Make sure the date's formt is: yyyy-mm-dd (ISO 8601)
      ...if not formatted properly, use as.Date() function 
      \n------------------------------------------------------------"
    )
  )
  
  # IF character, the heading of the column:
  disable_if <-- TRUE
  if(class(col_date)=="character" || disable_if == TRUE){
    
    if(check_date_iso(data[,col_date])){
      cont <- TRUE
    }else{
      cont <- FALSE
      message(
        cat(
          "\n\n ERROR: \n The dates are not format in yyyy-mm-dd \n"
        ),
        cat(" Cannot perform this function \n")
      )
    }
    
    this_col <- data[, col_date]
    this_data <- data[order(data[c(col_date)], decreasing = FALSE),]
    
    
    as.character(this_data[c(col_date)])
    
    
    this_data[,col_date] <- as.character(this_data[,col_date])
    # start date
    this_data <- subset(this_data, this_data[,col_date] >= start_date)
    
    #end date
    this_data <- subset(this_data, this_data[,col_date] <= end_date)
    
  }else if(class(col_date)=="factor"){
    message(
      cat("-------------------------------------------------------------------------------"),
      cat("\n   WARNING:"),
      cat("\n  Please input the name of column \"Date\" in instead of \"data$DATE\" "),
      cat("\n------------------------------------------------------------------------------")
    )
    
  }else{
    warning("Can't perform this action")
  }
  
}


