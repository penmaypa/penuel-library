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
      \n Our_Functions.r -- created by Penuel Maypa | Last Update: 2 April, 2020 02:32
     \nThis R file contains a collection of customized functions
      \n ========================================================= \n"  
  )
)

source("https://github.com/penmaypa/penuel-library/raw/master/R/Tools/check_package.r")

#==========================
#  PACKAGES & LIBRARIES
#--------------------------


check_package( c("crayon") )
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


# FUNCTION: Check if there was error in the object/variable during assigning
# INPUT: Objects/Vaiable
# OUTPUT: Boolean -- TRUE; if error during assigning , FALSE; if the assiging was perform without error.
#
is_error <- function(var_obj){
  
  notice <- FALSE
  if(notice==TRUE){
    message("\n----------------------------------------------------")
    message("\n REMINDER: Make sure you assigned the object \n\t   on \"try()\" function, ",
            "\n\t   before performing \"is_error()\" function \n")
    message("------------------------------------------------------ \n")
  }
  
  
  if("try-error" %in% class(var_obj)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

# DESC: Converts decimal into percent
#
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
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


# Checks for existing column-name in the dataframe
check_column_name <- function(dataframe, column_name){
  
  cont <- TRUE
  
  if(class(dataframe) == "data.frame"){
    cont <- TRUE
  }else{
    cont <- FALSE
    
    message("\n  ------------------------------------\n ")
    message("    ERROR: ")
    message("      The input of dataframe parameter is not a dataframe")
    message("\n  ------------------------------------\n ")
  }
  
  
  if(is.character(column_name)==TRUE && cont == TRUE){
    
    namex <- names(dataframe)
    xa <- 1
    for(item in namex){
      
      if(as.character(item) == column_name){
        returnx <- TRUE
        break
      }
      
      xa <- xa+1
    }
    
    if(xa >= length(namex) ){
      
      returnx <- FALSE
      
      message("\n  ------------------------------------\n ")
      message("    ERROR: ")
      message("      The column-name \"", column_name,"\" does not exist.")
      message("      please make sure the spelling and case are correct.")
      message("\n  ------------------------------------\n ")
      
    }
    
  }else{
    
    returnx <- FALSE
    
    message("\n  ------------------------------------\n ")
    message("    ERROR: ")
    message("      The column-name is not a string (character)")
    message("\n  ------------------------------------\n ")
  }
  
  return(returnx)
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
      cat(yellow("\n\t Please make sure that the value is a character")),
      cat(yellow("\n\t Use as.charcacter() function")),
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

#\\\-----------------------
  #\\\

    # DESC: Merge two dataframe
    # INPUT:
    # OUTPUT: Data Frame -- a merged dataframe 
    merge_dataframe <- function(df_1 , df_2){
      
      # Checks whether the column name are similar
      if(
        class(df_1)=="data.frame"
        &&
        class(df_2)=="data.frame"
      ){
        
        df1_names <- names(df_1)
        df2_names <- names(df_2)
        df_names_diff <- c
        
        is_names_similar <- TRUE
        
        n1 <- 1
        for(name in df1_names){
          
          if(name != df2_names[n1]){
       
            for(sel_name in df_names_diff){
              
              # If not, then add to vector
              if(sel_name != name){
                df_names_diff <- append(df_names_diff, name)
              }else if(sel_name != df2_names[n1]){
                df_names_diff <- append(df_names_diff, df2_names[n1])
              }
            }
            
          }
          
          n1 <- n1 + 1
        }
      
        if(length(df_names_diff) == 0){
          df_master <- rbind(df_1, df_2)
          
        }else{
          
          for(name_x2 in df_names_diff){
            df_master[name_x2] <- ""
          }
          
          df_master <- rbind(df_1, df_2)
      
        }
      
      }else{
        message(
          cat("\n----------------------------------------------------\n"),
          cat("  ATTENTION: "),
          cat("\n  Please make sure the arguments are data frame"),
          cat("\n-------------------------------------------------------\n")
        )
      }
      
      return(return_x)
    }

  #///
#///------------------------



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


# PROGRESS VECTOR & MESSAGE
#
progress_vector <- function(counter_x, total_x){
  
  prog_x <- counter_x / total_x
  perx <- percent(prog_x)
  prog_v <- c(counter_x, total_x, perx)
  
  return(prog_v)
}

progress_message <- function(counter_x, total_x){
  
  prog_x <- counter_x / total_x
  perx <- percent(prog_x)
  prog_v <- c(counter_x, total_x, perx)
  
  returnx <- message(" ",counter_x, " of ", total_x, "\t",perx )
  
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
    
    this_col <- data[col_date]
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

duplicate_dataframe_column <- function(dataframe){
  
  #==================
  #  PARAMETERS
  df <- dataframe
  #================
  
  namev <- names(df)
  
  dfb <- data.frame()
  nxa <- 1
  for(namex in namev){
    
    dfb[nxa] <- data.frame(x="")
    colnames(dfb)[nxa] <- namex 
    
    nxa <- nxa +1 
  }
  
  
  returnx <- dfb
  
  return(returnx)
}


delete_column <- function(df,col_names){
  DF <- df
  drops <- c(col_names)
  dfx <- DF[ , !(names(DF) %in% drops)]
  
  return(dfx)
}


create_dataframe <- function (columns){
  #===============
  # INPUT:
  #   vector 
  columns <- columns 
  
  #================
  
  dfx1 <- as.data.frame()
  
  for(colx in columns){
    dfx[colx] <- ""
  }
  
  returnx <- dfx
  
  return(returnx)
}



dataframe_add_column <- function(df, columns){
  
  df <- df
  columns <- columns
  
  for(colx in columns){
    df[colx] <- ""
  }
  
  returnx <- df
  return(returnx)
}

add_columns <- function(df, columns){
  dataframe_add_column(df, columns)
}


match_key_value <- function(df_x, colname_id_x,colname_empt_value_x,df_y,colname_id_y,colname_value_y){
  
  # INPUT:
  df_x <- df_x
  colname_id_x <- colname_id_x
  colname_empt_value_x <- colname_empt_value_x
  
  df_y <- df_y
  colname_id_y <- colname_id_y
  colname_value_y <- colname_value_y
  
  cont <- TRUE
  nx1 <- 1
  while(nx1 <= nrow(df_x) && cont == TRUE){
    
    id_x <- df_x[nx1, colname_id_x]
    value_dfx <- find_row_where_value(df_y, colname_id_y, as.character(id_x))
    
    if(nrow(value_dfx) != 1){
      
      message("\n  ---------------------------- \n")
      message("\n    Error:  ")
      message("\n      Value may had not found...")
      message("\n      ... OR there are more than 1 value")
      message("\n  ---------------------------- \n")
      
      stop(paste("\n The function stopped at: \n", progress_message(nx1,nrow(df_x)), sep=""),call. = TRUE, domain = NULL)
      # cont <- FALSE
      # message("\n The programe has stopped \n")
      
    }else{
      value_x <- value_dfx[1,colname_value_y]
      df_x[nx1, colname_empt_value_x] <- as.character(value_x)
    }
    
    progress_message(nx1,nrow(df_x))
    nx1 <- nx1 + 1
  }
  
  returnx <- df_x
  
  return(returnx)
}



print_vector <- function(x){
  
  nx <- 1
  for(item in x){
    print(paste(nx,": ",item," ",sep=""))
    
    nx <- nx +1
  }
}


rename_column <- function(df, sel_column, new_name){
  colnames(df)[names(df)==sel_column] <- as.character(new_name)
  return(df)
}

install_packages_if_not_installed <- function(vector_package){
  
  # DESC:  Installs packages if such package is not installed
  # INPUT: Vector , one or more package i.e, c("package_1", "package_2")
  
  for(packx in vector_package){
    list.of.packages <- packx
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    
    return(
      if(length(new.packages)){
        message(paste("\n  Installing package: ",packx,"\n",sep=","))
        install.packages(new.packages)
      }else{
        message(paste("\n  The package already installed: ",packx,sep=","))
      } 
    )
  }
 
}

decimal_round_to <- function(x,n){
  return_x <- format(round(x, 2), nsmall = n)
  return(return_x)
}
