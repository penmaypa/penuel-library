message(
  cat("\n  ===============================================================\n\n"),
  cat("\t       penmaypa: my_tools.r "),
  cat("\n\t   -------------------------------------"),
  cat("\n   A collection of custom tools created by Penuel Maypa (March 2020)"),
  cat("\n\n   Last Update: 03-Apr-2020 04:25"),
  cat("\n\n  ===============================================================\n")
)



id_correction <- function(dataframe , col_name_id, current_id, expected_id ){
  #DES: Corrects ID by comparing the wrong id to the original id
  
  dataframe <- dataframe
  
  col_name_id <- col_name_id
  
  # int
  current_id <- current_id
  
  # int
  expected_id <- expected_id
  
  #====================
  # PROCESS
  
  source("https://github.com/penmaypa/penuel-library/raw/master/R/our_functions.r")
  
  cont <- TRUE
  
  if( is.numeric(current_id) == FALSE || is.numeric(expected_id) == FALSE){
    
    current_id <- as.numeric(current_id)
    
    expected_id <- as.numeric(expected_id)
    
    if(is.integer(current_id) == FALSE || is.integer(expected_id) == FALSE){
      
      message("\n  ============================================================\n")
      message("\t Please make sure that the id is a numeric and an integer")
      message("\n  =============================================================n")
      
      cont=FALSE
      
    }
    
  }
  
  if(cont==TRUE){
    
    # FIND DIFFERENCE BETWEEN current and expcted
    if(expected_id > current_id){
      
      diffx <- expected_id - current_id
      
      nxa <- 1
      while(nxa <= nrow(dataframe) && cont==TRUE){
        
        cor_id <-as.integer( dataframe[nxa,col_name_id]) + diffx
        
        dataframe[nxa,col_name_id] <- cor_id
        nxa <- nxa + 1
      }
      
    }else if(expected_id < current_id){
      
      diffx <- current_id - expected_id
      
      nxa <- 1
      while(nxa <= nrow(dataframe) && cont==TRUE){
        
        cor_id <- as.integer(dataframe[nxa,col_name_id]) - diffx
        
        dataframe[nxa,col_name_id] <- cor_id
        nxa <- nxa + 1
      }
      
    }else{
      message("There are no difference between current_id and expected_id")
    }
    
  }
  
  returnx <- dataframe
  
  return(returnx)
}

