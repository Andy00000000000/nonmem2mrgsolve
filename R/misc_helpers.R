## Miscellaneous Helper Functions ####

## FUNCTION BLANK ROW ####

blank_df <- function(){
  return(data.frame(V1 = ""))
}

## FUNCTION MATCH PARENTHESES ####

par_match <- function(data = NULL, colnme = "V1", openChar = "\\(", closedChar = "\\)"){
  
  unlist(lapply(1:length(data[,colnme]), function(i){ # locate chr index of open and closed parentheses
    
    open <- stringr::str_locate_all(data[i,colnme], openChar)[[1]][,1] # start and end are identical since ( is 1 chr
    closed <- stringr::str_locate_all(data[i,colnme], closedChar)[[1]][,1]
    
    all <- data.frame(ALL = c(open,closed), OPEN = c(rep(1,length(open)), rep(0, length(closed))))%>% # combine open and closed parentheses and order
      dplyr::arrange(ALL)
    
    count <- 1
    iter <- 2
    
    while(iter <= length(all$ALL) & count != 0){ # find closing parenthesis of first open, since first open should always be if statement
      if(all[iter,"OPEN"] == 1){
        count <- count+1
      }else{
        count <- count-1
      }
      if(count == 0){
        match_close <- all[iter,"ALL"]
      }
      iter <- iter+1
    }
    
    return(match_close)
  }))
}

## END ####
