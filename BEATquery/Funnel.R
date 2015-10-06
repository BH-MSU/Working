brd <- c("Buick", "BYD", "Changan", "Chery", "Chevrolet", "Ford", 
         "Geely", "Great Wall", "Honda", "Hyundai", "Toyota", "Volkswagen")
mkt <- c("China", "Korea")
qtr <- "Quarter 2 2015"

#' Input Function
#' 
#' User friendly system for inputing parameters of other functions
#' @param vname Character, such as "market", "brand", "quarter"...
#' @return Character vector, specified values of the "vname". 
Input <- function(vname){
  repeat{
    value_in <- readline(paste('| Please type in the ', vname, 
                               '(s)(use comma "," to seperate if more than 2): \n', 
                               sep = ""))
    
    if(all(strsplit(value_in, split = "")[[1]] %in% c(LETTERS, letters, ",", "'", " ", as.character(0:9)))){
      value <- gsub("^ *|(?<= ) | *$", "", 
                  trimws(strsplit(value_in, split = ",")[[1]]), 
                  perl = TRUE)
      if(length(value) == 1) message("| The ", vname, " is: ") else message("| The ", vname, "s are: ")
      cat("| ", paste(value, collapse = ", "), sep = "")
      value_cfm <- readline(paste("| Could you confirm the ", vname, "(s)(Y/N)? ", sep = ""))
      if(toupper(value_cfm) == "Y"){
        return(value)
        break
      }
    } else {
      message("| Maybe you input illegal characters. Only letters, spaces and commas are acceptable.\n")
    }
  }
} # end of function Input

#' Brand Quarterly Funnel
#' 
#' Calculate Familiarity (top 5 box / all respondents answered Fam), FO (top 3 box / all respondents answered Opinion), Consideration (top 2 box), Shop (shopped certain brand / total shoppers), Intention (intend / total respondents).
#' @import RMySQL dplyr
#' @export
BrdQtrlyFO <- function(){
  # set variables range
  # all_brd <- c()
  # all_mkt <- c("")
  
  # input parameters
  message("| Welcome to use MSU BEAT query tool! \n")
  message("| The system is case sensitive! \n")
  message('| If 2 or more values you need to input, use comma "," to seperate them. \n')
  message("| This process is to calculate FO by quarter by market by brand. ")
  cat("\n")
  # 1. Market - mkt
  cat("\n", paste(rep("-", 20), collapse = ""), sep = "| ")
  cat("| Part. I  Market(s)")
  message("| What market(s) are you interested in? ")
  message('| Make sure the names typed in begin with a capital letter, like "China" or "New Zealand". \n')
  mkt <- Input("market")
  # 2. Brand - brd
  cat("\n", paste(rep("-", 20), collapse = ""), sep = "| ")
  cat("| Part. II  Brand(s)")
  message("| What brand(s) are you interested in? ")
  message('| Make sure the names typed in as they usually be, like "Ford", "BYD" or "Great Wall". \n')
  brd <- Input("brand")
  # 3. Quarter - qtr
  cat("\n", paste(rep("-", 20), collapse = ""), sep = "| ")
  cat("| Part. III  Period")
  message("| Which quarter(s) are you interested in? ")
  message('| Make sure the form is like "Quarter 1 2015". \n')
  qtr <- Input("quarter")
  
  # connect to MySQL db using dplyr method
  con1 <- src_mysql(dbname = "beat", host = "10.52.96.184", port = 3306, 
                    user = "MSU_DB_Manager", password = "Embarassing...")
  
  # collect data to local memory
  res0 <- tbl(con1, "apabrand") %>%
    # select the variables by code
    select(QTRREP, MARKET, BRAND, BAC11, WEIGHT) %>%
    # set conditions
    filter(MARKET %in% c(mkt, NA), 
           BRAND %in% c(brd, NA), 
           QTRREP %in% c(qtr, NA), 
           BAC11 != "<NA>") %>%
    collect
  print(res0)
  
  # calculation
  test <- res0 %>% 
    group_by(QTRREP, MARKET, BRAND) %>%
    summarise(FO = sum(WEIGHT[which(BAC11 %in% c("08", "09", "10-Excellent"))])/
                sum(WEIGHT), 
              SampleSize = n())
  print(test)
  
  # output csv file
  write.csv(test, 
            paste("Output ", as.character(Sys.time()), ".csv", sep = ""), 
            row.names = FALSE)
  message('| The output is named as "Output ', as.character(Sys.time()), 
          '.csv" under the directory "', getwd(), '". ')
  message('| Please rename the file in case that you forget the file info. Bye! ')
  cat("\n", paste(rep("-", 20), collapse = ""), sep = "| ")
}


