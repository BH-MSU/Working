brd <- c("Buick", "BYD", "Changan", "Chery", "Chevrolet", "Ford", 
         "Geely", "Great Wall", "Honda", "Hyundai", "Toyota", "Volkswagen")
mkt <- c("China", "Korea")
qtr <- "Quarter 2 2015"

#' Brand Quarterly Funnel
#' 
#' Calculate Familiarity (top 5 box / all respondents answered Fam), 
#'    FO (top 3 box / all respondents answered Opinion), Consideration (top 2 box), 
#'    Shop (shopped certain brand / total shoppers), Intention (intend / total respondents).
#' @param mkt_in Markets, form as "China" or "China, India, Australia, Thailand" with quotes and case-sensible. 
#' @param brd_in Brands, form as "Ford" or "Ford, Buick, BYD, Great Wall" with quotes and case-sensible.
#' @param qtr_in Quarters, form as "Quarter 1 2015" or "Quarter 1 2015, Quarter 2 2015, Quarter 3 2015" with quotes and case-sensible.
#' @import RMySQL dplyr
#' @export
BrdQtrlyFunnel <- function(mkt_in, brd_in, qtr_in){
  mkt <- gsub("^ *|(?<= ) | *$", "", trimws(strsplit(mkt_in, split = ",")[[1]]), perl = TRUE)
  brd <- gsub("^ *|(?<= ) | *$", "", trimws(strsplit(mkt_in, split = ",")[[1]]), perl = TRUE)
  qtr <- gsub("^ *|(?<= ) | *$", "", trimws(strsplit(mkt_in, split = ",")[[1]]), perl = TRUE)
  
  # connect to MySQL db using dplyr method
  con1 <- src_mysql(dbname = "beat", host = "10.52.96.184", port = 3306, 
                    user = "MSU_DB_Manager", password = "Embarassing...")
  message("| R is working on collecting data from MySQL database. \n| Please wait for a few minutes. \n")
  
  # collect data to local memory
  res0 <- tbl(con1, "apabrand") %>%
    # select the variables by code
    select(QTRREP, MARKET, BRAND, BAC1, BAC11, BAC2, BAC7, BAC4BRA, WEIGHT) %>%
    # BAC1: FAMILIARITY; BAC11: FO; BAC2: CONSIDERATION; BAC7: SHOPPED BRAND; BAC4BRA: INTENDED BRAND
    # set conditions
    filter(MARKET %in% c(mkt, NA), 
           BRAND %in% c(brd, NA), 
           QTRREP %in% c(qtr, NA)) %>%
    collect
  
  # calculation  
  familiar <- c("Familiar with", 
                "Have driven one within last 5 years", 
                "Owned/Leased one within the last 5 years", 
                "Have driven one, but over 5 years ago", 
                "Owned/Leased one, but over 5 years ago",
                "Have heard/read quite a bit, but never owned or drove one")
  fo <- c("08", "09", "10-Excellent")
  consider <- c("Probably Will Consider", "Definitely Will Consider")
  shop <- c("Dealer", "Internet", "Both Internet & Dealer")

  test <- res0 %>% 
    group_by(QTRREP, MARKET, BRAND) %>%
    summarise(FAMILIAR = sum(WEIGHT[which(BAC1 %in% familiar)])/sum(WEIGHT), 
              FO = sum(WEIGHT[which(BAC11 %in% fo)])/sum(WEIGHT[which(BAC11 != "<NA>")]), 
              CONSIDER = sum(WEIGHT[which(BAC2 %in% consider)])/sum(WEIGHT[which(BAC2 != "<NA>")]), 
              SHOP = sum(WEIGHT[which(BAC7 %in% shop)])/sum(WEIGHT[which(BAC7 != "<NA>")]), 
              INTEND = sum(WEIGHT[which(BAC4BRA == BRAND)])/sum(WEIGHT[which(BAC4BRA != "<NA>")]),
              NSample = n())
  
  # output csv file
  write.csv(res0, 
            paste("Raw ", gsub("-|:", "", as.character(Sys.time())), ".csv", sep = ""), 
            row.names = FALSE)
  write.csv(test, 
            paste("Funnel ", gsub("-|:", "", as.character(Sys.time())), ".csv", sep = ""), 
            row.names = FALSE)
  cat("\n")
  message('| The raw data is named as "Raw ', gsub("-|:", "", as.character(Sys.time())), 
          '.csv" under the directory "', getwd(), '". ')
  message('| The output is named as "Funnel ', gsub("-|:", "", as.character(Sys.time())), 
          '.csv" under the directory "', getwd(), '". ')
  message('| Please rename the file in case that you forget the file info. Bye! ')
}


