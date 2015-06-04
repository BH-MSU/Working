#########################
## STEP 0 PREPARATIONS ##
#########################

# STEP 0.2 Clear the Global Environment
# rm(list = ls())
# STEP 0.3 create an environment for ddi useful results
e <- new.env(globalenv())

# STEP 0.1 add & load required packages
e$all.pcg <- c("rJava", "xlsxjars", "xts", "xlsx", "zoo", "plyr", "tidyr", "dplyr",
             "lubridate", "ggplot2", "reshape", "lmtest", "car", "MASS", "Rcpp", "colorspace")
req.pkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
req.pkg(e$all.pcg)

# Function dplyr::select is masked by MASS
# if(suppressMessages(require(MASS)))detach(package:MASS)

DDI <- function(){
  
  # STEP 0.4 save original wd
  if("wd_recover" %in% ls(e)) setwd(e$wd_recover) else e$wd_recover <- getwd()
  
  # STEP 0.5 set working directory
  if(!file.exists("DDI_OUTPUT"))dir.create("DDI_OUTPUT")
  setwd(paste(e$wd_recover, "DDI_OUTPUT", sep = "/"))

  ######################
  ## STEP 1 LOAD DATA ##
  ######################
  
  message("\n| Welcome to use MSU DDI Tool!")
  message("| Please keep the Internet connected and follow the instructions! ")
  message("| The working directory is set as DDI_OUTPUT file under your origianl WD. ")
  message("| The input files should be moved to DDI_OUTPUT file in advance. \n")
  
  repeat{
    oldmodel <- readline("| Do you want to start with an existed model (Y/N)? ")
    cat("\n")
    if(!toupper(oldmodel) %in% c("Y", "N")){
      message('| Only "Y" or "N" is acceptable! \n')
    } else {
      oldmodel <- toupper(oldmodel)
      break
    }
  }
  
  if(oldmodel == "N"){
    
    repeat{
      data.name <- readline("| Please enter the name of raw data file: ")
      endstr <- substr(data.name, nchar(data.name)-3, nchar(data.name))
      cat("\n")
      if (!endstr %in% c(".csv", "xlsx", ".xls")){
        message('| Only "*.csv", "*.xlsx" or "*.xls" file is expected!\n')
      }else if(!file.exists(data.name)){
        message('| "', data.name, '" does not exist in your work directory!')
        message('| Please re-check it in the file explorer!\n')
        cat(paste(rep("-", getOption("width")-2), collapse = ""))
        cat("\n")
      }else if(endstr == ".csv"){
        e$data <- read.csv(data.name, stringsAsFactors = F)
        break
      }else{
        sht.names <- names(getSheets(loadWorkbook(data.name)))
        repeat{
          sht.name <- readline("| Please enter the name of worksheet: ")
          cat("\n")
          if (!sht.name %in% sht.names){
            message('| The worksheet "', sht.name, '" is not found!\n')
          }else{
            break
          }
        }
        e$data <- read.xlsx(data.name, sheetName = sht.name, stringsAsFactors = F)
        break
      }
    }
    message('| "', data.name, '" is loaded.') 
    message('| There are ', dim(e$data)[1], ' observations and ', 
            dim(data)[2], ' variables in the raw dataset.\n')
    
    
    ######################################
    ## STEP 2 DATA CREATION & TRANSFORM ##
    ######################################
    
    # STEP 2.1 choose the response variable
    repeat{
      e$resp <- readline("| Please enter the name of response variable: ")
      cat("\n")
      if (e$resp %in% names(e$data)){
        break
      }else{
        message('| The variable "', e$resp, '" does not exist in the database!\n')
      }    
    }
    
    # STEP 2.2 choose the time-line variable and its format
    repeat{
      e$tvar <- readline("| Please enter the name of time variable: ")
      cat("\n")
      if (e$tvar %in% names(e$data)){
        break
      }else{
        message('| The variable "', e$tvar, '" does not exist in the database!\n')
      }    
    }
    
    repeat{
      cat("| Which kind of date format is it in the raw data? \n")
      cat("|   1. m/d/y\n|   2. d/m/y\n|   3. y/m/d\n")
      t_f <- readline("\n| Please choose a format number: ")
      cat("\n")
      if(!t_f %in% c("1","2","3")){
        message("| Only a number between 1 and 3 is acceptable!\n")
      }else{
        t_f <- as.numeric(t_f)
        break
      }
    }
    
    # STEP 2.3 create new variables about time series. i.e. T_MON, T_TUE, etc.
    e$df <- e$data                                        %>%
      day.create(e$tvar, t_f)                               %>%
      plot.order(e$resp, e$tvar)
    # write.csv(df, "df.csv")
    
    # STEP 2.4 grab the cut-off date for modeling
    repeat{
      cod <- readline("| Please enter the cut-off date for modeling (mm/dd/yyyy): ")
      cat("\n")
      if(!grepl("^(0[1-9]|1[0-2])[-|/|.](0[1-9]|[12][0-9]|3[01])[-|/|.]20[0-9][0-9]$", cod)){
        message("| Please strictly follow the data input format: mm/dd/yyyy !\n")
      }else{
        e$cod <- mdy(cod)
        na.pos <- min(which(is.na(e$df[[e$resp]])))
        if (e$df[[e$tvar]][na.pos] <= e$cod){
          message("| The response variable contains NA for modeling!\n")
        }else if(!e$cod %in% e$df[[e$tvar]]){
          message("| The input date doesn't exist in the raw data!\n")
        }else{
          e$cod.pos <- which(e$df[[e$tvar]] == e$cod)
          break
        }
      }
    } # standard cut-off date
    
    
    # STEP 2.5 create new variables of RECOMMENDED time lag
    df.for.lag <- dplyr::select(e$df, -starts_with("T_"), -starts_with("H_"))
    e$df.h.t <- dplyr::select(e$df, starts_with(e$tvar), starts_with(e$resp), starts_with("H_"), starts_with("T_"))
    
    e$df.lag <- lag.trans(df.for.lag, e$resp, e$tvar, e$cod, lag.len.v = 0:70, top.no = 1)
    e$df.rec <- data.frame(e$df.h.t, e$df.lag)
    e$df.model <- data.frame(e$df, e$df.lag[, !names(e$df.lag) %in% names(e$df)])
    #e$x_axis <- e$df[[e$tvar]][1:e$cod.pos]
    
    ##########################################
    ## STEP 3 AUTOMATIC REGRESSION MODELING ##
    ##########################################  
    
    # STEP 3.1 lm and stepAIC
    # suppressMessages(require(MASS))
    
    fit0 <- lm(as.formula(sprintf('%s ~ .', e$resp)), data = e$df.rec[1:e$cod.pos, -1], na.action = na.exclude)
    
    e$fit.aic <- stepAIC(fit0, trace = F)
    e$fit.loop.temp <- e$fit.aic
    
    loop.output(e$resp, data0 = e$df.model, pos = e$cod.pos, fit = e$fit.aic)
    warn(fit1 = NULL, fit2 = e$fit.aic, p.cons = 0.2)
    
    # STEP 3.2 output or not
    repeat{
      out.aic <- readline("| Do you want to output this model's results (Y/N)? ")
      cat("\n")
      if (!toupper(out.aic) %in% c("Y","N")){
        message('| Only "Y" or "N" is acceptable!\n')
      }else{
        
        if(toupper(out.aic) == "Y"){
          model.xls.output(e$df.model, pos = e$cod.pos, e$resp, fit = e$fit.aic, aic.opt = "_AIC")
          break
        } else {
          break
        }
      }
    }
    
  } else if(oldmodel == "Y"){
    repeat{
      message("| You're expected to input the FULL DATA file and COEFFICIENTS file. ")
      message("| Make sure the full data file and coefficients file are RELATED! ")
      message("| Make sure the structure inside each worksheet or csv file is the same as DDI TOOL OUTPUTS! \n")
      
      cat("| What kind of file are you going to input? ",
          '|   1. One "*.xlsx" file; ',
          '|   2. Two "*.csv" files. ', "", sep = "\n")
      in.opt1 <- readline("| Please enter your choice: ")
      cat("\n")
      if(!as.numeric(in.opt1) %in% c(1,2)){
        message('| Only number "1" or "2" is acceptable! \n')
      } else if(as.numeric(in.opt1) == 1) {
        repeat{
          in.file1 <- readline("| Please enter the name of model result file: ")
          endstr <- substr(in.file1, nchar(in.file1)-3, nchar(in.file1))
          cat("\n")
          if (!endstr %in% c("xlsx", ".xls")){
            message('| Only "*.xlsx" or "*.xls" file is expected!\n')
          }else if(!file.exists(in.file1)){
            message('| "', in.file1, '" does not exist in your work directory!')
            message('| Please re-check it in the file explorer!\n')
            cat("\n")
          }else {
            sht.names <- names(getSheets(loadWorkbook(in.file1)))
            break
          }
        }
        # input model data
        repeat{
          message('| If you did not change the worksheet name, try "Full_Data" please. ')
          sht.name1 <- readline("| Please enter the name of FULL DATA worksheet: ")
          cat("\n")
          if (!sht.name1 %in% sht.names){
            message('| The worksheet "', sht.name1, '" is not found!\n')
          }else break
        }
        e$df.model <- read.xlsx(in.file1, sheetName = sht.name1, stringsAsFactors = F)
        
        # input coefficients data frame
        repeat{
          message('| If you did not change the worksheet name, try "Model_Coef" please. ')
          sht.name2 <- readline("| Please enter the name of COEFFICIENTS worksheet: ")
          cat("\n")
          if (!sht.name2 %in% sht.names){
            message('| The worksheet "', sht.name2, '" is not found!\n')
          }else break
        }
        e$oldmodel.coef <- read.xlsx(in.file1, sheetName = sht.name2, stringsAsFactors = F)
        break
        
      } else if(as.numeric(in.opt1) == 2) {
        # input model data
        repeat{
          in.file2 <- readline("| Please enter the name of FULL DATA file: ")
          cat("\n")
          endstr <- substr(in.file2, nchar(in.file2)-3, nchar(in.file2))
          if (!endstr == ".csv"){
            message('| Only "*.csv" file is expected!\n')
          }else if(!file.exists(in.file2)){
            message('| "', in.file2, '" does not exist in your work directory!')
            message('| Please re-check it in the file explorer!\n')
            cat("\n")
          }else break
        }
        e$df.model <- read.csv(in.file2, stringsAsFactors = F)
        #break
        
        # input coefficients data frame
        repeat{
          in.file3 <- readline("| Please enter the name of COEFFICIENTS file: ")
          cat("\n")
          endstr <- substr(in.file3, nchar(in.file3)-3, nchar(in.file3))
          if (!endstr == ".csv"){
            message('| Only "*.csv" file is expected!\n')
          }else if(!file.exists(in.file3)){
            message('| "', in.file3, '" does not exist in your work directory!')
            message('| Please re-check it in the file explorer!\n')
            cat("\n")
          }else break
        }
        e$oldmodel.coef <- read.csv(in.file3, stringsAsFactors = F)
        break
        
      }
    }
    message('| There are ', nrow(e$df.model), ' observations and ', 
            ncol(e$df.model), ' variables in the raw dataset. ')
    message('| There are ', nrow(e$oldmodel.coef)-1, ' variables in the existed model. \n')
    
    # STEP 2.1 choose the response variable
    repeat{
      e$resp <- readline("| Please enter the name of response variable: ")
      cat("\n")
      if (e$resp %in% names(e$df.model)){
        break
      }else{
        message('| The variable "', e$resp, '" does not exist in the database!\n')
      }    
    }
    
    # STEP 2.2 choose the time-line variable and its format
    repeat{
      e$tvar <- readline("| Please enter the name of time variable: ")
      cat("\n")
      if (e$tvar %in% names(e$df.model)){
        break
      }else{
        message('| The variable "', e$tvar, '" does not exist in the database!\n')
      }    
    }
    
    repeat{
      cat("| Which kind of date format is it in the raw data? \n")
      cat("|   1. m/d/y\n|   2. d/m/y\n|   3. y/m/d\n")
      t_f <- readline("\n| Please choose a format number: ")
      cat("\n")
      if(!t_f %in% c("1","2","3")){
        message("| Only a number between 1 and 3 is acceptable!\n")
      }else{
        t_f <- as.numeric(t_f)
        break
      }
    }
    
    if(class(e$df.model[[e$tvar]]) == "Date"){
      e$df.model[[e$tvar]] <- ymd(e$df.model[[e$tvar]])
    } else if(t_f == 1){
      e$df.model[[e$tvar]] <- mdy(e$df.model[[e$tvar]])
    } else if(t_f == 2){
      e$df.model[[e$tvar]] <- dmy(e$df.model[[e$tvar]])
    } else {
      e$df.model[[e$tvar]] <- ymd(e$df.model[[e$tvar]])
    }
    
    
    # STEP 2.3 grab the cut-off date for modeling
    repeat{
      cod <- readline("| Please enter the cut-off date for modeling (mm/dd/yyyy): ")
      cat("\n")
      if(!grepl("^(0[1-9]|1[0-2])[-|/|.](0[1-9]|[12][0-9]|3[01])[-|/|.]20[0-9][0-9]$", cod)){
        message("| Please strictly follow the data input format: mm/dd/yyyy !\n")
      }else{
        e$cod <- mdy(cod)
        na.pos <- min(which(is.na(e$df.model[[e$resp]])))
        if (e$df.model[[e$tvar]][na.pos] <= e$cod){
          message("| The response variable contains NA for modeling!\n")
        }else if(!e$cod %in% e$df.model[[e$tvar]]){
          message("| The input date doesn't exist in the raw data!\n")
        }else{
          e$cod.pos <- which(e$df.model[[e$tvar]] == e$cod)
          break
        }
      }
    } # standard cut-off date
    
    #e$x_axis <- e$df.model[[e$tvar]][1:e$cod.pos]
    df.old <- data.frame(e$df.model[[e$tvar]], e$df.model[[e$resp]], 
                         e$df.model[, match(e$oldmodel.coef[[1]][-1], names(e$df.model))])
    names(df.old)[1:2] <- c(e$tvar, e$resp)
    rownames(e$df.model) <- as.character(e$df.model[[e$tvar]])
    
    e$fit.loop.temp <- lm(as.formula(sprintf('%s ~ .', e$resp)), data = df.old[1:e$cod.pos, -1], na.action = na.exclude)
    loop.output(e$resp, data = df.old, pos = e$cod.pos, fit = e$fit.loop.temp)
    warn(fit1 = NULL, fit2 = e$fit.loop.temp, p.cons = 0.2)

  }
  
  
  ############################################
  ## STEP 5 MANUAL LOOP REGRESSION MODELING ##
  ############################################
  
  message("| Following modeling is based on the model above.\n")
  e$fit.loop <- e$fit.loop.temp
  repeat{
    cat("| What are you going to do now? ",
        "|   1. Transform variable; ", 
        "|   2. Directly do modeling using current database; ", 
        "|   3. Remove a variable from current model; ", 
        "|   4. Stop modeling and output the result. ", "", sep = "\n")
    
    mm.opt <- readline("| Please type in the number you choose: ")
    cat("\n")
    if(!mm.opt %in% as.character(1:4)){
      message("| Only the number between 1 and 4 is acceptable!")
      cat("\n")
    }else{
      mm.opt <- as.numeric(mm.opt)
      
      if(mm.opt == 1){
        # 1. Transform variable; 
        
        # INPUT: 
        #   1. var: variable to be transformed
        #   2. df.model: database to select variable from
        
        # OUTPUT:
        #   1. opt1(type): transformation method
        #   2. obj.trans: values to be calculated, Default is NULL. When type = certain index, object to be provided
        repeat{
          cat("| Which kind of transformation would you like to apply?",
              "|   0. monthly days remaining",
              "|   1. addition",
              "|   2. substraction",
              "|   3. multiplication",
              "|   4. division",
              "|   5. logarithm",
              "|   6. root",
              "|   7. exponent",
              "|   8. reciprocal",
              "|   9. time lag", "", sep="\n")
          opt1 <- readline("| Please enter an option number: ")
          cat("\n")
          
          if (!opt1 %in% as.character(0:9)){
            message("| Please only enter a number between 0 and 9!\n")
          } else if(opt1 == 0){
            e$df.model <- specify.remain(e$df.model)
            break
          } else {
            opt1 <- as.numeric(opt1)
            repeat{
              var <- readline("| Please enter the name of variable you want to transform: ")
              cat("\n")
              if (!var %in% names(e$df.model)){
                message('| The variable "', var, '" does not exist in the database!\n')
              } else {
                midname <- c("_A_","_M_","_T_","_D_","_LOG","_ROOT_","_EXP_","_RCPR","_LAG_")[opt1]
                if(opt1 == 5) {
                  # TRANSFORM 5: LOGARITHM
                  if(all(e$df.model[[var]] > 0)) {
                    obj.trans <- NULL
                    break
                  } else {
                    message('| The variable "', var, '" has NON-POSITIVE values!\n')
                  }
                  
                } else if(opt1 == 8) {
                  # TRANSFORM 8: RECIPROCAL
                  if(all(e$df.model[[var]] != 0)) {
                    obj.trans <- NULL
                    break
                  } else {
                    message('| The variable "', var, '" contains ZEROs!\n')
                  }
                  
                } else if(opt1 %in% 1:3) {
                  # TRANSFORM 1-3: ADDITION / SUBTRACTION / MULTIPLICATION
                  repeat{
                    cat("| What's the transformation object?",
                        "|   1. a number(zero is NOT allowed)",
                        paste("|   2. min/max/mean of ", var, sep = ""),
                        "|   3. another variable", "", sep = "\n")
                    obj1 <- readline("| Please enter an option number: ")
                    cat("\n")
                    if(!obj1 %in% as.character(1:3)) {
                      message("Man (or u r a woman/girl...)! Only 3 options!\n")
                    } else {
                      obj1 <- as.numeric(obj1)
                      break
                    }
                  }
                  
                  if(obj1 == 1) {
                    # 1: add/minus/times a number
                    repeat{
                      obj2 <- readline("| Please enter a NON-ZERO number: ")
                      cat("\n")
                      if(all(strsplit(obj2, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                        if(as.numeric(obj2) == 0){
                          message("| Zero is NOT allowed!\n")
                        }else {
                          obj.trans <- as.numeric(obj2)
                          e$df.model <- data.frame(e$df.model, bt(e$df.model[[var]], opt1, obj.trans))
                          names(e$df.model)[ncol(e$df.model)] <- paste(var, midname, sep = "")
                          message('| A new variable "', paste(var, midname, sep = ""), '" is created!\n')
                          break
                        }  
                      } else 
                        message("| Please do enter a number!\n") 
                    }
                    break
                    
                  } else if(obj1 == 2) {
                    # 2. add/minus/times the max/min/mean value
                    repeat{
                      obj2 <- tolower(readline("| Please choose among max, min & mean): "))
                      cat("\n")
                      if(!obj2 %in% c("max", "min", "mean")){
                        message("| Please only choose one of the three!\n")
                      } else {
                        if(obj2 == "min") { 
                          obj.trans <- min(e$df.model[[var]])
                        } else if(obj2 == "max") { 
                          obj.trans <- max(e$df.model[[var]])
                        } else if(obj2 == "mean") {
                          obj.trans <- mean(e$df.model[[var]])
                        }
                        break
                      }
                    }
                    break
                    
                  } else if(obj1 == 3) {
                    # 3. add/minus/times another variable
                    repeat{
                      obj2 <- readline("| Please enter the variable's name: ")
                      cat("\n")
                      if(!obj2 %in% colnames(e$df.model)){
                        message('| The variable "', obj2, '" does not exist in the database!\n')
                      } else {
                        obj.trans <- e$df.model[[obj2]]
                        break
                      }
                    }
                    break
                  }
                  
                } else if(opt1 == 4) {
                  # TRANSFORM 4 - DIVISION
                  repeat{
                    cat("| What's the transformation object?",
                        "|   1. a number(zero is NOT allowed)",
                        paste("|   2. min/max/mean of ", var),
                        "|   3. another variable", "", sep = "\n")
                    obj1 <- readline("Please enter an option number: ")
                    cat("\n")
                    if(!obj1 %in% as.character(1:3)){
                      message("| Man (or u r a woman/girl...)! Only 3 options!\n")
                    } else {
                      obj1 <- as.numeric(obj1)
                      break
                    }
                  }
                  
                  if(obj1 == 1) {
                    # divide by a number
                    repeat{
                      obj2 <- readline("| Please enter a NON-ZERO number: ")
                      cat("\n")
                      if(all(strsplit(obj2, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                        if(as.numeric(obj2) == 0){
                          message("Zero is NOT allowed!\n")
                        } else {
                          obj.trans <- as.numeric(obj2)
                          break
                        }
                      } else {
                        message("| Please do enter a number!\n")
                      }
                    }
                    break
                    
                  } else if(obj1 == 2) {
                    # divided by its max, min or mean
                    repeat{
                      obj2 <- tolower(readline("Please choose one among max, min and mean: "))
                      cat("\n")
                      if(!obj2 %in% c("max", "min", "mean")){
                        message("| Please only choose one of the three!\n")
                      } else {
                        if(obj2 == "min") { 
                          obj3 <- min(e$df.model[[var]])
                        } else if(obj2 == "max") { 
                          obj3 <- max(e$df.model[[var]])
                        } else if(obj2 == "mean") {
                          obj3 <- mean(e$df.model[[var]])
                        }
                        if(obj3 != 0){
                          obj.trans <- obj3
                          break   
                        } else {
                          message("| The ", obj2, " of ", var, " is ZERO!\n")
                        }
                      }
                    }
                    break
                    
                  } else if(obj1 == 3) {
                    # 3. divided by another variable
                    repeat{
                      obj2 <- readline("| Please enter the variable's name: ")
                      cat("\n")
                      if(!obj2 %in% colnames(e$df.model)){
                        message('| The variable "', obj2, '" does not exist in the database!\n')
                      } else {
                        if(any(e$df.model[[obj2]] == 0)){
                          message('| The variable "', obj2, '" contains ZEROs!\n')
                        } else {
                          obj.trans <- e$df.model[[obj2]]
                          break
                        }
                      }
                    }
                    break
                  }
                  
                } else if(opt1 %in% 6:7) {
                  # TRANSFORMATION 6: ROOT
                  # TRANSFORMATION 7: EXPONENT
                  repeat{
                    obj2 <- readline("| Please enter a positive integer: ")
                    cat("\n")
                    if(all(strsplit(obj2, split = "")[[1]] %in% as.character(0:9))) {
                      if(as.numeric(obj2) > 0) {
                        obj.trans <- as.numeric(obj2)
                        break
                      } else {
                        message("| The integer object should be POSITIVE!\n")
                      }
                    } else {
                      message("| Please do enter a positive integer!\n")
                    }
                  }
                  break
                  
                } else if(opt1 == 9) {
                  # TRANSFORMATION 9: TIME LAG
                  repeat{
                    obj2 <- readline("| Please enter an integer: ")
                    cat("\n")
                    if(all(strsplit(obj2, split = "")[[1]] %in% c("-",as.character(0:9)))) {
                      if(abs(as.numeric(obj2)) < 0.5 * nrow(e$df.model)) {
                        obj.trans <- as.numeric(obj2)
                        break
                      } else {
                        message("| The time lag influence need to be SHORTER THAN HALF of the data length!\n")
                      }
                    } else {
                      message("| Please do enter an integer!\n")
                    }
                  }
                  break
                }
              }
            }
            e$df.model <- data.frame(e$df.model, bt(e$df.model[[var]], opt1, obj.trans))
            names(e$df.model)[ncol(e$df.model)] <- paste(var, midname, obj.trans, sep = "")
            message('| A new variable "', paste(var, midname, obj.trans, sep = ""), '" is created!\n')
            break
          }
        }
        
      } else if(mm.opt == 2){
        # 2. Directly do modeling using current database;
        repeat{
          e$pred <- readline("| Please enter the name of predictor you want to test: ")
          cat("\n")
          if (!e$pred %in% names(e$df.model)){
            message('| The predictor "', e$pred, '" does not exist in the database!\n')
          } else if(e$pred %in% names(coef(e$fit.loop))){
            message('| The predictor "', e$pred, '" is already in the model!\n')
          } else{
            e$fit.loop.temp <- trial(e$df.model[1:e$cod.pos, -1], e$resp, fit = e$fit.loop, action = 2, pred = e$pred)
            loop.output(e$resp, data = e$df.model, pos = e$cod.pos, fit = e$fit.loop.temp)
            warn(fit1 = e$fit.loop, fit2 = e$fit.loop.temp)
            repeat{
              add.conf <- readline(paste('| Could you confirm the model above with predictor "', 
                                         e$pred, '" (Y/N)? ', sep = ""))
              cat("\n")
              if (!toupper(add.conf) %in% c("Y","N")){
                message('| Only "Y" or "N" is acceptable!\n')
              }else{
                if(toupper(add.conf) == "Y"){
                  e$fit.loop <- e$fit.loop.temp
                  message('| Predictor "', e$pred, '" is added to the model successfully!\n')
                } else {
                  message('| Predictor "', e$pred, '" ', "isn't added to the model!\n")
                }
                break
              }
            }
            break
          }
        }
        
      } else if(mm.opt == 3){
        # 3. Remove a variable from current model;
        repeat{
          e$pred <- readline("| Please enter the name of predictor you want to remove: ")
          cat("\n")
          if (!e$pred %in% names(e$df.model)){
            message('| The predictor "', e$pred, '" does not exist in the database!\n')
          } else if(!e$pred %in% names(coef(e$fit.loop))){
            message('| The predictor "', pred, '" does not exist in the model! \n')
          } else{
            e$fit.loop.temp <- trial(e$df.model[1:e$cod.pos, -1], e$resp, fit = e$fit.loop, action = -2, pred = e$pred)
            loop.output(e$resp, data = e$df.model, pos = e$cod.pos, fit = e$fit.loop.temp)
            warn(fit1 = e$fit.loop, fit2 = e$fit.loop.temp)
            repeat{
              rm.conf <- readline(paste('| Could you confirm the removal of predictor "', 
                                        e$pred, '" (Y/N)? ', sep = ""))
              cat("\n")
              if (!toupper(rm.conf) %in% c("Y","N")){
                message('| Only "Y" or "N" is acceptable!\n')
              }else{
                if(toupper(rm.conf) == "Y"){
                  e$fit.loop <- e$fit.loop.temp
                  message('| Predictor "', e$pred, '" is removed from the model successfully!\n')
                } else {
                  message('| Predictor "', e$pred, '" ', "is remained in the model!\n")
                }
                break
              }
            }
            break
          }
        }
        
      } else if(mm.opt == 4){
        # output
        model.xls.output(e$df.model, pos = e$cod.pos, e$resp, fit = e$fit.loop, aic.opt = NULL)
        message("| Thank you for using MSU DDI Tool! Goodbye!\n")
        break
      }
    }
  }
  
  setwd(e$wd_recover)
  
}


###################
## SUB FUNCTIONS ##
###################

#-----------------#
# 1. day.create() #
#-----------------#

day.create <- function(data, time, time_format=1){
  # INPUT: 
  #   1. time: the name of var. indicating the timeline
  #   2. time_format: time format index, 1 by default
  #                   1: m/d/y
  #                   2: y/m/d
  #                   3: d/m/y
  #   3. data: database
  #
  # OUTPUT:
  #   1. Updated data frame
  #   2. Simple summary about how many variables created
  
  # both time and time_format from the input of upper level functions "DDI"
  
  old.names <- names(data)
  
  data <- tbl_df(data)
  
  #--------------------------#
  # 1. Add Weekday Variables #
  #--------------------------#
  if(class(data[[time]]) == "Date"){
    ts <- ymd(data[[time]])
  }else if (time_format == 1){
    ts <- mdy(data[[time]])
  }else if (time_format == 2){
    ts <- ymd(data[[time]])
  }else{
    ts <- dmy(data[[time]])
  }
  data[[time]] <- ts
  
  for (i in 1:7){
    new.col <- (wday(ts) == i)*1
    data <- data.frame(data, new.col)
  }
  
  names(data) <- c(old.names, paste("T",substr(toupper(levels(wday(1, T))),1,3),sep="_"))
  
  #-----------------#
  # 2. Add Weekends #
  #-----------------#
  T_WEEKEND <- data[["T_SAT"]] + data[["T_SUN"]]
  
  #-----------------------------#
  # 3. Add Month Remaining Days #
  #-----------------------------#
  # find the last day of month
  eom <- function(date) {
    # date character string containing POSIXct date
    date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
    mon <- date.lt$mon + 2
    year <- date.lt$year
    year <- year + as.integer(mon==13) # if month was December add a year
    mon[mon==13] <- 1
    iso = ISOdate(1900+year, mon, 1, hour=0, tz=attr(date,"tz"))
    result = as.POSIXct(iso) - 86400 # subtract one day
    result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
  }
  
  T_REMAIN <- as.numeric(difftime(eom(data[[time]]), data[[time]],
                                  units ="days"))+1
  
  T_LAST_DAY <- (eom(data[[time]])==data[[time]])*1
  T_LAST_3DAY <- as.numeric(difftime(eom(data[[time]]),
                                     data[[time]],units ="days")) <= 2
  
  T_LAST_WORKDAY <- (wday(ts) %in% 2:6)*T_LAST_3DAY
  for (i in 2:length(T_LAST_WORKDAY)){
    if (T_LAST_WORKDAY[i] == 1 & T_LAST_WORKDAY[i-1] == 1){
      T_LAST_WORKDAY[i-1] <- 0
    }
  }
  
  
  #--------------------------------#
  # 4. Month / Weekday / Life-span #
  #--------------------------------#
  T_LIFESPAN <- 1:nrow(data)
  T_LIFESPAN_RCPR <- 1/T_LIFESPAN
  T_MONTH <- month(ts)
  T_WEEKDAY <- wday(ts)
  
  # combine all variables together
  data <- data.frame(data, T_WEEKEND, T_REMAIN, T_LAST_DAY, T_LAST_WORKDAY,  
                     T_LIFESPAN, T_LIFESPAN_RCPR, T_MONTH, T_WEEKDAY)
  
  # summary
  created.names <- names(data)[!names(data) %in% old.names]
  message("| ", length(created.names), " new variables are created:")
  
  console.width <- getOption("width")
  print.content <- paste("| ")
  for (i in 1:length(created.names)){
    print.content <- paste(print.content, created.names[i], 
                           if(i!=length(created.names))", ", sep ="")
    len.content <- nchar(print.content)
    short.len <- len.content - len.content%/%console.width * console.width
    if (short.len + nchar(created.names[i+1]) + nchar(" ,") > console.width){
      print.content <- paste(print.content, "\n| ", sep = "")
    }
    
  }
  message(print.content)
  cat("\n")
  
  return(data)
  
}

#-----------------#
# 2. plot.order() #
#-----------------#

plot.order <- function(data, resp, time){
  # INPUT: 
  #   1. response variable - order volume
  #   2. dataset for the plot
  # OUTPUT:
  #   1. plot: R-output of 3 charts together / .png in local drive
  #   2. data: datasets for the 3 plots
  #   3. dataset updated with a new variable created is returned as the final result
  
  require(dplyr)
  require(ggplot2)
  
  df <- data
  names(df)[names(df) == resp] <- "ORDERS"
  names(df)[names(df) == time] <- "DAY"
  # 1. volume of orders on daily basis
  time_pos <- which(names(df) == "DAY")
  resp_pos <- which(names(df) == "ORDERS")
  dly_ord <- tbl_df(df)                                              %>%
    dplyr::select(time_pos, resp_pos)
  names(dly_ord) <- c("TIME", "RESP.VAR")
  
  dly_plot <- dly_ord                                                %>%
    na.omit()                                                      %>%
    ggplot(aes(x = TIME, y = RESP.VAR))                             +
    geom_line(colour = "orange")                                       +
    ggtitle("Order Volume per Day")                                 +
    xlab("Date")                                                    +
    ylab("Order Volume")                                            +
    theme(legend.position = "none")
  
  # 2. volume of orders on weekly basis
  
  wkly_ord <- tbl_df(df)                                             %>%
    na.omit()                                                      %>%
    mutate(WDAY_CHR = factor(toupper(substr(wday(T_WEEKDAY, T),1,3)),
                             ordered = T, 
                             levels = c(toupper(substr(levels(wday(T_WEEKDAY, T)),
                                                       1,3))[2:7], "SUN")))  %>%
    group_by(WDAY_CHR)                                             %>%
    summarise(AVG.ORD = mean(ORDERS))                              
  
  # png("./DDI_OUTPUT/avg_order_by_weekday.png")
  wkly_plot <- wkly_ord                                              %>%
    ggplot(aes(x = WDAY_CHR, y = AVG.ORD))                          +
    geom_bar(stat = "identity", fill = "orange", colour = "white")  +
    ggtitle("Average Order Volume per Each Weekday")                +
    xlab("Week Day")                                                +
    ylab("Average Order Volume")                                    +
    theme(legend.position = "none")
  # dev.off()
  
  # volume of orders on monthly basis
  mthly_ord <- tbl_df(df)                                            %>%
    na.omit()                                                      %>%
    mutate(MONTH_DAY = day(DAY))                                   %>%
    group_by(MONTH_DAY)                                            %>%
    summarise(AVG.ORD = mean(ORDERS))                              
  
  # png("./DDI_OUTPUT/avg_order_by_day_of_month.png")
  mthly_plot <- mthly_ord                                            %>%
    ggplot(aes(x = MONTH_DAY, y = AVG.ORD))                         +
    geom_bar(stat = "identity", fill = "orange", colour = "white")  +
    ggtitle("Average Order Volume per Each Day of Month")           +
    xlab("Day of Month")                                            +
    ylab("Average Order Volume")                                    +
    theme(legend.position = "none")
  # dev.off()
  
  # volume of orders on yearly basis
  yrly_ord <- tbl_df(df)                                             %>%
    na.omit()                                                      %>%
    mutate(MONTH_CHR = factor(toupper(month(T_MONTH, T)),
                              ordered = T, 
                              levels = toupper(levels(month(1,T))))) %>%
    group_by(MONTH_CHR)                                            %>%
    summarise(AVG.ORD = mean(ORDERS))                              
  
  # png("./DDI_OUTPUT/avg_order_by_month.png")
  yrly_plot <- yrly_ord                                              %>%
    ggplot(aes(x = MONTH_CHR, y = AVG.ORD))                         +
    geom_bar(stat = "identity", fill = "orange", colour = "white")  +
    ggtitle("Average Order Volume per Each Month")                  +
    xlab("Month")                                                   +
    ylab("Average Order Volume")                                    +
    theme(legend.position = "none")
  # dev.off()
  
  #     if(file.exists("arrange_ggplot2.R")){
  #         source("arrange_ggplot2.R")
  #     }else{
  #         source("https://gist.githubusercontent.com/stephenturner/3724991/raw/83cd00bd249a56392eb448ac07e7e15198a67ded/arrange_ggplot2.R")
  #     }
  
  arrange_ggplot2(dly_plot, wkly_plot, mthly_plot, yrly_plot, ncol = 2)
  
  write.csv(dly_ord, "all_order.csv")
  write.csv(wkly_ord, "avg_order_by_weekday.csv")
  write.csv(mthly_ord, "avg_order_by_day_of_month.csv")
  write.csv(yrly_ord, "avg_order_by_month.csv")
  
  ggsave(plot = dly_plot, filename = "all_order.png", 
         width = 12,height = 8)
  ggsave(plot = wkly_plot, filename = "avg_order_by_weekday.png", 
         width = 12,height = 8)
  ggsave(plot = mthly_plot, filename = "avg_order_by_day_of_month.png", 
         width = 12,height = 8)
  ggsave(plot = yrly_plot, filename = "avg_order_by_month.png", 
         width = 12,height = 8)
  
  message('| The raw data for the 4 plots (plots incl.) are saved under folder "DDI_OUTPUT":')
  message('| 1. "all_order.csv" & "all_order.png"')
  message('| 2. "avg_order_by_weekday.csv" & "avg_order_by_weekday.png"')
  message('| 3. "avg_order_by_day_of_month.csv" & "avg_order_by_day_of_month.png"')
  message('| 4. "avg_order_by_month.csv" & "avg_order_by_month.png"\n')
  
  names(df)[names(df) == "DAY"] <- time
  names(df)[names(df) == "ORDERS"] <- resp
  return(df)
}

#-----------------------#
# 2.1 arrange_ggplot2() #
#-----------------------#
## Function for arranging ggplots. use png(); arrange(p1, p2, ncol=1); dev.off() to save.
suppressMessages(require(grid))
vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
  dots <- list(...)
  n <- length(dots)
  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
  if(is.null(nrow)) { nrow = ceiling(n/ncol)}
  if(is.null(ncol)) { ncol = ceiling(n/nrow)}
  ## NOTE see n2mfrow in grDevices for possible alternative
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
  ii.p <- 1
  for(ii.row in seq(1, nrow)){
    ii.table.row <- ii.row    
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
    for(ii.col in seq(1, ncol)){
      ii.table <- ii.p
      if(ii.p > n) break
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
      ii.p <- ii.p + 1
    }
  }
} 


#----------------#
# 3. lag.trans() #
#----------------#

lag.trans <- function(raw.data, resp, time, stop.point, lag.len.v = 0:70, top.no = 3){
  # INPUT: 
  # 1. raw.data: data.frame, has response variable; 
  #                              time line in time format
  # 2. resp: response variable name
  # 3. time: time variable name
  # 4. stop.point: cut-off date in time format
  # 5. lag.len.v: vector, contains lag number user wants to test
  # 6. top.no: single number, indicate the remaining number of highest correlation
  # 
  # OUTPUT: 
  # data.frame: contains all high correlation variables after lag transformation
  
  raw.ts <- xts(dplyr::select(raw.data, -starts_with(time)), order.by = raw.data[, time])
  
  raw.lag.pre <- raw.ts[, -which(colnames(raw.ts) ==resp)]
  raw.lag <- lag(raw.lag.pre, lag.len.v)
  raw.lag[is.na(raw.lag)] <- 0
  
  nrow.stop <- which(raw.data[,time] == stop.point)
  corr.vector <- cor(raw.lag[1:nrow.stop], raw.ts[, resp][1:nrow.stop])
  
  p.pstn <- regexpr("\\.", rownames(corr.vector))
  var.name <- substr(rownames(corr.vector), 1, p.pstn - 1)
  var.name[which(var.name == "")] <- rownames(corr.vector)[which(var.name == "")]
  lag.len <- rep("", length(corr.vector))
  lag.len <- substr(rownames(corr.vector), p.pstn + 1, nchar(rownames(corr.vector)))
  lag.len[which(var.name == rownames(corr.vector))] <- "0"
  
  corr.df <- data.frame(as.numeric(corr.vector), var.name, as.numeric(lag.len))
  colnames(corr.df) <- c("corr.vector", "var.name", "lag.len")
  corr.df <- tbl_df(corr.df)
  corr.df.new <- tidyr::spread(corr.df, var.name, corr.vector)
  
  order.lag <- function(vector, top.no = top.no){
    return(order(abs(vector), na.last = FALSE, decreasing = TRUE)[1:top.no])
  }
  lag.pstn <- sapply(corr.df.new[, -1], order.lag, 10)
  
  col.name <- colnames(lag.pstn)
  lag.pstn <- data.frame(matrix(lag.pstn, nrow = 10, 
                                dimnames = list(rownames = NULL, colnames = col.name)))
  h.corr <- data.frame(matrix(, nrow = 10, ncol = ncol(lag.pstn)))
  for(i in 1:ncol(lag.pstn)){
    h.corr[, i] <- corr.df.new[lag.pstn[[i]], i+1]
  }
  colnames(h.corr) <- col.name
  
  pstn_to_value <- function(lag.pstn.vector, lag.len.vector){
    return(lag.len.vector[lag.pstn.vector])
  }
  lag.h.corr <- data.frame(matrix(sapply(lag.pstn, pstn_to_value, corr.df.new$lag.len), 
                                  nrow = 10, dimnames = list(rownames = NULL, colnames = col.name)))
  corr.output <- data.frame(LAG_NO = rep("",nrow(lag.h.corr)), lag.h.corr, CORRELATION = rep("",nrow(lag.h.corr)), h.corr)
  write.csv(corr.output, "High_Corr_Lag_Top10.csv", row.names = FALSE)
  message('| "High_Corr_Lag_Top10.csv" is exported to "DDI_OUTPUT".\n')
  
  lag.h.corr <- lag.h.corr[1:top.no, ]
  lag.final0 <- data.frame()
  for(i in 1:dim(lag.h.corr)[2]){
    lag.final1 <- lag(raw.ts[, colnames(lag.h.corr)[i]], lag.h.corr[, i])
    lag.final1[is.na(lag.final1)] <- 0
    colnames(lag.final1) <- paste(colnames(lag.h.corr)[i], "LAG", 
                                  as.character(lag.h.corr[, i]),
                                  sep = "_")
    lag0.logic <- grepl("_0", colnames(lag.final1))
    colnames(lag.final1)[lag0.logic] <- colnames(lag.h.corr)[i]
    lag.final1 <- data.frame(lag.final1)
    
    if(i == 1){
      lag.final0 <- lag.final1
    } else {
      lag.final0 <- cbind(lag.final0, lag.final1)
    }
  }
  
  message("| Totally ", as.character(dim(raw.data)[2]*top.no), 
          " time lag variables have been created.")
  message('| If the original variable name is "', 
          colnames(lag.h.corr)[1], '".')
  message("| And the lag period equals to 25, which means ", 
          "\n| 25 periods lag from the data truly occurred.")
  message('| Then the newly created variable is named "', 
          colnames(lag.h.corr)[1], "_25.\n")
  
  return(lag.final0)
}


#---------------------#
# 4. specify.remain() #
#---------------------#

specify.remain <- function(data){
  df <- data
  repeat{
    repeat{
      cat("| The input formats can be: First, one number, like 5; \n")
      cat("|                           Second, 15:17, means 15, 16 and 17 days; \n")
      message("| Be careful that there can be no space or letters in your type! \n")
      num <- readline("| Please enter the number of days to be kept at the end of a month: ")
      cat("\n")
      if (grep("^[[:digit:]]*$|^[[:digit:]]*[:][[:digit:]]*$", num)!=1){
        message("| Only number is acceptable! \n")
      }else if (length(strsplit(num, ":")[[1]])==1){
        if (as.numeric(num) > 28){
          message("| The number is too large! Please try a smaller one!\n")
        }else if (as.numeric(num) < 1){
          message("| The number must be greater than 0!\n")
        }else{
          T_REMAIN <- (df$T_REMAIN == as.numeric(num))*1
          df <- data.frame(df, T_REMAIN)
          names(df) <- c(names(df)[1:ncol(df)-1], paste("T_REMAIN",num,sep="_"))
          
          message('| A new variable "',paste("T_REMAIN",num,sep="_"),
                  '" is inserted to the dataset.\n')
          break
        }
      }else{
        l_u <- as.numeric(strsplit(num, ":")[[1]])
        if (max(l_u) > 28){
          message("| The upper number is too large! Please try a smaller one!\n")
        }else if(min(l_u) < 1){
          message("| The lower number must be greater than 0!\n")
        }else{
          T_REMAIN <- (df$T_REMAIN <= max(l_u) & df$T_REMAIN >= min(l_u))*1
          df <- data.frame(df, T_REMAIN)
          names(df) <- c(names(df)[1:ncol(df)-1], 
                         paste("T_REMAIN",strsplit(num, ":")[[1]][1],
                               strsplit(num, ":")[[1]][2],sep="_"))
          message('| A new variable "',
                  paste("T_REMAIN",strsplit(num, ":")[[1]][1],
                        strsplit(num, ":")[[1]][2],sep="_"),
                  '" is inserted to the dataset.\n')
          break
        }
      }
    }
    return(df)
  }
}


#------------------#
# 5. loop.output() #
#------------------#

loop.output <- function(resp, data0, pos = cod.pos, fit) {                    
  # Input: resp(name), data0(modified), fit
  # Output: only on memory and screen, nothing to files
  
  # Consists of 4 parts:  Part. I  Summary of Fit & MAPE
  #                       Part. II  Plots
  #                       Part. III  DW-test
  #                       Part. IV  Contribution Rates
  
  # Needed Packages: "zoo" (for library(lmtest)), "lmtest" (for dwtest())
  
  x_axis <- data0[1:pos, 1]
  data <- data0[1:pos, -1]
  
  readline('| You can find the output of the model below.\n| In next 5 steps, please press <Enter> to continue...')
  cat("\n")
  
  #---------------------------------------------------------------
  # Part. I  Summary of Fit
  #---------------------------------------------------------------
  
  readline("Part. I  Summary of Fit")
  print(summary(fit))	
  cat("\n")
  
  #---------------------------------------------------------------
  # Part. II  MAPE
  #---------------------------------------------------------------
  
  readline("Part. II  MAPE")
  # Model / Daily Mape
  resp.temp <- data[[resp]]
  resp.temp[which(resp.temp == 0)] <- mean(data[[resp]])
  ape <- abs(fit$residuals/resp.temp)
  mape <- mean(abs(fit$residuals/resp.temp))
  cat("MAPE of the model (daily MAPE) is ", round(mape, 4),"\n", sep = "")
  ape.data.d <- data.frame(DAY = x_axis, 
                           Actual = data[[resp]], 
                           Predicted = fit$fitted.values, 
                           Residuals = fit$residuals, 
                           APE = ape, 
                           MAPE = c(mape, rep(NA, length(ape)-1)))
  
  # Weekly MAPE & Monthly MAPE
  wm.data0 <- mutate(ape.data.d, 
                     YEAR.M = as.character(year(ape.data.d[[1]])), 
                     MONTH = strftime(ape.data.d[[1]], format = "%m"), 
                     YEAR.W = as.character(year(ape.data.d[[1]])),
                     WEEK = strftime(ape.data.d[[1]], format = "%W"))
  for(i in 2:nrow(wm.data0)){
    if(wm.data0$WEEK[i] == "00"){
      wm.data0$WEEK[i] <- wm.data0$WEEK[i-1]
      wm.data0$YEAR.W[i] <- wm.data0$YEAR.W[i-1]
    }
  }
  wm.data1 <- wm.data0 %>%
    dplyr::mutate(YEAR.WEEK = paste(YEAR.W, WEEK, sep = "-"), 
                  YEAR.MONTH = paste(YEAR.M, MONTH, sep = "-"))
  
  # Weekly MAPE
  week.data <- wm.data1  %>%
    group_by(YEAR.WEEK)  %>%
    summarise(Actual = sum(Actual), 
              Predicted = sum(Predicted), 
              Residuals = sum(Residuals)) 
  resp.week <- week.data$Actual
  resp.week[which(resp.week == 0)] <- mean(resp.week, na.rm = TRUE)
  ape.week <- abs(week.data$Residuals/resp.week)
  mape.week <- mean(ape.week, na.rm = TRUE)
  date.week <- data0[[1]][match(unique(wm.data1$YEAR.WEEK), wm.data1$YEAR.WEEK)]
  ape.data.w <- data.frame(WEEK = date.week, week.data, APE = ape.week, 
                           MAPE = c(mape.week, rep(NA, nrow(week.data) - 1)))
  
  # Monthly MAPE
  month.data <- wm.data1  %>% 
    group_by(YEAR.MONTH)  %>% 
    summarise(Actual = sum(Actual), 
              Predicted = sum(Predicted), 
              Residuals = sum(Residuals)) 
  resp.month <- month.data$Actual
  resp.month[which(resp.month == 0)] <- mean(resp.month, na.rm = TRUE)
  ape.month <- abs(month.data$Residuals/resp.month)
  mape.month <- mean(ape.month, na.rm = TRUE)
  date.month <- ymd(paste(unique(wm.data1$YEAR.MONTH), "-1", sep = ""))
  ape.data.m <- data.frame(MONTH = date.month, month.data, APE = ape.month, 
                           MAPE = c(mape.month, rep(NA, nrow(month.data) - 1)))
  
  # MAPE screen output
  cat("Calculated Weekly MAPE is ", round(mape.week, 4),"\n", sep = "")
  cat("Calculated Monthly MAPE is ", round(mape.month, 4),"\n", sep = "")
  cat("\n")
  
  # Top 20 APE screen output
  ord.ape <- order(ape, na.last = FALSE, decreasing = TRUE)
  ape.top20 <- data.frame(DATE = x_axis[ord.ape[1:20]], 
                          APE_TOP20 = ape[ord.ape[1:20]], 
                          row.names = NULL)
  print(ape.top20)
  cat("\n")
  
  message('| Please make sure files "*_Loop_Ape.csv" are CLOSED. \n')
  readline('| Press <Enter> to continue...')
  cat("\n")
  write.csv(ape.data.d, "Daily_Loop_Ape.csv", row.names = FALSE)
  message('| "Daily_Loop_Ape.csv" file is exported to "DDI_OUTPUT". ')
  write.csv(ape.data.w, "Weekly_Loop_Ape.csv", row.names = FALSE)
  message('| "Weekly_Loop_Ape.csv" file is exported to "DDI_OUTPUT". ')  
  write.csv(ape.data.m, "Monthly_Loop_Ape.csv", row.names = FALSE)
  message('| "Monthly_Loop_Ape.csv" file is exported to "DDI_OUTPUT". \n')  
  
  #---------------------------------------------------------------
  # Part. III  Plots (Updated on Friday 03/06/2015)
  #---------------------------------------------------------------
  
  # use arrange_ggplot2() function
  
  na <- readline("Part. III  Plots")
  message("Please look at the Plots area!\n")
  
  df_to_plot <- data.frame(x_axis, res = summary(fit)$residuals)
  if(!class(df_to_plot$x_axis)[1] %in% c("POSIXct", "POSIXt", "numeric", "integer")){
    df_to_plot$x_axis <- seq_along(1:nrow(df_to_plot))
  }
  # 1. scatterplot of residuals
  scat_res <- df_to_plot                                  %>%
    ggplot(aes(x = x_axis, y = res))                     +
    geom_point(colour = "orange", size = 3)              +
    theme(legend.position = "none")                      +
    geom_hline(yintercept = 0, colour = "red")           +
    ggtitle("Scatterplot of Model Residuals")            +
    xlab("Observation Index")                            +
    ylab("Residuals")
  
  # 2. histogram of residuals
  max_res <- max(df_to_plot$res)
  min_res <- min(df_to_plot$res)
  mean_res <- mean(df_to_plot$res)
  binrange <- (max_res - min_res)/40
  hist_res <- df_to_plot                                             %>%
    ggplot(aes(x = res))                                            +
    geom_histogram(stats = "identity", fill = "orange", 
                   colour = "white", binwidth = binrange)           +
    geom_vline(xintercept = mean_res, colour = "red")               +
    ggtitle("Histogram of Model Residuals")                         +
    xlab("Residual Interval")                                       +
    ylab("Residuals")
  
  # 3. fit vs. actual
  fit.resp <- fit$fitted.values
  act.resp <- data[,resp]
  fit_to_plot <- data.frame(x_axis, fit.resp, act.resp)
  if(!class(fit_to_plot$x_axis)[1] %in% c("POSIXct", "POSIXt", "numeric", "integer")){
    fit_to_plot$x_axis <- seq_along(1:nrow(fit_to_plot))
  }
  
  fit_vs_act <- fit_to_plot                                        %>%
    ggplot(aes(x_axis))                                           +
    geom_line(aes(y = act.resp), colour = "darkgrey", size = 1)   +
    geom_line(aes(y = fit.resp), colour = "red", size = 1)        +
    ggtitle("Modeled vs. Actual Variable")                        +
    ylab("Count / Volume")                                        +
    theme(legend.position = "right", 
          axis.title.x = element_blank())
  
  arrange_ggplot2(scat_res, hist_res, fit_vs_act, ncol = 1)
  ggsave("scatterplot_residuals.png", scat_res, 
         width = 12,height = 8)
  ggsave("histogram_residuals.png", hist_res, 
         width = 12,height = 8)
  ggsave("fit_vs_actual.png", fit_vs_act, 
         width = 12,height = 8)
  message('| The 3 plots are exported to folder "DDI_OUTPUT":')
  message('| 1. "scatterplot_residuals.png"')
  message('| 2. "histogram_residuals.png"')
  message('| 3. "fit_vs_actual.png"\n')
  
  #---------------------------------------------------------------
  # Part. IV  DW-test
  #---------------------------------------------------------------
  
  na <- readline("Part. IV  DW-test")
  print(dwtest(fit))
  
  #---------------------------------------------------------------
  # Part. V  Contribution Rates
  #---------------------------------------------------------------    
  
  na <- readline("Part. V  Contribution Rates")
  cat("\n")
  
  # contri.d: draft    
  simulation <- cbind(coef(fit)[1], t(t(as.matrix(data[, names(coef(fit))[-1]])) * as.vector(coef(fit)[-1])))
  colnames(simulation) <- names(coef(fit))
  contri.d <- colSums(simulation)/sum(fit$fitted.values)
  contri <- contri.d
  
  # contri.p: positive
  contri.p <- contri.d[which(contri.d >= 0)]
  contri.p <- as.matrix(contri.p[order(contri.p, decreasing = T)])
  
  contri.top10 <- as.matrix(head(contri.p[which(rownames(contri.p) != "(Intercept)")], 10))
  colnames(contri.top10) <- "Top POSITIVE Contributors"
  rn.p <- rownames(contri.p)[which(rownames(contri.p) != "(Intercept)")]
  rownames(contri.top10) <- if (length(rn.p)>10) rn.p[1:10] else rn.p
  
  
  # contri.n: negative
  contri.n <- contri.d[which(contri.d < 0)]
  contri.n <- as.matrix(contri.n[order(contri.n, decreasing = F)])
  
  contri.bot5 <- as.matrix(head(contri.n[which(rownames(contri.n) != "(Intercept)")], 5))
  colnames(contri.bot5) <- "Top NEGATIVE Contributors"
  rn.n <- rownames(contri.n)[which(rownames(contri.n) != "(Intercept)")]
  rownames(contri.bot5) <- if (length(rn.n)>5) rn.n[1:5] else rn.n
  
  
  # message('Voila the top contributor(s) to the variable "', resp, '":', sep ="")
  if(nrow(contri.top10) > 0){
    print(contri.top10)
    cat("\n")
  }
  if(nrow(contri.bot5) > 0){
    print(contri.bot5)
    cat("\n")
  }
  
  #---------------------------------------------------------------
  # Part. VI  VIF (Updated on Friday 12/19/2014)
  #---------------------------------------------------------------  
  
  if(length(coef(fit)) > 2) {
    vif <- as.matrix(vif(fit))
    colnames(vif) <- "VIF"
    na <- readline("Part. VI  VIF")
    cat("\n")
    print(vif)
    cat("\n")
    
    if(any(vif >= 10)){
      message(paste("VIF of", paste(rownames(vif)[which(vif >= 10)], collapse = ", "), 
                    if(length(which(vif >= 10)) > 1) "are" else "is", 
                    "bigger than 10! ", sep = " "))
      cat("\n")
    }
  }
  
  cat(paste(rep("-", 40), collapse = ""), "", sep = "\n")
  
}	# end of function loop.output()


#-----------------------#
# 6. model.xls.output() #
#-----------------------#

model.xls.output <- function(data, pos, resp, fit, aic.opt = NULL){
  # INPUT: 
  # 1. df.model: complete data with all variables, and time in 1st column 
  # 2. fit
  # OUTPUT:
  # 1. One Excel File
  
  message('| Please make sure "model_result', aic.opt, '.xlsx" is CLOSED in "DDI_OUTPUT". ')
  message('| Please make sure necessary file is already COPIED to another directory. \n')
  readline("| Press <Enter> to continue...")
  cat("\n")
  
  coef <- coef(summary(fit))
  
  # Calculate VIF
  if(length(coef(fit)) > 2) {
    vif <- as.matrix(vif(fit))
    if(rownames(coef)[1] == "(Intercept)"){
      vif <- matrix(c(NA, vif), nrow = nrow(coef), 
                    dimnames = list(c("(Intercept)", rownames(vif)), "VIF"))
    }
  }
  
  # Calculate Contribution Rate
  simulation <- cbind(coef(fit)[1], t(t(as.matrix(data[1:pos, names(coef(fit))[-1]])) * as.vector(coef(fit)[-1])))
  colnames(simulation) <- names(coef(fit))
  contri.d <- colSums(simulation)/sum(fit$fitted.values)
  contri <- contri.d
  
  # Calculate Mape
  resp.temp <- data[[resp]][1:pos]
  resp.temp[which(resp.temp == 0)] <- mean(resp.temp)
  ape <- abs(fit$residuals/resp.temp)
  mape <- mean(abs(fit$residuals/resp.temp))
  
  # bind coef, contri.rate, vif, r.squared, adj.r.squared, dw.test, mape
  model.coef <- data.frame(coef, Contri.Rate = contri, VIF = vif, 
                           R.Squared = c(summary(fit)[[8]], rep(NA, nrow(coef)-1)), 
                           Adj.R.Squared = c(summary(fit)[[9]], rep(NA, nrow(coef)-1)), 
                           DW.test = c(dwtest(fit)[[1]], rep(NA, nrow(coef)-1)),
                           MAPE = c(mape, rep(NA, nrow(coef)-1)))
  
  # Calculate Predicted Values
  pred.data <- data.frame(Intercept = rep(1,nrow(data)), 
                          data[, match(rownames(coef)[-1], names(data))])
  pred.value <- as.matrix(pred.data) %*% coef(fit)
  # pred.value <- predict(fit, new.data = data)    
  pred.data.af <- t(t(pred.data) * coef(fit)) # data after multiplied by coef
  model.data <- data.frame(Date = data[[1]], 
                           Residuals = c(fit$fitted.values, rep(NA, nrow(data) - pos)), 
                           APE = c(ape, rep(NA, nrow(data) - pos)), 
                           Actual = data[[resp]], 
                           Predicted = pred.value, 
                           pred.data.af)
  
  # create variables for plotting predicted values
  model.var <- c(pred.value[1:pos], rep(NA, nrow(data)-pos))
  pred.var <- c(rep(NA, pos-1), pred.value[(pos):nrow(data)])
  
  # Plot Daily / Model Predicted Values
  final.plot1 <- final.plot(data[[resp]], model.var, pred.var, data[[1]], aic.opt = aic.opt, period = "Daily_")
  
  # Model / Daily Mape
  resp.temp <- data[[resp]]
  resp.temp[which(resp.temp == 0)] <- mean(data[[resp]])
  ape <- abs(fit$residuals/resp.temp)
  mape <- mean(abs(fit$residuals/resp.temp))
  ape.data.d <- data.frame(DATE = data[[1]], 
                           Actual = data[[resp]], 
                           Predicted = pred.value, 
                           Residuals = c(fit$residuals, rep(NA, nrow(data) - length(fit$residuals))), 
                           APE = c(ape, rep(NA, nrow(data) - length(ape))), 
                           MAPE = c(mape, rep(NA, nrow(data) - 1)))
  
  # Weekly MAPE & Monthly MAPE
  wm.data0 <- mutate(ape.data.d, 
                     YEAR.M = as.character(year(ape.data.d[[1]])), 
                     MONTH = strftime(ape.data.d[[1]], format = "%m"), 
                     YEAR.W = as.character(year(ape.data.d[[1]])), 
                     WEEK = strftime(ape.data.d[[1]], format = "%W"))
  for(i in 2:nrow(wm.data0)){
    if(wm.data0$WEEK[i] == "00"){
      wm.data0$WEEK[i] <- wm.data0$WEEK[i-1]
      wm.data0$YEAR.W[i] <- wm.data0$YEAR.W[i-1]
    }
  }
  wm.data1 <- wm.data0 %>%
    dplyr::mutate(YEAR.WEEK = paste(YEAR.W, WEEK, sep = "-"), 
                  YEAR.MONTH = paste(YEAR.M, MONTH, sep = "-"))
  
  # Weekly MAPE
  week.data <- wm.data1  %>%
    group_by(YEAR.WEEK)  %>%
    summarise(Actual = sum(Actual), 
              Predicted = sum(Predicted), 
              Residuals = sum(Residuals)) 
  resp.week <- week.data$Actual
  resp.week[which(resp.week == 0)] <- mean(resp.week, na.rm = TRUE)
  ape.week <- abs(week.data$Residuals/resp.week)
  mape.week <- mean(ape.week, na.rm = TRUE)
  date.week <- wm.data0[[1]][match(unique(wm.data1$YEAR.WEEK), wm.data1$YEAR.WEEK)]
  ape.data.w <- data.frame(WEEK = date.week, week.data, APE = ape.week, 
                           MAPE = c(mape.week, rep(NA, nrow(week.data) - 1)))
  
  model.var.w <- c(week.data$Predicted[which(!is.na(ape.week))], rep(NA, sum(is.na(ape.week))))
  pred.var.w <- c(rep(NA, sum(!is.na(ape.week))-1), 
                  week.data$Predicted[(min(which(is.na(ape.week)))-1):max(which(is.na(ape.week)))])
  final.plot2 <- final.plot(resp.week, model.var.w, pred.var.w, date.week, aic.opt = aic.opt, period = "Weekly_")
  
  # Monthly MAPE
  month.data <- wm.data1  %>% 
    group_by(YEAR.MONTH)  %>% 
    summarise(Actual = sum(Actual), 
              Predicted = sum(Predicted), 
              Residuals = sum(Residuals)) 
  resp.month <- month.data$Actual
  resp.month[which(resp.month == 0)] <- mean(resp.month, na.rm = TRUE)
  ape.month <- abs(month.data$Residuals/resp.month)
  mape.month <- mean(ape.month, na.rm = TRUE)
  date.month <- ymd(paste(unique(wm.data1$YEAR.MONTH), "-1", sep = ""))
  ape.data.m <- data.frame(MONTH = date.month, month.data, APE = ape.month, 
                           MAPE = c(mape.month, rep(NA, nrow(month.data) - 1)))
  
  model.var.m <- c(month.data$Predicted[which(!is.na(ape.month))], rep(NA, sum(is.na(ape.month))))
  pred.var.m <- c(rep(NA, sum(!is.na(ape.month))-1), 
                  month.data$Predicted[(min(which(is.na(ape.month)))-1):max(which(is.na(ape.month)))])
  final.plot3 <- final.plot(resp.month, model.var.m, pred.var.m, date.month, aic.opt = aic.opt, period = "Monthly_")
  arrange_ggplot2(final.plot1, final.plot2, final.plot3, ncol = 1)
  
  # write.table
  if(suppressMessages(require(xlsx))){
    wb <- createWorkbook()
    
    sht1 <- createSheet(wb, sheetName = "Model_Coef")
    sht2 <- createSheet(wb, sheetName = "Weekly_Predict")
    sht3 <- createSheet(wb, sheetName = "Monthly_Predict")
    sht4 <- createSheet(wb, sheetName = "Model_Data")
    sht5 <- createSheet(wb, sheetName = "Full_Data")
    
    addDataFrame(model.coef, sht1)
    addDataFrame(ape.data.w, row.names = FALSE, sht2)
    addDataFrame(ape.data.m, row.names = FALSE, sht3)
    addDataFrame(model.data, row.names = FALSE, sht4)
    addDataFrame(data, row.names = FALSE, sht5)
    
    saveWorkbook(wb, paste("model_result", aic.opt, ".xlsx", sep = ""))
    message('| The model results are saved as "model_result',aic.opt,
            '.xlsx" under folder "DDI_OUTPUT".\n')
    
  } else {
    write.csv(model.coef, paste("Model_Coef",aic.opt,".csv"))
    write.csv(week.data, paste("Weekly_Predict",aic.opt,".csv"))
    write.csv(month.data, paste("Monthly_Predict",aic.opt,".csv"))
    write.csv(model.data, paste("Model_Data",aic.opt,".csv"))
    write.csv(data, paste("Full_Data",aic.opt,".csv"))
    
    message('| The result files of the model are saved under folder "DDI_OUTPUT":')
    message('|   1. "Model_Coef',aic.opt,'.csv"')
    message('|   2. "Weekly_Predict',aic.opt,'.csv"')
    message('|   3. "Monthly_Predict',aic.opt,'.csv"')
    message('|   4. "Model_Data',aic.opt,'.csv"')
    message('|   5. "Full_Data',aic.opt,'.csv"')
  }
  
}


#---------#
# 7. bt() #
#---------#

bt <- function(x, type, object = NULL){
  #---------------------------------
  # basic transformation:
  # 1~4: +,-,*,/
  # 5: logarithm
  # 6: root
  # 7: exponent (exponent > 1; integers only; different than pc(x, exponent))
  # 8: reciprocal
  # 9: time lag
  #---------------------------------
  # x: variable to be transformed
  # type: index of transformation type - integers **0~9**!!!
  # object: Default is NULL. When type = certain index, object to be provided
  #---------------------------------  
  # make sure the 'x' is already loaded to upper level environment!!!
  # if 'object' is a variable, make sure 'object' is already loaded to upper level environment!!!
  # the validity/feasibility of using the 'x' and 'object' need to be verified OUTSIDE bt(...)!!!
  #---------------------------------
  
  #----------------------------------------#
  # PART I - Transformation method 1.1~1.4 #
  #----------------------------------------#
  asmd <- function(x, type, object){
    # asmd stands for addition/subtraction/multiplication/division
    
    # type to be transformed from numbers to letters in capital
    # type is supposed to be in the format 1.1~1.4, need to capture the 3rd field
    # type is supposed to be formed OUTSIDE bt(...) by asking user questions
    opt <- LETTERS[as.numeric(type)]
    
    # check the object OUTSIDE bt(...): a number, min/max/mean or a data serie/variable
    # number keeps as numeric vector (length(object) = 1) in object
    # "min"/"max"/"mean" should be turned into min(x)/max(x)/mean(x) before store into object
    # variable stored as numeric vector (length(object) = length(x))
    
    switch(opt,
           A = x + object,
           B = x - object,
           C = x * object,
           D = x / object)
  }
  
  #-----------------------------------#
  # PART II - Transformation method 9 #
  #-----------------------------------#
  tlag <- function(x, object){
    # create lagged data series (forward or backward)
    
    # object should be an integer (abs(object) < length(x))
    # object validity should be checked OUTSIDE tlag(...) & bt(...)
    
    object <- as.numeric(object)
    a <- lag(x, object)
    a[is.na(a)] <- 0
    return(a)
    
  }
  
  #-----------------------------------#
  # PART III - General transformation #
  #-----------------------------------#
  general.opt <- letters[if(type<=4) 1 else type-3]
  switch(general.opt,
         # 1.1~1.4: +,-,*,/
         a = asmd(x , type, object),
         # 2: log
         b = log(x),
         # 3: root
         c = x^(1/object),
         # 4: exponent
         d = x^object,
         # 5: reciprocal
         e = 1/x,
         # 6: time lag
         f = tlag(x, object))
  
}


#------------#
# 8. trial() #
#------------#

trial <- function(data, resp, fit = NULL, action = 2, pred = NULL) {          		
  # Build the model				
  # based on NULL or fit1, add/remove a predictor/the intercept, output summary				
  
  # action: -1 means delete the Intercept
  #          1 means add the Intercept
  #          2 means add a predictor
  #         -2 means delete a predictor
  
  if(action == -1 | action == 1) pred <- "(Intercept)"
  
  if(is.null(fit)){ # if(exists("fit", mode = "list"))
    if(action == 1) {
      fit.new <- lm(as.formula(sprintf('%s ~ 1', resp)), data = data, na.action = na.exclude)
    } else if(action == 2) {
      fit.new <- lm(as.formula(sprintf('%s ~ %s + 0', resp, pred)), data = data, na.action = na.exclude)
    } else if(action == -1) {
      fit.new <- lm(as.formula(sprintf('%s ~ -1', resp)), data = data, na.action = na.exclude)
    } else { # if(action == -2)
      message("There's no existed model to let you delete ", pred, " from!", sep = "")
      cat("\n")
      fit.new <- fit
    }
    
  } else { #if(!is.null(fit))
    if(pred %in% names(coef(fit))) {
      if(action == -1) {
        fit.new <- update(fit, ~. -1, data = data)
      } else if(action == -2) {
        fit.new <- update(fit, as.formula(sprintf('~. - %s', pred)), data = data)
      } else {
        message("The ", e$pred, " is already in the model!", sep = "")
        cat("\n")
        fit.new <- fit
      }
    } else {
      if(action == 1){
        fit.new <- update(fit, ~. + 1, data = data)
      } else if(action == 2){
        fit.new <- update(fit, as.formula(sprintf('~. + %s', pred)), data = data)
      } else {
        message("The ", pred, " isn't in the model!", sep = "")
        cat("\n")
        fit.new <- fit
      }
    }    
  }  
  return(fit.new)
}


#-----------#
# 9. warn() #
#-----------#

warn <- function(fit1, fit2, p.cons = 0.2) {
  # fit2 is the newest fit
  # 1. warn user when there is some coefficient which changes **sign** between two models
  # 2. warn user when there is big gap between **p-value** of one pred in two models 
  
  
  if(!is.null(names(coef(fit2)))) {
    
    coef2 <- coef(fit2)
    name2 <- names(coef2)
    p.value2 <- coef(summary(fit2))[, 4]
    
    if(sum(p.value2 > p.cons) > 0){
      message("P-value of the following predictor", 
              if(sum(p.value2 > p.cons) > 1) "s are" else " is", 
              " larger than ", as.character(p.cons), sep = "")
      cat(paste(names(p.value2)[p.value2 > p.cons], collapse = ", "), "", sep = "\n")
      cat(paste(rep("-", 40), collapse = ""), "", sep = "\n")
    }
    
    
    if(!is.null(names(coef(fit1)))){
      coef1 <- coef(fit1)    
      name1 <- names(coef1)
      p.value1 <- coef(summary(fit1))[, 4]
      
      name <- intersect(name1, name2)
      
      sign1 <- sign(coef1[which(name1 %in% name)])
      sign1 <- sign1[order(names(sign1))]
      
      sign2 <- sign(coef2[which(name2 %in% name)])
      sign2 <- sign2[order(names(sign2))]
      
      if(any(sign1 * sign2 == -1)) {
        message("Sign of the following predictor's coefficient has changed!", sep = "")
        cat(paste(names(sign1)[sign1 * sign2 == -1], collapse = ", "), "", sep = "\n")
        cat(paste(rep("-", 40), collapse = ""), "", sep = "\n")
      }
    } 
    
  } 
  
} # end function warn()


#------------------#
# 10. final.plot() #
#------------------#

# plot Actual Value, Model Value & Predicted Value 
final.plot <- function(resp, model.var, pred.var, time.var, aic.opt = aic.opt, period = NULL){
  suppressMessages(require(ggplot2))
  options(warn = -1)
  df_plot <- data.frame(TIME = time.var, 
                        ACTUAL = resp, 
                        MODELED = model.var, 
                        PREDICTED = pred.var)
  
  fit_extend <- df_plot                                         %>%
    ggplot(aes(TIME))                                          +
    geom_line(aes(y = ACTUAL), colour = "darkgrey", size = 1)  +
    geom_line(aes(y = MODELED), colour = "red", size = 1)      +
    geom_line(aes(y = PREDICTED), colour = "orange", size = 1,
              linetype = "dashed")                             +
    ggtitle(paste(period, "Model & Prediction", sep = ""))     +
    ylab("Count / Volume")                                     +
    theme(legend.position = "right", 
          axis.title.x = element_blank())
  
  ggsave(paste(period, 'model_prediction',aic.opt,'.png', sep = ""),
         fit_extend, width = 12,height = 8)
  message('| Prediction plot "', period, 'model_prediction',aic.opt,'.png" is exported! ')
  return(fit_extend)
  
  options(warn = 1)
}
