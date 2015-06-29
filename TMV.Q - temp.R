TMV.Q <- function(index, max.freq, max.cor, max.topic = 10, max.term = 10){
	# collection of all questions in TMV program
	# 1. indicating index and bump up corresponding questions
	# 2. determine the validity of the input for each question
	# 3. return the answer in corresponding format and value
	
	# parameters:
	# index: which question to be used in corresponding situation
	# max.freq: in the association plot, the frequency input could not exceed the maximum frequency in the dtm
	# max.freq: in the association plot, the correlation input could not exceed the maximum correlation in the dtm
	
	# !environment e is created outside this function!
	
	# Index = 1
	# Question 1: Enter the Data Source Name (only .csv with one column of data is permitted)
	# input: file name (.csv format obliged)
	# output: e$raw, data frame, read the raw data from local directory and save it in environment e
	Q1 <- function(){
		repeat{
			raw.name <- readline("| Please enter the data file name: ")
			cat("\n")
			endstr <- substr(raw.name, nchar(raw.name)-3, nchar(raw.name))
			if(!endstr == ".csv"){
				message('| Only "*.csv" file is acceptable!\n')
			} else if(!file.exists(raw.name)){
			  message('| There is no file called "', raw.name, '" in "Text_WD". \n')
			} else {
				raw <- na.omit(read.csv(raw.name, header = FALSE, stringsAsFactors = FALSE))
				if(!ncol(raw)==1) {
				  message("| Please confirm that your data has 1 single column!\n")
				  # !only applicable for single column input. need to find alternative solutions for further development
				} else {
				  return(raw)
				  break
				}
			}
		}
	}
	
	# Index = 2
	# Question 4.1: Add Additional Stopwords
	# input: N - do not add stopwords, c("word1", "word2", ...) - a string of stopwords
	# output: e$stops, character vector, a vector of additional stopwords 
	Q4.1 <- function(){
	  repeat{
	    message("| Do you have any Additional Stopwords? ")
	    message("| If none, press <Enter> without typing any letters. ")
	    stps <- readline('| Please specify (use comma "," to split words): ')
	    cat("\n")
	    if(!all(strsplit(tolower(stps), "")[[1]] %in% c(" ", ",", "", letters))){
	      message("| Unacceptable character is entered! Please type again! \n")
	    } else break
	  }
		
		stops <- strsplit(gsub(" ", "", tolower(stps)), ",")[[1]]
		
		if(length(stops) == 0) {
      message("| You don't need any Additional Stopwords. ")
    } else {
      message("| The new stopword(s) that you specified: ")
			
			console.width <- getOption("width")
			print.content <- paste("| ")
			for (i in 1:length(stops)){
				print.content <- paste(print.content, stops[i], 
															 if(i!=length(stops))", ", sep ="")
				len.content <- nchar(print.content)
				short.len <- len.content - len.content%/%console.width * console.width
				if (short.len + nchar(stops[i+1]) + nchar(" ,") > console.width){
					print.content <- paste(print.content, "\n| ", sep = "")
				}
			}
			message(print.content)
			cat("\n")
      # cat(stops, sep = ", ")
    }
		
    repeat{
      opt <- readline("| Could you confirm (Y/N)? ")
      cat("\n")
      if(!toupper(opt) %in% c("Y", "N")){
        message('| Only "Y" or "N" is acceptable! \n')
      } else break
    }
    if(toupper(opt) == "Y") {
			return(stops)
			break
    }
	}
	
	# Index = 3
	# Question 4.2: Sparsity?
	# Input: sparsity rate
	# Output: e$spars, numeric vector of length 1
	Q4.2 <- function(){
		repeat{
		  message("| Please specify the Sparsity, which should be a value between 0 and 1. ")
			spars <- readline("| How much do you want the sparsity to be for afterwards analysis: ")
			cat("\n")
			if(!all(strsplit(spars, split = "")[[1]] %in% c(as.character(0:9),"."))) {
				message("| Please do enter a positive decimal between 0 and 1 (excl.)!\n")
			}else if(as.numeric(spars) >= 1 |as.numeric(spars) <= 0){
				message("| Please do enter a positive decimal between 0 and 1 (excl.)!\n")
			}else {
				return(as.numeric(spars)) 
				break
			}
		}
	}
	
	# Index = 4
	# Question 5: a series of questions for visualization parameters in different plot
	# Question 5.1: No. of columns (no. of top words) in word frequency plot
	# Input: No. of columns
	# Output: e$ncol_freq, numeric vector of length 1
	Q5.1 <- function(){
		repeat{
			ncol_freq <- readline("| How many words to you want to keep in the frequency plot: ")
			cat("\n")
			if(!all(strsplit(ncol_freq, split = "")[[1]] %in% as.character(0:9))) {
				message("| Please do enter a positive integer!\n")
			} else {
				return(as.numeric(ncol_freq))
				break
			}
		}
	}
	
	# Index = 5
	# Question 5.2: Minimum frequency for a word to get into the wordcloud
	# Input: No. of minimum frequency
	# Output: e$min_freq_wc, numeric vector of length 1
	Q5.2 <- function(){
		repeat{
			min_freq_wc <- readline("| Please enter the minimum frequency for a word to get into wordcloud: ")
			cat("\n")
			if(!all(strsplit(min_freq_wc, split = "")[[1]] %in% as.character(0:9))) {
				message("| Please do enter a positive integer!\n")
			} else {
				return(as.numeric(min_freq_wc)) 
				break
			}
		}
	}
	
	# Index = 7
	# Question 5.3: Minimum frequency for a word to get into the association plot
	# Input: 1. min frequency for a variable to get into the association plot
	# 			 2. min collection for a variable to get into the association plot
	# Output: e$low_freq, numeric vector of length 1; e$cor_thres, numeric vector of length 1
	# Parameters: 1. max.freq: maximum frequency of a word in the dtm
	#							2. max.cor: maximum correlation between words in the dtm
	Q5.3 <- function(max.freq = max.freq, max.cor = max.cor){
		repeat{
			low_freq <- readline("| Please enter the min. freq. for a word to enter the plot: ")
			cor_thres <- readline("| Please enter the min. cor. rate for a word to enter the plot: ")
			cat("\n")
			if(!all(strsplit(low_freq, split = "")[[1]] %in% as.character(0:9))){
				message("| Please do enter a positive integer for the frequency!\n")
			}else if(as.numeric(low_freq) > max.freq){
				message("| Your input exceeds the maximum frequency: ", max.freq, "!\n")
			}else if(!all(strsplit(cor_thres, split = "")[[1]] %in% c(as.character(0:9),"."))){
				message("| Please do enter a positive decimal between 0 and 1 for the correlation!\n")
			}else if(as.numeric(cor_thres) > max.cor){
				message("| Your input exceeds the maximum correlation: ", max.cor, "!\n")
			}else{
				return(c(as.numeric(low_freq), as.numeric(cor_thres)))
				break
			}
		}
	}
	
	# Index = 8
	# Question 5.4: No. of Clusters
	# Input: No. of clusters
	# Output: e$no.clust, numeric vector of length 1
	Q5.4 <- function(){
		repeat{
			no_clust <- readline("| Please check the dendrogram and specify the number of clusters: ")
			cat("\n")
			if(!all(strsplit(no_clust, split = "")[[1]] %in% as.character(0:9))) {
				message("| Please do enter a positive integer!\n")
			} else {
				return(as.numeric(no_clust))
				break
			}
		}
	}
	
	# Index = 9
	# Question 999: - Satisfactory question
	# Input: Y/N
	# Output:
	Q999 <- function(){
		repeat{
			opt <- readline("| Are you satisfied (Y/N)? ")
			cat("\n")
			if(!toupper(opt) %in% c("Y", "N")){
				message("| Only Y or N is acceptable! \n")
			} else {
			  return(toupper(opt))
			  break
			}
		}
	}
	
	# Index = 10
	# Question 6.1 : - No. of Topics and No. of Terms 1 Topic can have
	# Input: 1. No. of Topics
	#        2. No. of Terms
	Q6.1 <- function(max.topic = max.topic, max.term = max.term) {
	  repeat{
	    n_topic <- readline("| Please enter the number of topic you'd like: ")
	    n_term <- readline("| Please enter the number of terms one topic should have: ")
	    cat("\n")
	    if(!all(strsplit(n_topic, split = "")[[1]] %in% as.character(0:9))){
	      message("| Please do enter a positive integer for the frequency!\n")
	    }else if(as.numeric(n_topic) > max.topic){
	      message("| Your input exceeds the maximum frequency: ", max.freq, "!\n")
	    }else if(!all(strsplit(n_term, split = "")[[1]] %in% c(as.character(0:9),"."))){
	      message("| Please do enter a positive decimal between 0 and 1 for the correlation!\n")
	    }else if(as.numeric(n_term) > max.term){
	      message("| Your input exceeds the maximum correlation: ", max.cor, "!\n")
	    }else{
	      return(c(as.numeric(n_topic), as.numeric(n_term)))
	      break
	    }
	  }
	}
	
	L_index <- LETTERS[index]
  switch(L_index,
         A = Q1(),
         B = Q4.1(),
         C = Q4.2(),
         D = Q5.1(),
         E = Q5.2(),
         G = Q5.3(max.freq, max.cor),
         H = Q5.4(),
				 I = Q999(), 
				 J = Q6.1(max.topic, max.term))
}





















