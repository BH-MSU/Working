#--------------------------------------#
# TMV.Q() (THE QUESTION LIST FUNCTION) #
#--------------------------------------#
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
      message("| Please specify the Sparsity value between 0 and 1. ")
      spars <- readline("| Enter a decimal please: ")
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
      ncol_freq <- readline("| How many words do you want for the frequency plot: ")
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
      min_freq_wc <- readline("| Please enter the minimum word frequency for the wordcloud: ")
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
      low_freq <- readline("| Please enter the min. freq. of a word for the plot: ")
      cor_thres <- readline("| Please enter the min. cor. rate for a word to enter the plot: ")
      cat("\n")
      if(!all(strsplit(low_freq, split = "")[[1]] %in% as.character(0:9))|nchar(low_freq)==0){
        message("| Please do enter a positive integer for the frequency!\n")
      }else if(as.numeric(low_freq) > max.freq){
        message("| Your input exceeds the maximum frequency: ", max.freq, "!\n")
      }else if(!all(strsplit(cor_thres, split = "")[[1]] %in% c(as.character(0:9),"."))|nchar(low_freq)==0){
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
      no_clust <- readline("| Please specify the number of clusters: ")
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
      opt <- readline("| Are you OK with the result (Y/N)? ")
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

#---------------#
# circos.plot() #
#---------------#
circos.plot <- function(dtm, pair.cor, min.cor, word = NULL){
	# dtm: document-term matrix in normal matrix format
	# pair.cor: correlation data frame with 3 columns:
	#						1. Word 1, type character
	#						2. Word 2, type character
	#						3. correlation, type numeric
	# word: if it's an association plot for a specified word, specify it.
	if(!require(RColorBrewer))require(RColorBrewer)
	if(!require(circlize))require(circlize)
	if(!require(GISTools))require(GISTools)
	
	# STEP 1.1
	# freq: frequency of words in the corpus in descending order
	freq <- data.frame(category = colnames(dtm), freq = colSums(dtm),
										 stringsAsFactors = F)
	freq <- freq[order(freq[,2], decreasing = T),]
	
	# STEP 1.2
	# if the focused word is specified, return all words (if # < 20) or top 20 words (sorting by frequency) which have connection with the word
	# if not, return top 20 words (sorting by frequency)
	# keep only the words satisfying the criteria above
	if (!is.null(word)){
		len.word <- ifelse(nrow(pair.cor) < 19, nrow(pair.cor), 19)  # including the word specified totally 20 (19+1) words
		topwords <- c(pair.cor[1:len.word, 2], word) # first len.word words ordered by correlation rate with the specified word
	}else{
		len.word <- ifelse(nrow(freq) < 20, nrow(freq), 20)
		topwords <- freq[1:len.word, 1] # first len.word words ordered by frequency
	}
	
	# STEP 1.3 
	# Allocate the selected words and related frequency: e$freqchosen
	# freq2 step 1: Extend e$freqchosen, make each term have an interval (to facilitate plotting)
	# freq2 step 2: Add a column "Pseudo", select from LETTERS by the length of e$freqchosen
	# str(freq2): 
	# 	3 columns - category(words/terms), freq(terms frequency), pseudo(A,B,C,D,...)
	# 	len.word observations in frequency descending order
	e$freqchosen <- freq[freq[, 1] %in% topwords, ]    #subset(freq, freq[,1] %in% topwords)
	freq2 <- rbind(e$freqchosen, 
								 data.frame(category = e$freqchosen[,1], 
														freq = rep(0,length(topwords)),
														stringsAsFactors = F))
	freq2 <- freq2[order(freq2[,2], decreasing = T),]
	freq2 <- data.frame(freq2, pseudo = rep(LETTERS[1:length(topwords)],2),
											stringsAsFactors = F)
											
	# STEP 2.1 
	# initialize the circos plot
	par(mar = c(1,1,1,1), lwd = 0.1, cex = 0.7)
	circos.par("track.height" = 0.1)
	# circos.initialize(factors = freq2$category, x = freq2$freq)
	circos.initialize(factors = rep(freq2$pseudo), x = freq2$freq)
	
	# STEP 2.2
	# plot the circos:
	# 	sector range depends on word frequency
	# 	words and their frequency are also plotted
	circos.trackPlotRegion(track.height = .15, ylim = c(0, 2),
												 bg.col =  brewer.pal(9, "OrRd")[5], bg.border = NA,
												 # bg.col = rainbow(34)[1:20]
												 panel.fun = function(x, y){
													 xlim = get.cell.meta.data("xlim")
													 ylim = c(0, 2)
													 sector.index = get.cell.meta.data("sector.index")
													 sector.label = paste(freq2$category[freq2$pseudo == sector.index],
																								"\n(",freq2$freq[freq2$pseudo == sector.index],
																								")", sep = "")
													 circos.text(mean(xlim), y = 1, sector.label, 
																			 facing = "inside", col = "white",
																			 cex = 1.1, font = 2, niceFacing = TRUE)
												 })
	
	# STEP 2.3.1
	# prepare the correlation data frame for the links (association lines) in the circos plot
	# !!! input for pair.cor:
	#  	CASE 1: parameter word not specified
	# 		the correlation rates between all pairs of top 20 words (distinct)
	#			3 columns Word1, Word2, Assocs (correlation)
	# 	CASE 2: parameter word specified
	#			the correlation rates between the specified word and all other words (e$freqchosen)
	# 		3 columns Word1(containing only the specified word), Word2, Assocs (correlation)
	
	# if (!is.null(word)) {
		# cor2 <- pair.cor[pair.cor[,2] %in% freq2[,1] & pair.cor[,3] > min.cor, ]
	# }else{
		# cor2 <- pair.cor[pair.cor[,1] %in% freq2[,1] & pair.cor[,2] %in% freq2[,1] & pair.cor[,3] > min.cor, ]
	# }
	cor2 <- pair.cor[pair.cor[,1] %in% freq2[,1] & pair.cor[,2] %in% freq2[,1] & pair.cor[,3] > min.cor, ]
	# using pseudo to replace the word in column 1 and column 2 in cor2
	for (i in 1:nrow(cor2)){
		cor2[i, 1] <- freq2[1:length(topwords),3][freq2[1:length(topwords),1] == cor2[i, 1]]
		cor2[i, 2] <- freq2[1:length(topwords),3][freq2[1:length(topwords),1] == cor2[i, 2]]
	}
	# print(cor2)
	
	# STEP 2.3.2
	# plot
	for (i in 1:nrow(cor2)){
		min1 <- min(freq2[freq2[, 3] == cor2[i,1], "freq"] )
		max1 <- max(freq2[freq2[, 3] == cor2[i,1], "freq"] )
		min2 <- min(freq2[freq2[, 3] == cor2[i,2], "freq"] )
		max2 <- max(freq2[freq2[, 3] == cor2[i,2], "freq"] )
		l.limit.1 <- (min1+(min1+max1)/2)/2
		l.limit.2 <- (min2+(min2+max2)/2)/2
		u.limit.1 <- (max1+(min1+max1)/2)/2
		u.limit.2 <- (max2+(min2+max2)/2)/2
		pos <- round((nrow(cor2)-i)/nrow(cor2)*6 + 1, 0)
		circos.link(cor2[i, 1], c(l.limit.1, u.limit.1), 
								cor2[i, 2], c(l.limit.2, u.limit.2),
								col = add.alpha(brewer.pal(9,"Reds")[3:9][pos], 0.5))
		
	}
	
	png(paste(if(is.null(word)) NULL else toupper(word), "CircosPlot.png", sep = ""), 1000, 1000, units = "px")
	dev.off()
}



#--------------------------------
# STEP 0 Install necessary packages
req.pcg <- function(pcg){
  # packages to be installed
  tbinst <- pcg[(!(pcg %in% installed.packages()[, "Package"]))|
                  (pcg %in% old.packages()[, "Package"])]  
  if (sum(tbinst %in% c("tmcn", "Rwordseg", "Rweibo"))>0){
    cntm <- tbinst[tbinst %in% c("tmcn", "Rwordseg", "Rweibo")]
    install.packages(cntm, 
                     repos = "http://R-Forge.R-project.org", 
                     type = "source")
  }else if(sum(tbinst == "Rgraphviz")>0){
    source("http://bioconductor.org/biocLite.R")
    biocLite("Rgraphviz")
  }else if (length(tbinst)){
    install.packages(tbinst, dependencies = T)  
  } 
  sapply(pcg, require, warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)  
}

all.pcg <- c("tm", "SnowballC", "qdap", "qdapDictionaries", "dplyr", "fpc", "chron", 
             "RColorBrewer", "ggplot2", "scales", "wordcloud", "igraph", "gridExtra",
             "Rweibo", "Rwordseg", "rJava", "RWeka", "ggdendro", "topicmodels", 
             "circlize", "GISTools")
# rJava is needed for installing and requiring Rwordseg
req.pcg(all.pcg)

# ERROR: compilation failed for package 'tmcn'
# Warning in install.packages : package 'tmcn' is not available (for R version 3.2.0)

#--------------------------------
e <- new.env()
e$wd_rcv <- getwd()

#------------------------------#
# PRIMARY FUNCTION (THE FRAME) #
#------------------------------#
TMV <- function(){
  
  message("| Welcome to use MSU Text Mining & Visualization (TMV) Tool! \n")
  message("| The tool provides BASIC DESCRIPTIVE ANALYSIS, as well as ")
  message("| Simple Mining and Visulizations based on your data. \n")
  cat("| Please prepare your data following rules below: ", 
      "|   1. a *.csv file;", 
      "|   2. with 1 single column;",  
      "|   3. without headline.\n", sep = "\n")
  
  # set wd
  if(substr(e$wd_rcv, nchar(e$wd_rcv) - 6, nchar(e$wd_rcv)) == "Text_WD"){
    e$wd_rcv <- substr(e$wd_rcv, 1, nchar(e$wd_rcv) - 8)
  } else {
    if(!file.exists("Text_WD")) dir.create("Text_WD")
    setwd(paste(e$wd_rcv, "Text_WD", sep = "/"))
  }
  message('| The working directory is set as "', getwd(), '". ')
  message('| If you have not put the data file under "Text_WD" folder, do it now! \n')

  # STEP 1 Input data
  # check ncol == 1
  # df <- na.omit(df)
  e$raw <- TMV.Q(index = 1)
	# output: e$raw
  
	#--------------------------------------------------------------------------------
  # STEP 2 Treatments
  # turn data frame into corpus: raw corpus
  e$rawc <- Corpus(DataframeSource(e$raw))
  
  e$corpus0 <- e$rawc                                    %>% 
    tm_map(content_transformer(tolower))                 %>%
		tm_map(PlainTextDocument)                            %>%
    tm_map(stripWhitespace)                              %>%
    tm_map(removeNumbers)                                %>%
    tm_map(removeWords, stopwords("english")[c(-(81:98), -(165:167))])
  
  # retain "not" meaning words
  # change them all to "no"
  change <- content_transformer(function(x, from, to) 
    gsub(paste(" ", from, " ", sep = ""), 
         paste(" ", to, " ", sep = ""), x))
  for(j in c(81:98, 165:167)) {
    e$corpus0 <- tm_map(e$corpus0, change, from = stopwords("english")[j], to = "not")
  }
  
  e$corpus0 <- e$corpus0              %>% 
    tm_map(removePunctuation)         
  e$corpus0C <- e$corpus0
  e$corpus0 <- e$corpus0              %>% 
		tm_map(stemDocument)
  
	message('| The file is imported and the basic clean-up is done. \n')
	
	# change words into original form
	e$tdm0 <- TermDocumentMatrix(e$corpus0)
	colnames(e$tdm0) <- as.character(1:ncol(e$tdm0))
	e$tdmm0 <- as.matrix(e$tdm0)
	freq0 <- rowSums(e$tdmm0)
	e$freq0 <- freq0[order(freq0, decreasing = TRUE)]
	write.csv(data.frame(Stems = names(e$freq0), 
										 Completions = stemCompletion(names(e$freq0), e$corpus0C, "shortest"), 
										 Frequency = e$freq0, 
										 stringsAsFactors = FALSE), 
					# default method: prevalent, takes the most frequent match as completion
					# risking of changing some words which don't need heuristic completion of stemming
					"stemCompletion.csv", row.names = FALSE)
	repeat{
	  message('| Some words seem strange after stemmed by R. ')
	  opt <- readline("| Would you like to resume some stems to complete words(Y/N)? ")
	  cat("\n")
	  if(!toupper(opt) %in% c("Y", "N")){
	    message('| Only "Y" or "N" is acceptable! \n')
	  } else if(toupper(opt) == "N") {
	    break
  	} else if(toupper(opt) == "Y") {  	  
  	  message('| A *.csv file named "stemCompletion.csv" is exported. ')
  	  message('| Please open it in Excel, check and type the corresponding complete words ')
  	  message('| on the 2nd column. Keep the 1st column words as they are. ')
  	  message("| If you input spaces, they'll be removed in later process. \n")
  	  readline("| Make sure you have saved the changes in the Excel... \n")
  	  
  	  e$inwords <- read.csv("stemCompletion.csv", stringsAsFactors = FALSE)
  	  comwords <- e$inwords
  	  message("| Your type is imported. \n")
  	  # comwords <- apply(e$inwords, c(1, 2), function(x, from, to) gsub(from, to, x), " ", "")
  	  # comwords <- as.data.frame(comwords, row.names = rownames(comwords), stringsAsFactors = FALSE)
  	  comwords[which(comwords[, 2] == ""), 2] <- NA
  	  comwords <- na.omit(comwords)
  	  comwords <- comwords[which(comwords[, 1] != comwords[, 2]), ]
  	  e$comwords <- comwords[order(comwords[[3]], decreasing = TRUE), ]
  	  
  	  message("| ", nrow(e$comwords), " words are chosen to be changed, ")
  	  message("| in decending order are printed below. \n")
  	  print(e$comwords[if(nrow(e$comwords) > 30) 1:30 else 1:nrow(e$comwords), ])
  	  cat("\n")
  	  
  	  # Satisfactory question
  	  opt <- TMV.Q(index = 9)
  	  if(opt == "Y") {
  	    for(i in 1:nrow(e$comwords)) {
  	      e$corpus0 <- e$corpus0                                        %>% 
  	        tm_map(change, from = e$comwords[i, 1], 
  	               to = paste(" ", e$comwords[i, 1], " ", sep = ""))    %>%
  	        tm_map(change, from = paste(" ", e$comwords[i, 1], " ", sep = ""), 
  	               to = e$comwords[i, 2])
  	    }
  	    e$tdm0 <- TermDocumentMatrix(tm_map(e$corpus0, stripWhitespace))
  	    e$tdmm0 <- as.matrix(e$tdm0)
  	    freq0 <- rowSums(e$tdmm0)
  	    e$freq0 <- freq0[order(freq0, decreasing = TRUE)]
  	    break
  	  }
  	}
	}
	
	#--------------------------------------------------------------------------------
  # STEP 3 Descriptive Analysis
  # 1. Text: total number of words and comments, summary(freq)
  # 2. Frequency Plot (1. Top 20 words; 2. quarter quantiles positions)
  # e$tdm0 <- e$tdm0stem
	# colnames(e$tdm0) <- as.character(1:ncol(e$tdm0))
	# rownames(e$tdm0)[which(rownames(e$tdm0) %in% e$comwords[[1]])] <- e$comwords[[2]]
  # wordv0 <- rownames(e$tdm0)
	# e$dtm0 <- DocumentTermMatrix(e$corpus0stem)
  # dim(dtm)
  # inspect(dtm[1:5, 1:5])
  # e$tdmm0 <- as.matrix(e$tdm0)
  # rownames(e$tdmm0) <- as.character(1:nrow(e$tdmm0))
  # df <- aggregate(data.frame(e$tdmm0), by = list(wordv0), sum)
  # tdmdf <- df[, -1]
  # rownames(tdmdf) <- df[[1]]
  # e$tdm0 <- as.TermDocumentMatrix(as.matrix(tdmdf)) 
	# Error in .TermDocumentMatrix(x, weighting) : 
	# argument "weighting" is missing, with no default
  
  message("| The data contains ", ncol(e$tdm0), " pieces of text(document),")
  message("| including ", nrow(e$tdm0), " unique words. \n")
  message("| The statistical summary of words frequency is as follows: ")
  print(summary(e$freq0))
  cat("\n")
  
  # Words Frequency Plot
  e$wf0 <- data.frame(word = names(e$freq0), freq = e$freq0)
  # head(e$wf0)
  # e$wf0[1:20, ]                                       %>%
  #   ggplot(aes(word, freq))                            +
  #   geom_bar(stat="identity")                          +
  #   ggtitle("Word Frequency Top 20")                   +
  #   theme(axis.text.x=element_text(angle=45, hjust=1)) 
  
  e$wf0$word <- factor(e$wf0$word, 
                       ordered = TRUE, 
                       levels = e$wf0[order(e$wf0[,2]), 1])
	# e$wf0 <- e$wf0[order(e$wf0$freq, decreasing = TRUE),]
	
	windowsFonts(Impact=windowsFont("Impact")) # !!! windowsFonts/windowsFont {grDevices} 
	# These functions handle the translation of a device-independent R graphics font family name to a windows font description.
	# windowsFonts(Times=windowsFont("TT Times New Roman"))
	
	# plot word frequency with horizontal lines at mean, 1st quartile and 3rd quartile
  p1 <- ggplot(e$wf0[1:40,], aes(word, freq))          						   						+
    geom_bar(stat = "identity", fill = "orange")                  	 						  +
    ggtitle("Word Frequency - Top 40")           						    								+
    ylab("Frequency")                             						    								+
    xlab("Word")																																	+
		geom_hline(aes(yintercept = mean(e$freq0)), colour = "red")										+
		# geom_hline(aes(yintercept = summary(e$freq0)[[2]]), colour = "grey")					+
		# geom_hline(aes(yintercept = summary(e$freq0)[[5]]), colour = "grey")					+
		annotate("text", 40, mean(e$freq0)+1, size = 3,
						 label = paste("Mean =", round(mean(e$freq0), 2)))														+
		# annotate("text", 20.8, summary(e$freq0)[[2]]+1, size = 3,
		# 				 label = paste("1st Quartile:", summary(e$freq0)[[2]]))							+
		# annotate("text", 20.8, summary(e$freq0)[[5]]+1, size = 3,
		# 				 label = paste("3rd Quartile:", summary(e$freq0)[[5]]))							+
		theme(panel.background = element_rect(fill = "transparent",colour = NA),
					plot.background = element_rect(fill = "transparent",colour = NA),
					panel.grid.minor = element_blank(), 
					panel.grid.major = element_blank(),
					plot.title = element_text(size = 20, family = "Impact"))								+
		coord_flip()                                  						    				
  
  e$p1 <- p1
  # grid.arrange(e$p1, ncol = 1)
	print(e$p1)
	message("| Please check the Words Frequency Plot at the plot zone.\n")
	ggsave("p1_top40_word_freq.png", p1, width = 300, height = 360, units = "mm", bg = "transparent")
	
	#--------------------------------------------------------------------------------
  # STEP 4  Deeper Analysis Preparation
  # warning: e.g. blue oral -> blueoral
  message("| TMV only provides the analytic results for single words. ")
  message('| If you have phrases like "new world" which you would ')
  message('| like to treat them as one word for analysing, please ')
  message('| replace them by "newworld" in data source and restart the program. \n')
  
	# Question 4.1: Add Additional Stopwords
	# input: N - do not add stopwords, c("word1", "word2", ...) - a string of stopwords
	# output: e$stops, character vector, a vector of additional stopwords 
	e$stops <- TMV.Q(index = 2)
  
	if(length(e$stops) == 0){
	  e$tdm1 <- e$tdm0
	  colnames(e$tdm1) <- as.character(1:ncol(e$tdm1))
	  e$tdmm1 <- as.matrix(e$tdm1)
	  freq1 <- rowSums(e$tdmm1)
	  e$freq1 <- freq1[order(freq1, decreasing = TRUE)]
	} else {
	  e$tdm1 <- e$tdm0[ - which(rownames(e$tdm0) %in% e$stops), ]
	  colnames(e$tdm1) <- as.character(1:ncol(e$tdm1))
	  e$tdmm1 <- as.matrix(e$tdm1)
	  freq1 <- rowSums(e$tdmm1)
	  e$freq1 <- freq1[order(freq1, decreasing = TRUE)]
	  message("| The data contains ", ncol(e$tdm1), " pieces of text(document),")
	  message("| including ", nrow(e$tdm1), " unique words. \n")
	  message("| The statistical summary of words frequency is as follows: ")
	  print(summary(e$freq1))
	  cat("\n")
	}
  
  # Q2: Sparsity? (big loop) 
  repeat{
    # Question 4.2: Sparsity?
		# Input: sparsity rate
		# Output: e$spars, numeric vector of length 1
    repeat{
      message("| Sparsity is the parameter which all the following analysis is based on.")
      e$spars <- TMV.Q(index = 3)
      
      e$tdm2 <- removeSparseTerms(e$tdm1, e$spars)
      colnames(e$tdm2) <- as.character(1:ncol(e$tdm2))
      e$tdmm2 <- as.matrix(e$tdm2)
      
      freq2 <- rowSums(e$tdmm2)
      # table(freq)
      e$freq2 <- freq2[order(freq2, decreasing = TRUE)]
      e$wf2 <- data.frame(word = names(e$freq2), freq = e$freq2)
      e$wf2$word <- factor(e$wf2$word, 
                           levels = e$wf2[order(e$wf2[,2]), 1], 
                           ordered = TRUE)
      
      message("| The data contains ", ncol(e$tdm2), " pieces of text(document),")
      message("| including ", nrow(e$tdm2), " unique words. \n")
      message("| The summary for words frequency is as follows: ")
      print(summary(e$freq2))
      cat("\n")
      
      opt <- TMV.Q(index = 9)
      if(toupper(opt) == "Y") break
    }
    
		#--------------------------------------------------------------------------------
    # STEP 5 Visualization
    # remarks: small loops, ask preferred parameters for plots, 
    #          if satisfied, save useful objects for final output
    
		# 1. Frequency Plot: ncol_freq?
    cat(paste("|", paste(rep("*",27), collapse = "")), 
        "|  SECTION 1. FREQUENCY PLOT",
        paste("|", paste(rep("*",27), collapse = "")),
        "", sep = "\n")
    
    repeat{
      # Question 5: a series of questions for visualization parameters in different plot
			# Question 5.1: No. of columns (no. of top words) in word frequency plot
			# Input: No. of columns
			# Output: e$ncol_freq, numeric vector of length 1
			e$ncol_freq <- TMV.Q(index = 4)
			
      p2 <- ggplot(e$wf2[1:e$ncol_freq, ], aes(word, freq))          						   		+
				geom_bar(stat = "identity", fill = "orange")                  	 						  +
				ggtitle(paste("Word Frequency - Top", e$ncol_freq))           						    	+
				ylab("Frequency")                             						    								+
				xlab("Word")																																	+
        geom_hline(aes(yintercept = mean(e$freq2)), colour = "red")										+
        annotate("text", e$ncol_freq, mean(e$freq2)+1, size = 3,
                 label = paste("Mean =", round(mean(e$freq2), 2)))														+
        theme(panel.background = element_rect(fill = "transparent",colour = NA),
							plot.background = element_rect(fill = "transparent",colour = NA),
							panel.grid.minor = element_blank(), 
							panel.grid.major = element_blank(),
							plot.title = element_text(size = 20, family = "Impact"))								+
				coord_flip()     
			e$p2 <- p2
			grid.arrange(e$p1, e$p2, ncol = 2)
			ggsave(paste("p2_top", e$ncol_freq, "_word_freq.png", sep = ""), 
			       e$p2, width = 8, height = 12, 
			       units = "in", bg = "transparent")
			
			message("| Please check the Words Frequency Plot at the plot zone.\n")
			
			# index = 9: always satisfactory question
      opt <- TMV.Q(index = 9)
			
      if(toupper(opt) == "Y") {
        e$min.freqh <- e$wf2$freq[e$ncol_freq]
        message("| The chart of Top ", e$ncol_freq, " word frequency is plotted, ")
        message("| in which the minimum frequency equals to ", e$min.freqh, ". \n")
        break
      }
    }
    
    # 2. Word Cloud: min.freq? defualt: rot.per=.3, random.order=F
    cat(paste("|", paste(rep("*",23), collapse = "")), 
        "|  SECTION 2. WORD CLOUD",
        paste("|", paste(rep("*",23), collapse = "")),
        "",sep = "\n")
    
    repeat{
			# Question 5.2: Minimum frequency for a word to get into the wordcloud
			# Input: No. of minimum frequency
			# Output: e$min_freq_wc, numeric vector of length 1
      e$min.freqc <- TMV.Q(index = 5)
			
      set.seed(123)
      wordcloud(e$wf2$word, e$wf2$freq, min.freq = e$min.freqc, rot.per = .3, 
                random.order = FALSE, colors = brewer.pal(6, "Dark2"))
      dev.copy(png, "WordCloud.png", width = 1000, height = 1000, units = "px")
      dev.off()
      message("| ", e$min.freqc, " is set as the minimum frequency for the wordcloud.\n")
      opt <- TMV.Q(index = 9)
      if(toupper(opt) == "Y") break
    }
    
    # 3. Association Plot(output: pdf): lowfreq? corThreshold? 
    #    Correlation output .csv
    cat(paste("|", paste(rep("*",29), collapse = "")), 
        "|  SECTION 3. ASSOCIATION PLOT",
        paste("|", paste(rep("*",29), collapse = "")),
        "", sep = "\n")
    
		max.freq <- max(e$freq2)
		cor_pairs <- data.frame()
		for(i in 1:length(e$freq2)){
		  cor1 <- findAssocs(e$tdm2, names(e$freq2)[i], corlimit = 0)
			if (class(cor1)=="matrix"){
				cor_pair <- data.frame(Word1 = names(e$freq2)[i], 
															 Word2 = rownames(cor1), 
															 Assocs = cor1[, 1], 
															 row.names = NULL, 
															 stringsAsFactors = FALSE)
				cor_pairs <- rbind(cor_pairs, cor_pair)
			}else next
		}
		e$cor_pairs <- cor_pairs <- distinct(cor_pairs)
		write.csv(cor_pairs, "CorrelationPairs.csv", row.names = F)
		max.cor <- max(cor_pairs[[3]])
		# max.cor <- check.cor(t(e$tdmm2))
		message("| The summary of word Frequency: ")
		print(summary(e$freq2))
		message("\n| The summary of word Association: ")
		print(summary(cor_pairs[[3]]))
		cat("\n")
		
		cat("| Per below you could find a circos plot of top20 words and their association.\n")
		repeat{
			repeat{
				min.assc <- readline("| Please enter the minimum correlation rate you want to check: ")
				if(!all(strsplit(min.assc, split = "")[[1]] %in% c(as.character(0:9),"."))) {
					message("| Please do enter a positive decimal between 0 and 1 (excl.)!\n")
				}else if(as.numeric(min.assc) >= 1 |as.numeric(min.assc) <= 0){
					message("| Please do enter a positive decimal between 0 and 1 (excl.)!\n")
				}else {
					min.assc <- as.numeric(min.assc)
					break
				}
			}
			
			circos.plot(as.data.frame(t(e$tdmm2)), e$cor_pairs, min.assc)
			message('| Please look to the plot zone for the Circos Plot. ')
			message('| "CircosPlot.png" is exported. \n')
			opt <- TMV.Q(index = 9)
			if(toupper(opt) == "Y") break
		}
		
		readline("| You could find more detailed stats and graphs in the following steps.\n| Press <Enter> to continue...\n")
		
		repeat{
			# Question 5.3: Minimum frequency for a word to get into the association plot
			# Input: 1. min frequency for a variable to get into the association plot
			# 			 2. min collection for a variable to get into the association plot
			# Output: e$low_freq, numeric vector of length 1; e$cor_thres, numeric vector of length 1
			# Parameters: 1. max.freq: maximum frequency of a word in the dtm
			#							2. max.cor: maximum correlation between words in the dtm
			ans5_3 <- TMV.Q(index = 7, max.freq = max.freq, max.cor = max.cor)
			e$min.freqa <- ans5_3[1]
			e$corth <- ans5_3[2]
			
			attrs <- list(node = list(shape = "rectangle", fixedsize = FALSE,
																style = "invis", fontcolor = "black", fillcolor = "orange"),
										edge = list(dir = "both", color = "grey", weight = 1.2)) 
			plot(e$tdm2,
			     terms = findFreqTerms(e$tdm2, lowfreq = e$min.freqa),
			     corThreshold = e$corth,
			     attrs = attrs, 
			     weighting = TRUE)
			dev.copy(pdf, "AssociationPlot.pdf", 9, 9)
			dev.off()
			message('| Please look to the plot zone for the Association Plot. ')
			message('| "AssociationPlot.pdf" is exported. \n')
			opt <- TMV.Q(index = 9)
			if(toupper(opt) == "Y") break
		}
		
		repeat{
		  message('| You can specify a single word to see its relationship with others. ')
		  message('| Be sure the word you specified is in the following word list: ')
		  print(unique(e$cor_pairs[[1]]))
		  cat("\n")
		  opt <- readline('| Please specify the word you are interested in: ')
		  if(!tolower(opt) %in% unique(e$cor_pairs[[1]])){
		    message("| The word you specified doesn't exist in the current data! ")
		  } else {
		    word <- tolower(opt)
		    word_cor <- findAssocs(e$tdm2, word, corlimit = 0)
				e$cor_pairs2 <- data.frame(Word1 = word, 
																	 Word2 = rownames(word_cor), 
																	 Assocs = word_cor[, 1], 
																	 row.names = NULL, 
																	 stringsAsFactors = FALSE)
				circos.plot(as.data.frame(t(e$tdmm2)), e$cor_pairs2, 0, word)
				
				readline("| Press <Enter> to check the next plot...")
				
		    message("| Following are the Correlated Words and their Associations. ")
		    print(word_cor[1:(if(nrow(word_cor) > 20) 20 else nrow(word_cor)), ])
		    cat("\n")
		    plot(e$tdm2,
		         terms = c(word, rownames(word_cor)[1:(if(nrow(word_cor) > 20) 20 else nrow(word_cor))]),
		         corThreshold = 0,
		         attrs = attrs, 
		         weighting = TRUE)
				dev.copy(pdf, paste(word, "AssociationPlot.pdf", sep = "_"), 9, 9)
				dev.off()
		    message("| Please look to the plot zone for ", word, "'s Association Plot. \n")
				message(paste('| "',word, '_AssociationPlot.pdf" is exported. \n', sep = ""))
		    opt <- TMV.Q(index = 9)
		    if(toupper(opt) == "Y") break
		  }
		}
		
		readline("| The single word association plot in circos plot format is also plotted.\n| Press <Enter> to continue...\n")
		
		
    
    # 4.0 hclust
    # Cluster Dendrogram:
		cat(paste("|", paste(rep("*",37), collapse = "")), 
		    "|  SECTION 4. TERMS CLUSTER DENDROGRAM",
		    paste("|", paste(rep("*",37), collapse = "")),
		    "",sep = "\n")
    DistMat <- dist(scale(e$tdmm2))
    fit <- hclust(DistMat, method = "ward.D") 
    # method = "ward.D", "ward.D2", "single", "complete", "average"...
    repeat{
      plot(fit)
      dev.copy(png, "dendrogram.png", width = 1000, height = 1000, units = "px")
			dev.off()
      # ggdendrogram(dendro_data(fit))
      
      # 4.1 rect.hclust: k?
      # cut tree into k clusters
		
			# Question 5.4: No. of Clusters
			# Input: No. of clusters
			# Output: e$no.clust, numeric vector of length 1
			e$clustterm <- TMV.Q(index = 8)
			message("| The main ", e$clustterm, " clusters are plotted in red rectangles.")
			rect.hclust(fit, k = e$clustterm)
			dev.copy(png, "rect.dendrogram.png", width = 1000, height = 1000, units = "px")
			dev.off()
			# rect.hclust(tree, k = NULL, which = NULL, x = NULL, h = NULL,
			#             border = 2, cluster = NULL)
			
			opt <- TMV.Q(index = 9)
			if(toupper(opt) == "Y") break
		}
		
    
    # 5 kmeans: cluster by documents
    cat(paste("|", paste(rep("*",31), collapse = "")), 
        "|  SECTION 5. K-MEANS CLUSTERING",
        paste("|", paste(rep("*",31), collapse = "")),
        "",sep = "\n")
    repeat{
      dtmm2 <- t(e$tdmm2)
      e$clustdoc <- TMV.Q(index = 8)
      set.seed(122)
      kmeansResult <- kmeans(dtmm2, centers = e$clustdoc)
			centers <- kmeansResult$centers
      # round(kmeansResult$centers, digits = 3) # cluster centers
			segs <- data.frame(matrix(, nrow = 10, ncol = e$clustdoc))
			for(i in 1:e$clustdoc){
				segs[[i]] <- colnames(centers)[order(centers[i, ], decreasing = T)[1:10]]
			}
			colnames(segs) <- paste("Cluster_", 1:e$clustdoc, sep = "")
			write.csv(segs, "kmeans_Top10_Impactors.csv", row.names = F)
      write.csv(data.frame(size = kmeansResult$size, kmeansResult$centers), 
                "kmeans_centers.csv")
      # write.csv(data.frame(document = names(kmeansResult$cluster), 
      #                      cluster = kmeansResult$cluster), 
      #           "kmeans_cluster.csv", row.names = FALSE)
      message('| "kmeans_centers.csv" and "kmeans_top10_impactors.csv" are exported. \n')
      
      message("| The documents clusters and the key words are as following: ")
      for (i in 1:e$clustdoc) {
        s <- sort(kmeansResult$centers[i, ], decreasing = T)
        cat(paste("| Cluster ", i, ": ", sep = ""), paste(names(s)[1:5], sep = ", "))
        cat("\n")
        # print the tweets of every cluster
        # print(tweets[which(kmeansResult$cluster==i)])
      }
      cat("\n")
      
      opt <- TMV.Q(index = 9)
      if(toupper(opt) == "Y") break
    }
    
    # 4.2 clust: automatic k?
    # library(fpc)
    # partitioning around medoids with estimation of number of clusters
    # pamResult <- pamk(dtmm2, metric = "manhattan")
    # k <- pamResult$nc # number of clusters identified
    # pamResult <- pamResult$pamobject
    # print cluster medoids
    # for (i in 1:k) {
    # cat("cluster", i, ": ",
    #     colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
    # }
    # plot clustering result
    # layout(matrix(c(1, 2), 1, 2)) # set to two graphs per page
    # plot(pamResult, col.p = pamResult$clustering)
    
    # 6 Topic Modeling
    cat(paste("|", paste(rep("*",31), collapse = "")), 
        "|    SECTION 6. TOPIC MODELING",
        paste("|", paste(rep("*",31), collapse = "")),
        "",sep = "\n")
    dtm2 <- as.DocumentTermMatrix(e$tdm2)
    dtm2 <- dtm2[-which(rowSums(as.matrix(dtm2))==0), ]
    repeat{
      # library(topicmodels)
      ans6_1 <- TMV.Q(index = 10)
      e$topicn <- ans6_1[1]
      e$termn <- ans6_1[2]
      lda <- LDA(dtm2, k = e$topicn) # find 8 topics
      term <- terms(lda, e$termn) # first 4 terms of every topic
      print(term)
			write.csv(term, "topics.csv", row.names = F)
      write.csv(data.frame(Document = names(topics(lda)), 
                           Topic = topics(lda)), 
                "document_topics_segment.csv", row.names = FALSE)
      message('\n| "document_topics.csv" is exported. \n')
      # term <- apply(term, MARGIN = 2, paste, collapse = ", ")
      # topic <- topics(lda, 1)
      # topics <- data.frame(date=as.IDate(tweets.df$created), topic)
      # qplot(date, ..count.., data=topics, geom="density",
      #       fill=term[topic], position="stack")
      
      opt <- TMV.Q(index = 9)
      if(toupper(opt) == "Y") break
    }
    
    # End Sparsity Big Loop
    repeat{
      opt <- readline("| Are you satisfied with all the output based on the specified Sparsity (Y/N)? ")
      cat("\n")
      if(!toupper(opt) %in% c("Y", "N")){
        message("| Only Y or N is acceptable! \n")
      } else break
    }
    if(toupper(opt) == "Y") break
		
		# dev.off()
  }
  
	
  # STEP 6 final output
  message("| Thank you for using MSU TMV tool! Hope to see you again! Bye~ \n")
  setwd(e$wd_rcv)
}


# findAssocs(dtm, "not", corlimit = 0.3)

#------------------------------------
# Compare Among Several Text Source
# Word Cloud
# df <- read.csv("FO_Compare.csv", head = FALSE)
# df <- read.csv("FO_Compare2.csv", head = FALSE)
# df <- sapply(df, as.character)
# df <- df[, -1]
# sub_cont <- Corpus(DataframeSource(df))
# tdm6 <- as.matrix(tdm)
# tdm6 <- tdm6[!rownames(tdm6) %in% c("new", "vehicl"), ]
# colnames(tdm6) <- c("Drop1", "Drop2", "Increase1", "Increase2", "Same1", "Same2")
# colnames(tdm6) <- c("Drop2", "Increase2", "Same2")
# comparison.cloud(tdm6, random.order = F, max.words = Inf, title.size = 1.5)
# commonality.cloud(tdm6, random.order=FALSE,
#                    colors = brewer.pal(8, "Dark2"),
#                    title.size=1.5)