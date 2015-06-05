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

all.pcg <- c("tm", "SnowballC", "qdap", "qdapDictionaries", "dplyr", 
             "RColorBrewer", "ggplot2", "scales", "wordcloud", "igraph",
             "Rweibo", "Rwordseg", "rJava", "RWeka", "ggdendro")
# rJava is needed for installing and requiring Rwordseg
req.pcg(all.pcg)

# ERROR: compilation failed for package 'tmcn'
# Warning in install.packages : package 'tmcn' is not available (for R version 3.2.0)

#--------------------------------
e <- new.env()
e$wd_recover <- getwd()
# e$raw      cleaned data frame after input with 1 column and no NA
# e$rawc     e$raw turned to corpus
# e$corpus0  after routine primary treatments
# e$corpus1  after removing stopwords which user specified
# e$corpus2  after Sparsity specified and used


TMV <- function(){
  
  message("| Welcome to MSU Text Mining & Visualization (TMV) Tool! \n")
  message("| The tool will provide you Basic Descriptive Analysis, as well as ")
  message("| Simple Mining and Visulizations based on your data. \n")
  cat("| Please prepare your data as: ", 
      "|   1. a *.csv file;", 
      "|   2. with 1 single column;",  
      "|   3. without headline.", "", sep = "\n")
  
  # set wd
  if(!file.exists("Text_WD"))dir.create("Text_WD")
  setwd(paste(e$wd_recover, "Text_WD", sep = "/"))
  message('| The working directory is set as "Text_WD" under your original WD. ')
  message('| If you have not put the data file under "Text_WD" file, do it now! ')

  # STEP 1 Input data
  # check ncol == 1
  # df <- na.omit(df)
  repeat{
    raw.name <- readline("| Please enter the data file name: ")
    cat("\n")
    endstr <- substr(raw.name, nchar(raw.name)-3, nchar(raw.name))
    if(!endstr == ".csv"){
      message('| Only "*.csv" file is acceptable! \n')
    } else {
      raw <- read.csv(raw.name, header = FALSE, stringsAsFactors = FALSE)
      raw <- na.omit(raw)
      if(!ncol(raw)==1){
        message("| Please confirm that your data has 1 single column! \n")
      } else {
        e$raw <- raw
        break
      }
    }
  }
  
  # STEP 2 Treatments
  # turn data frame into corpus: raw corpus
  e$rawc <- Corpus(VectorSource(e$raw))
  
  e$corpus0 <- e$rawc %>% 
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english")[c(-(81:98), -(165:167))])
  # retain "not" meaning words
  # change them all to "no"
  change <- content_transformer(function(x, from, to) gsub(from, to, x))
  for(j in c(81:98, 166:167)) {
    e$corpus0 <- tm_map(e$corpus, change, stopwords("english")[j], "no")
  }
  
  e$corpus0 <- e$corpus0 %>% 
    tm_map(removePunctuation) %>% 
    tm_map(stripWhitespace) %>%
    tm_map(stemDocument) %>%
    tm_map(removeNumbers)
  
  # STEP 3 Descriptive Analysis
  # 1. Text: total words number, number of comments, summary(freq)
  # 2. Frequency Plot (1. Top 20 words; 2. quarter quantiles positions)
  e$dtm0 <- DocumentTermMatrix(e$corpus0)
  # e$tdm0 <- TermDocumentMatrix(e$corpus0)
  # dim(dtm)
  # inspect(dtm[1:5, 1:5])
  freq0 <- colSums(as.matrix(e$dtm0))
  # length(freq)
  ord0 <- order(freq0, decreasing = TRUE)
  # table(freq)
  e$freq0 <- freq0[ord0]
  
  message("| You have input ", nrow(e$raw), " pieces of paragraph, ")
  message("| including ", length(freq0), " unique words. \n")
  message("| The distribution of words frequency is as follows. ")
  print(summary(e$freq0))
  
  # Histogram of Frequency
  message("| Please look to right for the Histogram of Words Frequency. \n")
  
  e$wf0 <- data.frame(words=names(e$freq0), freq=e$freq0)
  # head(e$wf0)
  # e$wf0[1:20, ]                                       %>%
  #   ggplot(aes(word, freq))                            +
  #   geom_bar(stat="identity")                          +
  #   ggtitle("Word Frequency Top 20")                   +
  #   theme(axis.text.x=element_text(angle=45, hjust=1)) 
  
  e$wf0$word <- factor(e$wf0$word, 
                       levels = e$wf0[order(e$wf0[,2], decreasing = FALSE), 1], 
                       ordered=T)  
  ggplot(e$wf0[1:20, ],aes(word, freq))               +
    geom_bar(stat = "identity")                       +
    coord_flip()                                      +
    ggtitle("Word Frequency Top 20")                  +
    ylab("Frequency")                                 +
    xlab("Word")
  
  # quarter quantiles positions?  geom_vline()?
  
  # png("Dendrogram_db.png", width=12, height=8, units="in", res=300)
  
  
  # STEP 4  Deeper Analysis Preparation
  # warning: e.g. blue oral -> blueoral
  message("| TMV Tool can only get the analytic results for single words. ")
  message('| So if you have phrases like "new world" which you would ')
  message('| like to treat them as one word when analysing, please ')
  message('| replace them with "newworld" in Excel and restart the tool. \n')
  
  # Q1: Additional stopwords? 
  repeat{
    repeat{
      message("| Do you have any Additional Stopwords? ")
      # need to output words whose freq == 1 as reference? 
      message("| If none, just press <Enter> without typing any letters. ")
      message("| If any, please type them below in the type they look now. ") # words stems
      stps <- readline('| Please specify (use comma "," to split words): ')
      cat("\n")
      if(!all(strsplit(tolower(stps), "") %in% c(" ", ",", "", letters))){
        message("| Unacceptable character is entered! Please type again! \n")
      } else break
    }
    stops <- strsplit(gsub(" ", "", tolower(stps)), ",")[[1]]
    if(length(stops) == 0) {
      message("| You don't need any additional stopwords. ")
    } else {
      message("| The new stopword(s) that you specified: ")
      cat(stops, sep = ", ")
    }
    repeat{
      opt <- readline("| Could you confirm (Y/N)? ")
      if(!toupper(opt) %in% c("Y", "N")){
        message('| Only "Y" or "N" is acceptable! ')
      } else break
    }
    if(toupper(opt) == "Y") {
        e$stops <- stops
        break
    }
  }
  e$corpus1 <- tm_map(e$corpus0, removeWords, e$stops)
  e$dtm1 <- DocumentTermMatrix(e$corpus1)
  freq1 <- colSums(as.matrix(e$dtm1))
  ord1 <- order(freq1, decreasing = TRUE)
  # table(freq)
  e$freq1 <- freq1[ord1]
  e$wf1 <- data.frame(words = names(e$freq1), freq = e$freq1)

  
  # Q2: Sparsity? (big loop) 
  repeat{
    repeat{
      spst <- readline("| How much would you like to be the sparsity? ")
      if(!all(strplit(tolower(e$spst))[[1]] %in% c(as.character(0:9), "."))
         |sum(strplit(tolower(e$spst))[[1]] == ".") > 1){
        message("| Only numbers and one decimal is acceptable! \n")
      } else break
    }
    e$spst <- as.numeric(spst)
    
    # STEP 5 Visualization
    # remarks: small loops, ask preferred parameters for plots, 
    #          if satisfied, save useful objects for final output
    # 1. Frequency Plot: ncol?
    repeat{
      repeat{
        ncol <- readline("| How many words are you preferred for the frequency histogram? ")
        if(!all(strsplit(ncol)[[1]] %in% as.character(0:9))){
          message("| Only integer is acceptable! \n")
        } else break
      }
      ncol <- as.numeric(ncol)
      e$wf1$word <- factor(e$wf1$word, 
                           levels = e$wf1[order(e$wf1[,2], decreasing = FALSE), 1], 
                           ordered=T)
      ggplot(e$wf1[1:ncol, ],aes(word, freq))                +
        geom_bar(stat = "identity")                          +
        coord_flip()                                         +
        ggtitle(paste("Word Frequency Top", ncol, sep = " ") +
        ylab("Frequency")                                    +
        xlab("Word")
      
      message("| Please look to right for the Histogram of Words Frequency. \n")
      repeat{
        opt <- readline("| Are you satisfied with the number(Y/N)?")
        
      }
    }

    
    # 2. Word Cloud: min.freq? defualt: rot.per=.3, random.order=F
    # 3. Association Plot(output: pdf): lowfreq? corThreshold? 
    #    Correlation output csv
    # 4.0 hclust
    # 4.1 rect.hclust: k?
    # 4.2 clust: automatic k?
    # 5 Topic Modeling
      
    # change words into original form
    repeat{
      opt <- readline("| Would you like to resume some stems to complete words(Y/N)? ")
      if(!toupper(opt) %in% c("Y", "N")){
        message('| Only "Y" or "N" is acceptable! ')
      } else if(toupper(opt) == "N") {
        break
      } else if(toupper(opt) == "Y") {
        e$outwords <- data.frame(stem = names(e$freq1), completeword = rep(NA, length(e$freq1)), 
                                 stringsAsFactors = FALSE)
        write.csv(e$comwords, "CompleteWords.csv", row.names = FALSE)
        
        message('| A *.csv file named "CompleteWords.csv" is exported to file "TMV_WD". ')
        message('| Please open it in Excel and type the corresponding complete words ')
        message('| on the 2nd column. Keep the blank if no changes needed for the word. ')
        message('| No phrases are acceptable! ')
        readline("| Make sure you have saved the changes... \n")
        
        e$inwords <- read.csv("CompleteWords.csv", stringsAsFactors = FALSE)
        message("| Your type is imported. \n")
        e$comwords <- apply(e$inwords, c(1, 2), change, " ", "")
        e$comwords[which(e$comwords[[2]] == ""), 2] <- NA
        e$comwords <- na.omit(e$comwords)
        
        message("| The word pairs to be changed are printed below. ")
        print(e$comwords)
        
        repeat{
          opt <- readline("| Is this what you want (Y/N)? ")
          if(!toupper(opt) %in% c("Y", "N")){
            message('| Only "Y" or "N" is acceptable! ')
          } else break
        }
        
        if(toupper(opt) == "Y"){
          for(k in 1:nrow(e$comwords))){
            e$corpus2 <- tm_map(e$corpus1, change, e$comwords[k, 1], e$comwords[k, 2])
          }
          e$dtm2 <- DocumentTermMatrix(e$corpus2)
          freq2 <- colSums(as.matrix(e$dtm2))
          ord2 <- order(freq2, decreasing = TRUE)
          # table(freq)
          e$freq2 <- freq2[ord2]
          e$wf2 <- data.frame(words = names(e$freq2), freq = e$freq2)
          e$wf2$word <- factor(e$wf2$word, 
                               levels = e$wf2[order(e$wf2[,2], decreasing = FALSE), 1], 
                               ordered=T)  
          ggplot(e$wf2[1:20, ],aes(word, freq))               +
            geom_bar(stat = "identity")                       +
            coord_flip()                                      +
            ggtitle("Word Frequency Top 20")                  +
            ylab("Frequency")                                 +
            xlab("Word")
        }
        break
      }
    }
    
  
  
  
  }
  
  # STEP 6 final output
  
  message("| Thank you for using MSU TMV tool! Hope to see you again! Bye~ \n")
  setwd(e$wd_recover)
}






set.seed(123)
wordcloud(names(freq), freq, min.freq = 4, scale = c(5, .8), 
          random.order = FALSE, colors=brewer.pal(6, "Dark2"))

# Association plot
Attrs <- list(node = list(shape = "ellipse", fixedsize = FALSE,
                          style = "invis", fontcolor = "white",
                          fillcolor = "red"),
                          edge = list(dir = "both", color = "darkblue", weight = 1.2))
plot(dtm,
     terms = findFreqTerms(dtm, lowfreq = 4),
     corThreshold = 0.2,
     attrs = Attrs, 
     weighting = TRUE)  

# Cluster Dendrogram:
# DistMat <- dist(scale(as.matrix(tdm)))
DistMat <- dist(scale(as.matrix(tdm)[order(rowSums(as.matrix(tdm)), decreasing = TRUE), ][1:35, ]))
fit <- hclust(DistMat) 
# method = "ward.D", "ward.D2", "single", "complete", "average"...
plot(fit)
ggdendrogram(fit)
# cut tree into k clusters
rect.hclust(fit, k = 6)
# rect.hclust(tree, k = NULL, which = NULL, x = NULL, h = NULL,
#             border = 2, cluster = NULL)

# kmeans

# findAssocs(dtm, "not", corlimit = 0.3)

#------------------------------------
# Compare Among Several Text Source
# Word Cloud
#
# df <- read.csv("FO_Compare.csv", head = FALSE)
# df <- read.csv("FO_Compare2.csv", head = FALSE)
# 
# df <- sapply(df, as.character)
# df <- df[, -1]
# sub_cont <- Corpus(DataframeSource(df))
# 
# tdm6 <- as.matrix(tdm)
# tdm6 <- tdm6[!rownames(tdm6) %in% c("new", "vehicl"), ]
# 
# colnames(tdm6) <- c("Drop1", "Drop2", "Increase1", "Increase2", "Same1", "Same2")
# 
# colnames(tdm6) <- c("Drop2", "Increase2", "Same2")
# 
# comparison.cloud(tdm6, random.order = F, max.words = Inf, title.size = 1.5)
# 
# commonality.cloud(tdm6, random.order=FALSE,
#                    colors = brewer.pal(8, "Dark2"),
#                    title.size=1.5)