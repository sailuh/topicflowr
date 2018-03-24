##################### TF-IDF Filtering Functions ############ #############

#' Print Terms TF-IDF
#'
#' Method was replicated from topicmodels publication on the
#' Journal of Statistical Software by Grun and Hornik, 2011.
#' This function prints the terms tf-idf values in quartiles.
#' The rule of thumb used in the journal is to use the value immediately before the mean when
#' deciding the filtering threshold for \code{\link{FilterDFMByTFIDF}}.
#'
#' @param dfm_quanteda A Document Frequency Matrix as defined in the quanteda package.
#'
#' @return Prints the terms TF-IDF on the console.
PrintTermTFIDF <- function(dfm_quanteda){
  dfm_tm <- convert(dfm_quanteda,to="tm")
  #print(summary(col_sums(dfm_tm)))
  term_tfidf <- tapply(dfm_tm$v/row_sums(dfm_tm)[dfm_tm$i], dfm_tm$j, mean) * log2(nrow(dfm_tm)/col_sums(dfm_tm > 0))
  print(summary(term_tfidf))
}

#' Plot Term TF-IDF
#'
#' Method was replicated from topicmodels publication on the
#' Journal of Statistical Software by Grun and Hornik, 2011.
#' For more details see \code{\link{PrintTermTFIDF}}
#'
#' @param dfm_quanteda A Document Frequency Matrix as defined in the quanteda package.
#' @param filter.by.term If TRUE n.terms.threshold will be used.
#' @param n.terms.threshold Specify a limit to the number of terms plotted.
#'
#' @return A plot of the terms TF-IDF on the console.
PlotTermTFIDF <- function(dfm_quanteda,filter.by.term=FALSE,n.terms.threshold=5000){
  dfm_tm <- convert(dfm_quanteda,to="tm")
  #print(summary(col_sums(dfm_tm)))
  term_tfidf <- tapply(dfm_tm$v/row_sums(dfm_tm)[dfm_tm$i], dfm_tm$j, mean) * log2(nrow(dfm_tm)/col_sums(dfm_tm > 0))

  term.tfidf.sort <- sort(term_tfidf,decreasing = TRUE)


  plot.table <- data.table(n.terms=1:length(term.tfidf.sort),tfidf=term.tfidf.sort)
  if(filter.by.term==TRUE) plot.table <- plot.table[n.terms < n.terms.threshold]
  p <- ggplot(data=plot.table, aes(x=tfidf, y=n.terms)) + geom_line() + theme_minimal()
  return(p)
}

#' List Term TF-IDF
#'
#' Method was replicated from topicmodels publication on the
#' Journal of Statistical Software by Grun and Hornik, 2011.
#' For more details see \code{\link{PrintTermTFIDF}}
#'
#' @param dfm_quanteda A Document Frequency Matrix as defined in the quanteda package.
#'
#' @return A list of the terms TF-IDF on the console.
ListTermTFIDF <- function(dfm_quanteda){
  dfm_tm <- convert(dfm_quanteda,to="tm")
  #print(summary(col_sums(dfm_tm)))
  term_tfidf <- tapply(dfm_tm$v/row_sums(dfm_tm)[dfm_tm$i], dfm_tm$j, mean) * log2(nrow(dfm_tm)/col_sums(dfm_tm > 0))

  term.tfidf.order <- order(term_tfidf,decreasing = TRUE)
  top.terms <- dfm_tm$dimnames$Terms[term.tfidf.order]
  top.terms.score <- term_tfidf[term.tfidf.order]
  top.terms <- data.table(term=top.terms,tfidf=top.terms.score)

  return(top.terms)
}


FilterDFMByTFIDF <- function(dfm_quanteda,threshold){
  # The output is a dfm as defined by (tm) package. TODO: Figure out how to do it on quanteda.
  # Method was replicated from topicmodels publication on the Journal of Statistical Software by Grun and Hornik, 2011.
  dfm_tm <- convert(dfm_quanteda,to="tm")
  term_tfidf <- tapply(dfm_tm$v/row_sums(dfm_tm)[dfm_tm$i], dfm_tm$j, mean) * log2(nrow(dfm_tm)/col_sums(dfm_tm > 0))
  dfm_tm <- dfm_tm[, term_tfidf >= threshold]
  dfm_tm <- dfm_tm[row_sums(dfm_tm) > 0,]
  return(dfm_tm)
}


############# LDA Model Fitting Functions ##################

#' Calculate LDA Models up to N
#'
#' Returns a list, where every position is a created model. List starts on k=2.
#' For more control on the values of k, see \code{\link{CalculateLDAModelsInKSet}}
#' @param dfm A document frequency matrix
#' @param N The number of topics (i.e. 1:N).
#'
#' @return A list of models from 1 to N specified on param N.
CalculateLDAModelsUpToN <-function(dfm,N){
  if(class(dfm)[1] == "dfmSparse"){ #Quanteda package, needs conversion to topicmodels
    dfm <- convert(dfm, to = "topicmodels")
  }
  lda.vem <- list()
  for(i in 1:(N-1)){
    print(paste0("Creating a model for k=",i+1," topics...",sep=" "))
    lda.vem[[i]] <- LDA(dfm, k = i+1)
  }
  return(lda.vem)
}


CalculateLDAModelsInKSetOld <-function(dfm,Ks,method="VEM"){
  #
  if(class(dfm)[1] == "dfmSparse"){ #Quanteda package, needs conversion to topicmodels
    dfm <- convert(dfm, to = "topicmodels") # Note this conversion leads to loss of the document ids
  }
  lda.vem <- list()
  Ks.size <- length(Ks)
  for(i in 1:Ks.size){
    print(paste0("Creating a model for k=",Ks[i]," topics...",sep=" "))
    lda.vem[[i]] <- LDA(dfm, k = Ks[i],method=method)
  }
  return(lda.vem)
}

#' Create Topic Models from 1 to k
#'
#' Returns a list, where every position is a created model. List of k values must be provided as a vector.
#'
#' @param folder Folder variable containing all raw data. See \code{\link{loadFiles}}.
#' @param Ks A vector of integers (e.g. c(1,3,2,7)) specifying the number of topics to be created
#' @param months A character vector containing the months of interest (currently one uses the first month)
#'
#'
#' @return A list of the models for all chosen values in Ks
CalculateLDAModelsInKSet <-function(folder,Ks=2:20,months,method="VEM"){

  #Subset in the folder containing all e-mail replies, the months of interest (leverages the fact the Month name is part of the file name)
  month <- months[1]
  is.document.from.month <- grepl(month,folder$doc_id)
  folder.month <- folder[is.document.from.month,]

  # Every e-mail reply is a document
  corpus <- corpus(x=folder.month)

  #2008_Feb_223.txt 2008_Feb_227.txt 2008_Feb_300.txt
  #0                0                0


  # Tokenize. Several assumptions made here on pre-processing.
  tokens <- tokens(corpus, what = "word", remove_numbers = FALSE, remove_punct = TRUE,
                   remove_symbols = TRUE, remove_separators = TRUE,
                   remove_twitter = FALSE, remove_hyphens = FALSE, remove_url = TRUE)
  tokens <- tokens_tolower(tokens)
  tokens <- removeFeatures(tokens, stopwords("english"))

  # Filter Empty Documents
  tokens.length <- sapply(tokens,length)
  tokens <- tokens[!tokens.length == 0]

  # DFM
  dfm <- dfm(tokens)

  # DFM filter for tokens with nchar > 2 only
  dfm <-dfm_select(dfm,min_nchar=2,selection="remove")


  #
  if(class(dfm)[1] == "dfmSparse"){ #Quanteda package, needs conversion to topicmodels
    dfm <- convert(dfm, to = "topicmodels") # Note this conversion leads to loss of the document ids
  }
  lda.vem <- list()
  Ks.size <- length(Ks)
  for(i in 1:Ks.size){
    print(paste0("Creating a model for k=",Ks[i]," topics...",sep=" "))
    lda.vem[[i]] <- LDA(dfm, k = Ks[i],method=method)
  }
  return(lda.vem)
}




############ Parameter Selection Plot Functions #################

#' Choose K through Perplexity
#'
#'This function attemps to quantify our visual perception of the elbow method to pick k, the number of topics.
#'
#' @param lda.model.list A list of topic models. See \code{\link{CalculateLDAModelsUpToN}}.
#'
#' @return a single integer indicating the number of topics
ChooseKLDAModelsPerplexity <- function(lda.model.list,topic.size.median.threshold){
  n_topics <- length(lda.model.list)
  perplexities <- sapply(lda.model.list,perplexity)

  # As the number of topics increases, the number of documents per topic decreases.
  median_of_n_documents_per_lda_model <- sapply(sapply(lda.model.list,GetDocumentsPerTopicCount),median)
  k_up_to <- max(which(median_of_n_documents_per_lda_model > topic.size.median.threshold))


  slopes <- perplexities[1:(k_up_to-1)] - perplexities[2:k_up_to]
  # Next we assess how bigger the biggest slope is compared to the smallest one (highest number of topics available).
  slopes_magnitude <- abs(slopes/slopes[k_up_to - 1])
  # And we choose the highest k which the slope is higher than 2 (twice the size of the smallest difference available)
  choice_of_k <- max(which(slopes_magnitude > 1)) + 1 #slope is between k and k+1. If the slope was the highest, means k+1 was the latest justifiable choice.

  chosen_k_and_median <- data.table(data.frame(choice_of_k,median_of_n_documents_per_lda_model[choice_of_k]))
  names(chosen_k_and_median) <- c("k","topic_size_median")

  return(chosen_k_and_median)
}

#' Plot LDA Models Perplexity
#'
#' This will calculate the perplexity of the model against itself (TODO: Add a holdout option) for every model
#' in the list, and plot as a line plot. The perplexity serves to give a single digit value per
#' model (each with a different k, or alpha) representing how well the generative model can generate the documents.
#' Lower value is better. In topicmodels journal, this is used to select the k-value, when it reaches the lowest.
#' A continuously decreasing curve may suggest the existence of too many topics in the data,
#' perhaps requiring it to be sliced in a smaller subset before creating the LDA model (I guess).
#'
#' @param lda.model.list A list of topic models. See \code{\link{CalculateLDAModelsUpToN}}.
#'
#' @return A perplexity plot.
PlotLDAModelsPerplexity <- function(lda.model.list){
  perplexities <- sapply(lda.model.list,perplexity)
  plot.table <- data.table(k=1:length(perplexities),perplexity=perplexities)
  p <- ggplot(data=plot.table, aes(x=k, y=perplexity)) + geom_line() + geom_point() + theme_minimal()
  return(p)
}



############# LDA Batch #################
# Functions in this block encapsulate functions above to run an entire LDA model from raw data.


#' Load Corpus Files
#'
#' Used to load the files into memory. Assume the format of the new crawler, where each year of
#' mailing list is inside a folder, and months inside sub-folders.
#' See \code{\link{rawToLDA}} to see it's usage.
#'
#' TODO: Parameterize the file extension (currently assumes reply.body.txt)
#'
#' @param raw.corpus.folder.path The path to the corpus folder (e.g. 2012.parsed)
#'
#' Returns a folder used by \code{\link{rawToLDA}}.
loadFiles <- function(parsed.corpus.folder.path,corpus_setup="/**/*.reply.title_body.txt"){
  #Load raw corpus from folder path.
  folder <- readtext(paste0(parsed.corpus.folder.path,corpus_setup),
                     docvarsfrom = "filepaths"
  )
  # Update file names to remove folder name and extension
  remove_prefix <- sapply(str_split(folder$doc_id,"/"),"[[",2)
  remove_prefix_and_suffix <- sapply(str_split(remove_prefix,"[.]"),"[[",1)
  folder$doc_id <- remove_prefix_and_suffix
  rownames(folder) <- folder$doc_id

  return(folder)
}

#' Raw to LDA
#'
#' Raw to LDA make several assumptions on pre-processing rules. See the topic_flow_creation vignette for more details.
#' @param folder Folder variable containing all raw data. See \code{\link{loadFiles}}.
#' @param k The number of topics k for the model.
#' @param months A character vector containing the months of interest (currently one uses the first month)
#'
#' @return A list structure containing 'tokens', 'dfm', 'LDA', used by various functions in the package.
rawToLDA <- function(folder,k,months){
  s <- suppressPackageStartupMessages



  #Subset in the folder containing all e-mail replies, the months of interest (leverages the fact the Month name is part of the file name)
  month <- months[1]
  is.document.from.month <- grepl(month,folder$doc_id)
  folder.month <- folder[is.document.from.month,]

  # Every e-mail reply is a document
  corpus <- corpus(x=folder.month)

  #2008_Feb_223.txt 2008_Feb_227.txt 2008_Feb_300.txt
  #0                0                0


  # Tokenize. Several assumptions made here on pre-processing.
  tokens <- tokens(corpus, what = "word", remove_numbers = FALSE, remove_punct = TRUE,
                   remove_symbols = TRUE, remove_separators = TRUE,
                   remove_twitter = FALSE, remove_hyphens = FALSE, remove_url = TRUE)
  tokens <- tokens_tolower(tokens)
  tokens <- removeFeatures(tokens, stopwords("english"))

  # Filter Empty Documents
  tokens.length <- sapply(tokens,length)
  tokens <- tokens[!tokens.length == 0]

  # DFM
  dfm <- dfm(tokens)

  # DFM filter for tokens with nchar > 2 only
  dfm <-dfm_select(dfm,min_nchar=2,selection="remove")

  #LDA
  #TODO: Extend function to select best K based on lowest perplexity.
  #Ks <- c(10,12) #2:20 # Remember there is no model k=1, always start by 2 or LDA will crash.
  #model.k.for.inspection <- 1 #If the list of models contain only 2 positions, then access it by either 1 or 2. Don't create a Ks <-c(10) and expect to access it at position 10, it will be on position 1!

  lda.vem <- CalculateLDAModelsInKSetOld(dfm,k,method="VEM")

  model <- list()
  model[["tokens"]] <- tokens
  model[["LDA"]] <- lda.vem[[1]]
  model[["dfm"]] <- dfm #Necessary for LDAVis plot

  return(model)
}

#' Prototype Function to Filter Corpus
filterCorpus <- function(year,month,folder){

  #Subset in the folder containing all e-mail replies, the months of interest (leverages the fact the Month name is part of the file name)
  is.document.from.month <- grepl(month,folder$doc_id)
  folder.month <- folder[is.document.from.month,]

  # Every e-mail reply is a document
  corpus <- corpus(x=folder.month)

  #2008_Feb_223.txt 2008_Feb_227.txt 2008_Feb_300.txt
  #0                0                0


  # Tokenize. Several assumptions made here on pre-processing.
  tokens <- tokens(corpus, what = "word", remove_numbers = FALSE, remove_punct = TRUE,
                   remove_symbols = TRUE, remove_separators = TRUE,
                   remove_twitter = FALSE, remove_hyphens = FALSE, remove_url = TRUE)
  tokens <- tokens_tolower(tokens)
  tokens <- removeFeatures(tokens, stopwords("english"))

  # Filter Empty Documents
  tokens.length <- sapply(tokens,length)
  tokens <- tokens[!tokens.length == 0]

  # DFM
  dfm <- dfm(tokens)

  # DFM filter for tokens with nchar > 2 only
  dfm <-dfm_select(dfm,min_nchar=2,selection="remove")

  return(dfm)
}
