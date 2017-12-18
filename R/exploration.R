############ Making sense of Topics Functions ################

#' Get Documents per Topic Count
#'
#' Returns a named numeric vector, where the names are the topics represented by
#' a single word, and the values are the number of documents the given topic was assigned.
#'
#' @param lda.model A single lda model.
#'
#' @return The topic document distribution.
GetDocumentsPerTopicCount <- function(lda.model){

  topic.document.distribution <- summary(as.factor(topics(lda.model)))
  #names(topic.document.distribution) <- get_terms(lda.model)
  return(topic.document.distribution)
}

#' Prints Topics Top Term
#'
#' Calls \code{\link{GetDocumentsPerTopicCount}} for every topic. Best used just after creating the LDA models
#' to get a sense of them for every value of k. Beware this can be HIGHLY misleading since a topic is a distribution
#' over terms, and the top term may have a very close probability to occur as other term.
#'
#' @param lda.model.list A list of lda models. See \code{\link{rawToLDA}}.
PrintTopicsTopTerm <- function(lda.model.list){
  for(i in 1:length(lda.model.list)){
    topic.document.counts[[i]] <- GetDocumentsPerTopicCount(lda.model.list[[i]])
    cat("LDA k=",lda.model.list[[i]]@k,"\n")
    print(topic.document.counts[[i]])
    cat("\n")
  }
}

#' Get Documents of Topic K
#'
#' Returns a vector of characters, where each element is the name of a document associated to topic K.
#' Note that this can be HIGHLY misleading since a document have a distribution over topics and the top topic
#' may have a very close probability to occur as the other topic.
#'
#' @param lda.model A single lda model.
#' @param k The topic k (e.g. topic 5 from a lda model of 10 topics) to list the documents.
#'
#' @return The list of document names.
GetDocumentsOfTopTopicK <- function(lda.model,k){
  return(names(topics(lda.model)[topics(lda.model) == k]))
}

#' Get Topic Term Matrices
#'
#' Returns the topic term matrices for a list of lda models. See also \code{\link{GetDocumentTopicMatrices}}.
#'
#' @param lda.model.list The list of lda models. See \code{\link{rawToLDA}}.
#' @param n.terms The number of terms to display per topic (note the vocabulary can go up to over 95k for a given month).
#'
#' @return The ordered topic term matrices.
GetTopicTermMatrices <- function(lda.model.list,n.terms=10){
  terms.and.topics <- lapply(lda.model.list,posterior)
  topic.term.matrices <- lapply(terms.and.topics,"[[","terms")
  #topic <- topic.matrix[i,]
  #ordered.positions.topic <- order(-topic)
  #table <- data.table(terms=topic.model@terms[ordered.positions.topic],score=topic[ordered.positions.topic])
  ordered.topic.term.matrices <- lapply(topic.term.matrices,AuxiliaryOrderTopicTermMatrices,n.terms)
  return(ordered.topic.term.matrices)
}
AuxiliaryOrderTopicTermMatrices <- function(topic.term.matrix,n.terms){
  # This auxiliary function for GetDocumentTopicMatrices turns a DocumentTermMatrix from LDA into a list of ordered numeric vectors and a list of ordered topic names.
  topic.term.list <- apply(topic.term.matrix, 1, as.list)
  topic.term.list <- lapply(topic.term.list,as.numeric) #turn every document of the list into a numeric vector so ordering can be performed
  topic.top.topics <- lapply(topic.term.list,order,decreasing=TRUE) #ordered position of the topics
  topic.top.terms <- lapply(topic.top.topics,function(x) colnames(topic.term.matrix)[x][1:n.terms])
  topic.top.probabilities <- lapply(topic.term.list,sort,decreasing=TRUE)
  topic.top.probabilities <- lapply(topic.top.probabilities,function(x) x[1:n.terms])
  return(list(topic.top.terms=topic.top.terms,topic.top.probabilities=topic.top.probabilities))
}

#' Get Document Topic Matrices
#'
#' Returns the topic term matrices for a list of lda models. See also \code{\link{GetTopicTermMatrices}}.
#'
#' @param The list of models. See \code{\link{rawToLDA}}.
#'
#' @return The ordered document topic matrices.
GetDocumentTopicMatrices <- function(lda.model.list){
  terms.and.topics <- lapply(lda.model.list,posterior)
  document.topic.matrices <- lapply(terms.and.topics,"[[","topics")
  ordered.document.topic.matrices <- lapply(document.topic.matrices,AuxiliaryOrderDocumentTopicMatrices)
  return(ordered.document.topic.matrices)
}
AuxiliaryOrderDocumentTopicMatrices <- function(document.topic.matrix){
  # This auxiliary function for GetDocumentTopicMatrices turns a DocumentTermMatrix from LDA into a list of ordered numeric vectors and a list of ordered topic names.
  doc.term.list <- apply(document.topic.matrix, 1, as.list)
  doc.term.list <- lapply(doc.term.list,as.numeric) #turn every document of the list into a numeric vector so ordering can be performed
  document.top.topics <- lapply(doc.term.list,order,decreasing=TRUE) #ordered position of the topics
  document.top.probabilities <- lapply(doc.term.list,sort,decreasing=TRUE)
  return(list(document.top.topics=document.top.topics,document.top.probabilities=document.top.probabilities))
}

#' Get Documents Assigned to Topic K
#'
#' Note the documents are defined as a distribution over topics.
#' This returns the highest probability for a given topic,
#' but it is possible the 2nd to high topic may have about the same probability.
#'
#' @param lda.model A single lda model.
#' @param k The topic k (e.g. topic 5 from a lda model of 10 topics) to list the documents.
#'
#' @return The list of documents for topic k in lda.model.
GetDocumentsAssignedToTopicK <- function(lda.model,k){
  all.documents <- topics(lda.model)
  documents.assigned.to.k <- names(all.documents[all.documents == k])
  return(documents.assigned.to.k)
}
