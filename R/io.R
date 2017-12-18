############ Export Models for Topic Flow Visualization ##############

#' Export Document Term Matrix
#'
#' Exports a document term matrix to a specified *folder* path.
#'
#' @param models The models object as output from \code{\link{rawToLDA}}.
#' @param exportPath A folder path to export the Document Term Matrix of each month in models
exportDocumentTermMatrix <- function(models,exportPath){
  names <- names(models)
  exportLDAModelAux <- function(name,models,exportPath){
    lda.model <- models[[name]][["LDA"]]
    lda.model.posterior <- posterior(lda.model)
    dtm <- lda.model.posterior$topics
    path <- paste0(exportPath,"/",name,".csv")

    write.csv(dtm,path,row.names=TRUE)
  }
  lapply(names,exportLDAModelAux,models,exportPath)
}

#' Export Topic Term Matrix
#'
#' Exports a document term matrix to a specified *folder* path.
#'
#' @param models The models object as output from \code{\link{rawToLDA}}.
#' @param exportPath A folder path to export the Document Term Matrix of each month in models
exportTopicTermMatrix <- function(models,exportPath){
  names <- names(models)
  exportLDAModelAux <- function(name,models,exportPath){
    lda.model <- models[[name]][["LDA"]]
    lda.model.posterior <- posterior(lda.model)
    dtm <- lda.model.posterior$terms
    path <- paste0(exportPath,"/",name,".csv")
    write.csv(dtm,path,row.names=TRUE)
  }
  lapply(names,exportLDAModelAux,models,exportPath)
}


#' Export Leximancer to Topic Flow
#'
#' A transformation is provided for the equivalent of a topic-term matrix in order
#' that replaces LDAVis. The code below provides a transformation for it's equivalent to topic-term matrix in order
#' to construct topic flows in the same way as done as LDA.
#' This section, although functional, is still undergoing work as the tool as only recently acquired by the group.
#'
#' @param conceptMapYear1 A concept versus concept matrix from Leximancer on a given year.
#' @param conceptMapYear2 A concept versus concept matrix from Leximancer on a *following* year.
#' @param savePath The csv file to save the the topic flow file.
testConceptSimilarity <- function(conceptMapYear1="~/Desktop/ttm_leximancer_2009.csv",
                                  conceptMapYear2="~/Desktop/ttm_leximancer_2010.csv",
                                  savePath="~/Desktop/leximancer_FD_similarity_highest_likelehood_term_representation_2009_2010.csv"){
  l.ttm1 <- fread(conceptMapYear1)
  l.ttm1.names <- l.ttm1$concept
  l.ttm1 <- l.ttm1[,2:ncol(l.ttm1),with=FALSE]
  l.ttm1 <- as.matrix(l.ttm1)

  l.ttm2 <- fread(conceptMapYear2)
  l.ttm2.names <- l.ttm2$concept
  l.ttm2 <- l.ttm2[,2:ncol(l.ttm2),with=FALSE]
  l.ttm2 <- as.matrix(l.ttm2)
  similarity <- CalculateHighestTopicCosineSimilarity(l.ttm1,l.ttm2)
  #write.csv(similarity,"~/Desktop/leximancer_FD_similarity_2009_2010.csv",row.names=FALSE)

  # Beware this is a over simplification of the topic, using the highest likelehood word rather than all it's terms:
  similarity$ttm1 <- l.ttm1.names[similarity$ttm1]
  similarity$ttm2 <- l.ttm2.names[similarity$ttm2]
  write.csv(similarity,savePath,row.names=FALSE)
}


