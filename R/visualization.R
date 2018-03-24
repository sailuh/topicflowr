##### LDA Vis ######

#' Localhost Interactive Topic Visualization
#'
#' This function is a temporary placeholder for the LDAvis package to test
#' different cosine similarity measurements.
plotLDAVis <- function(model,as.gist=FALSE,topicSimilarityMethod=jsPCA){

  lda.model <- model[["LDA"]]
  lda.model.posterior <- posterior(lda.model)

  phi <- lda.model.posterior$terms
  theta <- lda.model.posterior$topics

  dfm <- model[["dfm"]]
  doc.length <- as.data.frame(dfm)
  doc.length <- rowSums(doc.length)
  doc.length <- doc.length

  vocab <- colnames(phi)

  term.frequency <- as.data.frame(dfm)
  term.frequency <- colSums(term.frequency)

  json <- createJSON(phi=phi,theta=theta,doc.length=doc.length,vocab=vocab,term.frequency=term.frequency,mds.method=topicSimilarityMethod)
  serVis(json=json,open.browser = TRUE,as.gist=as.gist)
}

#' Calculate LDAvis Topic Cosine Similarity
#'
#' This function is used as a replacement to `jsPCA` in LDAVis package function createJSON, as
#' the default can't handle words with probability 0. This also ensures consistency to the different
#' month topics, as it now uses the same cosine similarity instead of symetric Dkl. For more details,
#' see issue: https://github.com/sailuh/perceive/issues/84
#'
#' @param phi, as specified in createJSON on LDAvis.
cosinePCA <- function(phi){
  dist.mat <- proxy::dist(x = phi, method = cosine) #needs package lsa
  # then, we reduce the K by K proximity matrix down to K by 2 using PCA
  pca.fit <- stats::cmdscale(dist.mat, k = 2)
  data.frame(x = pca.fit[,1], y = pca.fit[,2])
}

