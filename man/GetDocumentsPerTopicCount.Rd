% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exploration.R
\name{GetDocumentsPerTopicCount}
\alias{GetDocumentsPerTopicCount}
\title{Get Documents per Topic Count}
\usage{
GetDocumentsPerTopicCount(lda.model)
}
\arguments{
\item{lda.model}{A single lda model.}
}
\value{
The topic document distribution.
}
\description{
Returns a named numeric vector, where the names are the topics represented by
a single word, and the values are the number of documents the given topic was assigned.
}
