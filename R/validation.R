#' Validate if Topics Group or not emails from same CVE ID
#'
#' This function is used to validate the models from PERCEIVE, by checking if emails
#' discussing about the same vulnerability were mapped to the same topic (good), or not (bad).
#' See the topic_flow_creation vignette for it's usage.
#' @param A list of lda models. See \code{\link{rawToLDA}}.
#' @param A list of cves and file emails, as parsed from CVE Mitre Website. See the validation vignette (cve_mitre_xml_parser_to_yearly_validation.Rmd) for details.
#' @param chosen_year The year to validate the method, as used in the topic_flow_creation vignette.
#'
#' @return A table containing the email of the files, their cve id, and wether they mapped or not to the same topic.
isSameTopicAndSameCVE <- function(models,cves,chosen_year){

  validation <- cves

  validation <- validation[,.(cve_id,file_id)]

  # extract month and year from file_id, which in turn was extracted from url in cve's xml
  validation$month <- sapply(str_split(validation$file_id,"_"),"[[",2)
  validation$year <- sapply(str_split(validation$file_id,"_"),"[[",1)

  # for now only caring about the year of 2013
  validation <- validation[year == chosen_year]

  #add topic id the document was mapped to using maximum likelehood
  all_documents <- topics(models[[1]][["LDA"]])

  for(i in 2:12){
    all_documents <- c(all_documents,topics(models[[i]][["LDA"]]))
  }

  all_documents <- data.table(file_id=names(all_documents),topic_id=all_documents)

  # All files that had a cve id now will be annotated with their topic id resulted from lda
  validation_df <- merge(validation,all_documents,by="file_id",all.x="TRUE")

  # we want cve_ids on the same month
  isSameCluster <- function(topic_ids){
    all(topic_ids == topic_ids[1])
  }

  df <- validation_df[,.(cve_id,month,is_same_topic_id=isSameCluster(topic_id),count=length(file_id),file_ids=str_c(file_id, collapse = ", ")),by=c("cve_id","month")][count >= 2]

  return(df)
}


