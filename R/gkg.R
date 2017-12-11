#' Function to retrive Data from Google Knowledge Graph API as a Data Frame output
#'
#' This function allows you to get the Information for a specific Keyword you enter. The output is a Dataframe with the Data retrived by the Google API.
#' @query The keyword you want to get the information for
#' @token The token for yor Google API Project. Please use your own token if you have a lot of querys. You can find additional informations about the token here: https://developers.google.com/knowledge-graph/prereqs
#' @language The list of language codes (defined in ISO 639) to run the query with. Default is "en"
#' @limit Limits the number of entities to be returned. The API-Limit is 20.
#' @types Restricts returned entities to those of the specified types. For example, you can specify Person (as defined in http://schema.org/Person) to restrict the results to entities representing people. If multiple types are specified, returned entities will contain one or more of these types. Default are all types.
#' @prefix Enables prefix (initial substring) match against names and aliases of entities. For example, a prefix Jung will match entities and aliases such as Jung, Jungle, and Jung-ho Kang. Default is FALSE.
#' @type_output In most cases, the API returns more than one type for a keyword. If type_output is set to TRUE, the types are placed in a cell. FALSE returns the list of types in a row. Default is TRUE.
#' gkg()

gkg <- function(query,
                token = "AIzaSyDsxs2vKm33doRABnW4JoieK8RrDQvJwds",
                language = "en",
                limit = 1,
                types = "",
                prefix = FALSE,
                type_output = TRUE) {

  #detecting problematic Inputs
  if (limit > 20) {
    warning("The Limit for the API is 20")
    limit <- 20
  }

  if (!is.logical(prefix)) {
    stop("The Prefix should be a logical input")
  }

  if (!is.logical(type_output)) {
    stop("The type_output should be a logical input")
  }

  #Build the Call
  query <- gsub("&", "", query)
  url <-
    paste0(
      "https://kgsearch.googleapis.com/v1/entities:search?query=",
      query,
      "&key=",
      token,
      "&languages=",
      language,
      if(types == ""){""} else {
        "&types="},
      types,
      "&prefix=", tolower(prefix),
      "&limit=",
      limit,
      "&indent=false"
    )
  knowledge_call <- curl::curl_fetch_memory(URLencode(url))
  content <-
    jsonlite::fromJSON(rawToChar(knowledge_call$content), simplifyVector = F)
  result <- NULL
  if (knowledge_call$status_code != 200) {
    content <- jsonlite::fromJSON(rawToChar(knowledge_call$content), simplifyVector = F)
    stop(paste0("Error: ", content[[1]][[1]][1], " ", content[[1]][[2]][1]))
  }

  try(for (i in 1:limit) {
    res <- NULL

    res$id <- as.data.frame(content[[3]][[i]]$result$`@id`)
    res$name <- as.data.frame(content[[3]][[i]]$result$name)
    if(isTRUE(type_output)){
      a<-content[[3]][[i]]$result$`@type`
      res$type <- as.data.frame(paste(unlist(a), collapse =" | "))} else {
        res$type <-
          as.data.frame(unlist(content[[3]][[i]]$result$`@type`))
      }
    res$description <-
      as.data.frame(content[[3]][[i]]$result$description)
    if (is.null(content[[3]][[i]]$result$image$url)) {
      res$image <- as.data.frame("No Image")
    } else {
      res$image <- as.data.frame(content[[3]][[i]]$result$image$url)
    }
    if (is.null(content[[3]][[i]]$result$detailedDescription$articleBody)) {
      res$detailedDescription <- as.data.frame("No Description")
      res$detailedDescriptionURL <-
        as.data.frame("No Description")
    } else {
      res$detailedDescription <-
        as.data.frame(content[[3]][[i]]$result$detailedDescription$articleBody)
      res$detailedDescriptionURL <-
        as.data.frame(content[[3]][[i]]$result$detailedDescription$url)
    }
    if (is.null(content[[3]][[i]]$result$url)) {
      res$resultURL <- as.data.frame("No resultURL")
    }
    else {
      res$resultURL <- as.data.frame(content[[3]][[i]]$result$url)
    }
    res$resultScore <- as.data.frame(content[[3]][[i]]$resultScore)
    res <- as.data.frame(res)
    colnames(res) <-
      c(
        "id",
        "name",
        "type",
        "description",
        "image",
        "detailedDescription",
        "detailedDescriptionURL",
        "resultURL",
        "resultScore"
      )
    result <- rbind(result, res)
  }, silent = TRUE)
  if(is.null(result)){result<-"No Google Knowledge Graph available"}
  return(result)
}
