#' Function to retrive Data from Google Knowledge Graph API
#'
#' This function allows you to get the Information for a specific Keyword you enter.
#' @query The keyword you want to get the information for
#' @token The token for yor Google API Project. Please use your own token if you have a lot of querys. You can find additional informations about the token here: https://developers.google.com/knowledge-graph/prereqs
#' @language The list of language codes (defined in ISO 639) to run the query with. Default is "en"
#' @limit Limits the number of entities to be returned. The API-Limit is 20.
#' @types Restricts returned entities to those of the specified types. For example, you can specify Person (as defined in http://schema.org/Person) to restrict the results to entities representing people. If multiple types are specified, returned entities will contain one or more of these types. Default are all types.
#' @prefix Enables prefix (initial substring) match against names and aliases of entities. For example, a prefix Jung will match entities and aliases such as Jung, Jungle, and Jung-ho Kang. Default is FALSE.
#' gkg_raw()

gkg_raw <- function(query,
                    token = "AIzaSyDsxs2vKm33doRABnW4JoieK8RrDQvJwds",
                     language = "en",
                     limit = 1,
                     types = "",
                     prefix = FALSE) {

  #detecting problematic Inputs
  if (limit > 20) {
    warning("The Limit for the API is 20")
    limit <- 20
  }

  if (!is.logical(prefix)) {
    stop("The Prefix should be a logical input")
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
  return(content)
}
