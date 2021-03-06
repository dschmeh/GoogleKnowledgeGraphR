# GoogleKnowledgeGraphR
[![Rdoc](https://www.r-pkg.org/badges/version/GoogleKnowledgeGraphR)](https://cran.r-project.org/web/packages/GoogleKnowledgeGraphR/index.html)
[![Build Status](https://travis-ci.org/dschmeh/GoogleKnowledgeGraphR.svg?branch=master)](https://travis-ci.org/dschmeh/GoogleKnowledgeGraphR)

A simple R Package to retrieve information from Google Knowledge Graph API. You find more information about the Knowledge Graph here: https://www.google.com/intl/bn/insidesearch/features/search/knowledge.html

# Installation 
To get the current stable Version from CRAN:

```
install.packages("GoogleKnowledgeGraphR")
```
To get the current development version from github:

```
# install.packages("devtools")
devtools::install_github("dschmeh/GoogleKnowledgeGraphR")
```

# Getting Data form the API
Getting data for a specific Keyword:
```
keyword <- "nike"
token <- {{YOUR-API-TOKEN}}
language <-"en"
limit <- 1
types <- ""
perfix <- FALSE
gkg(keyword, token, language, limit, types, perfix)
```
The output is a dataframe containing different informations about the given keyword.
