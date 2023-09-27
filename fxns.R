# 
# crosses<-jsonlite::fromJSON("historicalcrosses.json")
# parents<-as.data.frame(cbind(crosses[["result"]][["data"]]$parent1$germplasmName,
#             crosses[["result"]][["data"]]$parent2$germplasmName))
# fre<-as.data.frame(table(parents$V1, parents$V2))
# fre_sub<-fre[which(as.numeric(fre$Freq)>0),]
# 
# 

show_metadata <- function(res) {
  #cat(str(res))
  res <- httr::content(res, "text")
  show_pagination(res)
}

show_pagination <- function(res) {
  
  res <- jsonlite::fromJSON(txt = res)
  
  if (is.null(res$metadata)) return()
  pagination <- res$metadata$pagination
  
  if (!is.null(pagination)) {
    ba_message(msg = paste0("Returning page ",
                            pagination$currentPage,
                            " (max. ",
                            as.integer(pagination$totalPages) - 1,
                            ") with max. ",
                            pagination$pageSize,
                            " items (out of a total of ",
                            pagination$totalCount,
                            ")."))
    
  }
  
}


brapiGET <- function(url, format = "json", con = NULL) {
  ba_message(msg = paste0("URL call was: ", url, "\n"))
  ba_message(msg = paste0("Waiting for response from server: ...\n"))
  
  res <- httr::GET(url = url,
                   httr::timeout(25),
                   httr::add_headers("Authorization" = paste("Bearer", con$token)))
  
  txt <- ifelse(res$status == 200, " ok!", " problem!")
  ba_message(msg = paste0("Server status: ", txt, "\n"))
  url <- httr::content(res)
  if (format == "json") show_server_status_messages(res)
  return(res)
}


ba_message <- function(msg = "Using local test server.") {
  if(is.null(msg)) return("")
  if(!getOption("brapi_info", default = FALSE)) return("")
  return(message(msg))
}

show_server_status_messages <- function(out) {
  
  
  show_message <- function(msg_type, msg_Title, msg_color) {
    ba_message(msg_color(paste0("\n", msg_Title,":")))
    sapply(out[names(out) == msg_type] %>% unlist, msg_color) %>%
      as.character %>%
      paste0("\n") %>%
      ba_message()
  }
  
  if(!getOption("brapi_info", default = FALSE)) return()
  
  # if(!is.null(out$metadata)) {
  #   out <- out$metadata$status %>% unlist %>% as.list()
  #   ba_message(crayon::yellow("Status details from Server:"))
  #
  #   show_message("info", "Infos", crayon::blue)
  #   show_message("success", "Successes", crayon::green)
  #   show_message("error", "Errors", crayon::red)
  #
  # }
  
}

# Retrieve the url where BrAPI has been implemented
get_brapi <- function(con = NULL) {
  if (is.null(con)) return(NULL)
  if (!is.null(con$apipath)) {
    con$apipath <- paste0("/", con$apipath)
  }
  if (con$secure) {
    con$protocol <- "https://"
  }
  port <- ifelse(con$port == 80, "", paste0(":", con$port))
  
  version <- ifelse("version" %in% names(con), con$version, "v1")
  
  brapi_version <- paste0("/brapi/", version, "/")
  
  if (con$multicrop) {
    url <- paste0(con$protocol, con$db, port, con$apipath, "/",
                  con$crop, brapi_version)
  } else {
    url <- paste0(con$protocol, con$db, port, con$apipath,
                  brapi_version)
  }
  return(url)
}




#create pedigree matrix

PEDMATRIX<-function(pedigree){
  
  ped<-pedigree[,1:3]
  
  ## clean data
  
  #recode NAs and blank cells as 0
  ped[is.na(ped)]<-"0"
  #ped$Female_Parent<-gsub("^$","0", ped$Female_Parent)
  #ped$Male_Parent<-gsub("^$","0", ped$Male_Parent)
  
  #recode unknown accessions as 0
  ped$Accession<-gsub("unknown", "0", ped$Accession, fixed=T)
  ped$Male_Parent<-gsub("unknown", "0", ped$Male_Parent, fixed=T)
  ped$Female_Parent<-gsub("unknown", "0", ped$Female_Parent, fixed=T)
  
  #remove duplicate entries
  ped<-ped[!duplicated(ped$Accession),]
  
  #get rid of unknown accessions in first column
  ped<-ped[-which(ped$Accession==0),]
  
  #convert characters to factors
  str(ped)
  ped<-as.data.frame(map_if(ped, is.character, as.factor))
  
  #calculate rel matrix
  relmat<-as.matrix(Amatrix(ped, ploidy=10))
  
  return(relmat)
  
}


ba_germplasm_details2<- function (con = NULL, germplasmQuery = "", rclass = c("tibble", 
                                                     "data.frame", "list", "json")) 
{
  ba_check(con = con, verbose = FALSE, brapi_calls = "germplasm/id")
  #check_character(germplasmQuery)
  #check_req(germplasmQuery)
  rclass <- match.arg(rclass)
  callurl <- get_brapi(con = con) %>% paste0("germplasm", germplasmQuery)
  try({
    resp <- brapiGET(url = callurl, con = con)
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    out <- NULL
    if (rclass %in% c("json", "list")) {
      out <- dat2tbl(res = cont, rclass = rclass)
    }
    if (rclass == "data.frame") {
      out <- gp2tbl(cont)
    }
    if (rclass == "tibble") {
      out <- gp2tbl(cont) %>% tibble::as_tibble(validate = FALSE)
    }
    class(out) <- c(class(out), "ba_germplasm_details")
    show_metadata(resp)
    return(out)
  })
}


gp2tbl <- function(res, type = '1') {
  
  lst <- tryCatch(
    jsonlite::fromJSON(txt = res)
  )
  
  if (type == '1') {
    dat <- jsonlite::toJSON(x = lst$result)
  } else {
    dat <- jsonlite::toJSON(x = lst$result$data)
  }
  
  
  df <- jsonlite::fromJSON(txt = dat, simplifyDataFrame = TRUE, flatten = TRUE)
  dfdonors <- synonyms <- taxonIds <- list()
  if (length(df$donors) == 0) df$donors <- list()
  if (length(df$synonyms) == 0) df$synonyms <- list()
  taxonIds <- as.list(df$taxonIds)
  donors <- as.list(df$donors)
  typeOfGermplasmStorageCode <- df$typeOfGermplasmStorageCode
  
  if(length(df$donors) == 0) df$donors <- NULL
  if(length(df$taxonIds) == 0) df$taxonIds <- NULL
  if(length(df$synonyms) == 0) df$synonyms <- NULL
  if(length(df$typeOfGermplasmStorageCode) == 0) df$typeOfGermplasmStorageCode <- NULL
  
  if(length(df$instituteName) == 0) df$instituteName <- NULL
  if(length(df$speciesAuthority) == 0) df$speciesAuthority <- NULL
  
  # filter out empty lists by replacing with empty string
  n <- length(df)
  for (i in 1:n) {
    if (class(df[[i]]) == "list" & length(df[[i]]) == 0) {
      df[[i]] <- ""
    }
  }
  
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  
  rep_df <- function(df, n) {
    if(n == 1) return(df)
    df1 <- df
    for(i in 2:n) {
      df <- rbind(df, df1)
    }
    df
  }
  join_df <- function(df, al, prefix) {
    if (nrow(al) == 0) return(df)
    names(al) <- paste0(prefix, ".", names(al))
    df <- rep_df(df, nrow(al))
    as.data.frame(cbind(df, al), stringsAsFactors = FALSE)
  }
  
  df <- join_df(df, as.data.frame(donors), "donors")
  df <- join_df(df, as.data.frame(taxonIds), "taxonIds")
  df <- join_df(df, as.data.frame(synonyms), "synonyms")
  df <- join_df(df, as.data.frame(typeOfGermplasmStorageCode), "typeOfGermplasmStorageCode")
  
  
  
  # join_all <- function(dat2) {
  #   dat2 <- join_slaves(dat2, "synonyms")
  #   if ("taxonIds" %in% names(dat2))  dat2 <- join_slaves(dat2, "taxonIds")
  #   dat2 <- join_slaves(dat2, "donors")
  #
  #   return(dat2)
  # }
  #
  #
  # out <- join_all(df[1, ])
  
  # n <- nrow(df)
  #
  # if(n > 1) {
  #   for (i in 2:n) {
  #     out <- dplyr::bind_rows(out, join_all(df[i, ]))
  #   }
  # }
  
  return(df)
}


ba_seedlots_details <- function(con = NULL,
                                 
                                pageSize=1000,
                                page=0,
                                 rclass = c("tibble", "data.frame",
                                            "list", "json")) {
  # ba_check(con = con, verbose = FALSE, brapi_calls = "germplasm/id")
  # check_character(germplasmDbId)
  # check_req(germplasmDbId)
  rclass <- match.arg(rclass)
  
  callurl <- get_brapi(con = con) %>% paste0("seedlots?page=",page,"&pageSize=", pageSize)
  
  try({
    resp <- brapiGET(url = callurl, con = con)
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    out <- NULL
    if (rclass %in% c("json", "list")) {
      out <- dat2tbl(res = cont, rclass = rclass)
    }
    if (rclass == "data.frame") {
      out <- gp2tbl(cont)
    }
    if (rclass == "tibble") {
      out <- gp2tbl(cont) %>% tibble::as_tibble(validate = FALSE)
    }
    class(out) <- c(class(out), "ba_germplasm_details")
    
    show_metadata(resp)
    return(out)
  })
}

ba_crosses_study <- function(con = NULL,
                                crossingProjectDbId="",
                                
                                pageSize=1000,
                                page=0,
                                rclass = c("tibble", "data.frame",
                                           "list", "json")) {
  # ba_check(con = con, verbose = FALSE, brapi_calls = "germplasm/id")
  # check_character(germplasmDbId)
  # check_req(germplasmDbId)
  rclass <- match.arg(rclass)
  
  callurl <- get_brapi(con = con) %>% paste0("crosses?crossingProjectDbId=", crossingProjectDbId,
                                             "&page=",page,"&pageSize=", pageSize)
  
  try({
    resp <- brapiGET(url = callurl, con = con)
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    out <- NULL
    if (rclass %in% c("json", "list")) {
      out <- dat2tbl(res = cont, rclass = rclass)
    }
    if (rclass == "data.frame") {
      out <- gp2tbl(cont)
    }
    if (rclass == "tibble") {
      out <- gp2tbl(cont) %>% tibble::as_tibble(validate = FALSE)
    }
    class(out) <- c(class(out), "ba_germplasm_details")
    
    show_metadata(resp)
    return(out)
  })
}



ba_check <- function(con = NULL, verbose = TRUE, brapi_calls = "any") {
  stopifnot(is.ba_con(con))
  stopifnot(is.logical(verbose))
  stopifnot(is.character(brapi_calls))
  
  url <- con$db
  
  ba_can_internet()
  ba_can_internet(url)
  
  if (verbose) {
    ba_message("BrAPI connection ok.")
    ba_message(paste(con, collapse = "\n"))
  }
  return(TRUE)
}

is.ba_con <- function(obj) {
  res <- "ba_con" %in% class(obj)
  return(res)
}

ba_can_internet <- function(url = "www.google.org") {
  stopifnot(is.character(url))
  return(invisible(curl::nslookup(host = url)))
}

dat2tbl <- function(res, rclass = "tibble", brapi_class = "ba", result_level = "data") {
  if (rclass == "json") {
    return(jsonlite::prettify(txt = res))
  }
  lst <- jsonlite::fromJSON(txt = res)
  if(result_level == "data") {
    dat <- jsonlite::toJSON(x = lst$result$data)
  }
  if(result_level == "result"){
    dat <- jsonlite::toJSON(x = lst$result)
  }
  if(result_level == "progeny"){
    dat <- jsonlite::toJSON(x = lst$result$progeny)
    germplasmDbId <- lst$result$germplasmDbId
    defaultDisplayName <- lst$result$defaultDisplayName
    np <- max(length(lst$result$progeny), 1)
    dat1 <- as.data.frame(cbind(germplasmDbId = rep(germplasmDbId, np),
                                defaultDisplayName = rep(defaultDisplayName, np)))
    
    
  }
  
  if (rclass == "list") {
    return(jsonlite::fromJSON(txt = res, simplifyVector = FALSE))
  }
  if (rclass == "vector") {
    return(jsonlite::fromJSON(txt = dat, simplifyVector = TRUE))
  }
  if (rclass == "data.frame") {
    res <- jsonlite::fromJSON(txt = dat,
                              simplifyDataFrame = TRUE) %>% as.data.frame
  }
  if (rclass == "tibble") {
    res <- jsonlite::fromJSON(txt = dat, simplifyDataFrame = TRUE,
                              flatten = TRUE)
    res <- tibble::as_tibble(x = res)
  }
  if(result_level == "progeny") {
    if(nrow(res) > 0) {
      names(res) <- paste0("progeny.", names(res))
      res <- cbind(dat1, res)
    } else {
      res <- dat1
    }
  }
  attr(x = res, which = "metadata") <- lst$metadata
  class(res) <- c(class(res), brapi_class)
  return(res)
}


init_cross_table<-function(cross_list, Female.Parent='Female.Parent', Male.Parent="Male.Parent", new_crosses=F){
 
   if(dim(cross_list)[2]==1){ #if cross list is empty (first day)
    return(NULL)
  }
  
  #new_crosses means that there won't be any progenies because the cross was made this year
  freq_crosses<-as.data.frame(table(cross_list[,Female.Parent], cross_list[,Male.Parent]))
  freq_crosses<-freq_crosses[-which(freq_crosses$Freq==0),] #get rid of crosses never made
  colnames(freq_crosses)<-c("Female.Parent", "Male.Parent", "Number.of.Crosses")
  
  if(new_crosses==F){
  progeny_crosses<-as.data.frame(aggregate(Number.of.Progenies~Female.Parent+Male.Parent,FUN=sum, data=cross_list))
  crosses_table <- freq_crosses %>% right_join(progeny_crosses, by=c("Female.Parent","Male.Parent"))
  
  return(crosses_table)
  } else {
  
  freq_crosses$Number.of.Progenies<-"None, new cross"
  crosses_table<-freq_crosses
  return(crosses_table)
  }
  
}

get_endpoint <- function(pointbase, ...) {
  forbidden <- "[/?&]$"
  pointbase <- sub(forbidden, "", pointbase)
  args <- list(...)
  
  if (all(c("pageSize", "page") %in% names(args))) {
    check_paging(args$pageSize, args$page)
  }
  if ("pageSize" %in% names(args)) {
    args$pageSize <- ifelse(args$pageSize == 1000, "", args$pageSize)
  }
  if ("page" %in% names(args)) {
    args$page <- ifelse(args$page == 0, "", args$page)
  }
  
  p <- list()
  j <- 1
  
  for (i in seq_along(args)) {
    if (nchar(names(args)[[i]]) == 0) stop("All parameters must have a name.")
    if (is.logical(args[[i]])) args[[i]] <- tolower(args[[i]])
    if (length(args[[i]]) == 1) {
      if (is.na(args[[i]])) args[[i]] <- ""
      
      if (args[[i]] == "any") next()
      if (args[[i]] == 0) next()
    }
    
    if (!is.null(args[[i]]) && args[[i]] != "") {
      args[[i]] <- sub(forbidden, "", args[[i]])
      
      p[[j]] <- paste0(names(args)[[i]], "=", paste(args[[i]], collapse = ","))
      j <- j + 1
    }
  }
  url <- gsub(pattern = " ",
              replacement = "%20",
              x = paste0(pointbase, "?", paste(p, collapse = "&")))
  return(sub(pattern = forbidden,
             replacement = "",
             x = url))
}
