##ptt crawler
libraries <- c("rvest", "dplyr", "data.table", "tmcn", "rjson")
invisible(sapply(libraries, function(x){
  if(x!="tmcn" & !is.element(x, installed.packages()[,1])){
    install.packages(x)#, dependencies=TRUE)
  }else if(x=="tmcn" & !is.element(x, installed.packages()[,1])){
    install.packages("http://download.r-forge.r-project.org/src/contrib/tmcn_0.1-4.tar.gz", repos =NULL)
  }
  library(x, character.only = TRUE)
}))

cat("\nptt_list_crawler <- function(link, min=1, max=9999999, forum_name)")
ptt_list_crawler <- function(link, min=1, max=9999999, forum_name = paste0('ptt ',substr(link,unlist(gregexpr(pattern ='bbs',link))+4,unlist(gregexpr(pattern ='index',link))-2))){
  link <- substr(link, 1, 
                 gregexpr("index",link)[[1]][length(gregexpr("index",link)[[1]])] + 4)
  article_url_list <- {}
  ##Get posts' urls from lists of pages.
  cat(forum_name, "\n")
  for(i in min:max){
    tmp <- paste(i, ".html", sep="")
    url <- paste(link, tmp, sep="")
    tryCatch({
      cat("\r ", forum_name, "- Page: ", i)
      
      ##articles' url
      article_url      <- read_html(url) %>% html_nodes(".title") %>% html_nodes("a") %>% html_attr('href')
      article_url_list <- c(article_url_list, article_url)
      gc() 
      Sys.sleep(runif(1,4,6))
      #check <- i
      status <<- "success"
    }, error = function(e) {
      #conditionMessage(e)
      cat("\ntry again...")
      cat("\nwait for about 2 mins...")
      Sys.sleep(runif(1,120,130))
      
      cat("\nstart...")
      status <<- list_re_crawl(url, article_url_list, forum_name, i)
    })
    if(status=="success")
      check <- i
    ## Has Accessed the last page: break
    if(check!=i){
      #cat("\ncheck=",check)
      #cat("\ni=",i)
      break
    }
  }
  cat("\r ", forum_name, " latest page: ", i-1)
  ##latest page
  cat("\n ")
  
  ##default value of max
  if(max==9999999)
    max <- i - 1 
  
  cat("\nHas Accessed the last page : Page", max)
  article_url_list <- unique(article_url_list)
  
  ptt_df <- data.frame("Url" = paste0("https://www.ptt.cc", article_url_list),
                       "Title"="", "Date"="", "Author"="", "Content"="", "Reply"="", 
                       "Repliers"="", stringsAsFactors=F)
  
  output_list <- list(forum_name, min, max, ptt_df)
  names(output_list) <- c("forum_name", "min", "max", "ptt_df")
  return(output_list)
}

cat("\nptt_article_crawler <- function(x = \"\")")
ptt_article_crawler <- function(x = ""){
  ##suppress warnings
  options(warn = -1)
  if(x != ""){
    forum_name <- x$forum_name
    min        <- x$min
    max        <- x$max
    ptt_df     <- x$ptt_df
    rm(x)
    
    start <- 1 
  }else{
    ##if x == "", user should select a file to continue crawling....
    cat("please select the tmp file which you want to keep on crawling...")
    ptt_filename <- file.choose()
    
    #ptt_df <- fread(ptt_filename)
    dat <- lapply(fromJSON(file=ptt_filename), function(j) {
      as.data.frame(replace(j, sapply(j, is.list), NA), stringsAsFactors=F)
    })
    ptt_df = do.call("rbind", dat)
    
    options(warn = 0)
    ptt_df[is.na(ptt_df)] <- ""
    start  <- which(ptt_df[, 2:6, with=FALSE]=="")[1]
    
    ptt_filename <- strsplit(ptt_filename, "[\\.]")[[1]][length(strsplit(ptt_filename, "[\\.]")[[1]])-1]
    
    use_default <- readline("use default parameter based on the selected file's name(y or n): ")
    if(use_default=="y"){
      c(min, max) %tin% as.numeric(unique(unlist(regmatches(ptt_filename, gregexpr("[0-9]+", ptt_filename)))))
      forum_name <- substr(ptt_filename, 1, gregexpr(min, ptt_filename)[[1]][1]-2)
      }else{
      forum_name <- readline("enter a forum name: ")
      min        <- readline("enter a min(indicates which list page of the forum to start with): ")
      max        <- readline("enter a max(indicates which list page of the forum to end on): ")
    }
  }
  cat("\nforum name:", forum_name, "\nmin:", min, "\nmax:", max)
  cat("\n\nCreate output folders...")
  dir.create(paste0(".\\output\\", forum_name, "\\raw data\\tmp"), showWarnings = FALSE, recursive = TRUE)
  
  fail_list <- c()
  ##Start to crawl out the content...
  for(i in start:nrow(ptt_df)){
    tryCatch({
      url       <- ptt_df$Url[i]
      total_css <- read_html(url) 
      
      content_css <- total_css %>% html_nodes("#main-content") %>% html_text() %>% toUTF8
      content <- substr(content_css, gregexpr("\n", content_css, fixed=TRUE)[[1]][1], gregexpr("發信站: 批踢踢實業坊(ptt.cc)", content_css, fixed=TRUE)[[1]][1] - 3)
      
      meta_value  <- total_css %>% html_nodes(".article-meta-value") %>% html_text() %>% toUTF8
      if(length(meta_value)==4){
        author <- meta_value[1]
        title  <- meta_value[3]
        date   <- meta_value[4]
      }
      
      reply_msg <- total_css %>% html_nodes(".push-content") %>% html_text() %>% 
        paste(., collapse="\n") %>% toUTF8
      reply_id  <- total_css %>% html_nodes(".push-userid") %>% html_text() %>% paste(., collapse="\n")
      
      ptt_df$Title[i]    <- title
      ptt_df$Date[i]     <- date
      ptt_df$Author[i]   <- author
      ptt_df$Content[i]  <- content
      ptt_df$Reply[i]    <- reply_msg
      ptt_df$Repliers[i] <- reply_id
      
      gc()
      #write.csv(ptt_df, paste0(".\\output\\", forum_name, "\\raw data\\tmp\\", forum_name,"_", min, "_", max, "_tmp.csv"), row.names=FALSE)
      
      ##df to json
      ##creating json tmp output will slow down the procedure...
      json_ptt <- toJSON(unname(split(ptt_df, 1:nrow(ptt_df))))#, "R")
      
      #cat(json_ptt)
      write(json_ptt, file(paste0(".\\output\\", forum_name, "\\raw data\\tmp\\", forum_name,"_", min, "_", max, "_tmp.json"), encoding="UTF-8"))
      
      cat("\r PTT article: ",i, " ==>", i/nrow(ptt_df)*100, "% completed.",paste(replicate(50, " "), collapse = ""))
      Sys.sleep(runif(1, 4, 6))
      
    }, error = function(e) {
      fail_list <<- c(fail_list, i)
      cat("\n ")
      cat("\n", forum_name, " PTT article: ", i, " failed. ", i/nrow(ptt_df)*100, "%")
      Sys.sleep(runif(1, 4, 6))
    })
  }
  cat("\n ")
  
  ##try again
  if(!is.null(fail_list)){
    for(i in fail_list){
      ptt_df <- content_re_crawler(i, ptt_df)
    }
  }
  
  cat(forum_name,' : ',nrow(ptt_df),' articles.\n')
  
  ptt_df = unique(ptt_df)
  
  #write.csv(ptt_df, paste0(".\\output\\", forum_name,"\\raw data\\", "\\", forum_name,"_", min, "_", max, ".csv"), row.names=F)
  
  ## split the data frame into multiple output files
  ptt_df$split_id <- sort(rep(1:((nrow(ptt_df) %/% 10000)+1),10000))[1:nrow(ptt_df)]
  spt_ptt_df <- split(ptt_df, ptt_df$split_id) 
  
  if(length(spt_ptt_df) > 1){
    ##invisible: stop lapply from printing to console
    lapply(names(spt_ptt_df), function(x){
      #write.csv(spt_ptt_df[[x]] %>% select(-split_id), paste0(".\\output\\", forum_name,"\\raw data\\", forum_name,"_", min, "_", max, "-", x, ".csv"), row.names = FALSE)
      ##df to json
      json_ptt <- toJSON(unname(split(spt_ptt_df[[x]] %>% select(-split_id), 1:nrow(spt_ptt_df[[x]] %>% select(-split_id)))), "R")
      #cat(json_ptt)
      write(json_ptt, file(paste0(".\\output\\", forum_name,"\\raw data\\", forum_name,"_", min, "_", max, "-", x, ".json"), encoding="UTF-8"))
      
    }) %>% invisible
  }else{
    json_ptt <- toJSON(unname(split(spt_ptt_df[[1]] %>% select(-split_id), 1:nrow(spt_ptt_df[[1]] %>% select(-split_id)))), "R")
    #cat(json_ptt)
    write(json_ptt, file(paste0(".\\output\\", forum_name,"\\raw data\\", forum_name,"_", min, "_", max, ".json"), encoding="UTF-8"))
  }
  
  return(ptt_df)
}

##assign mutiple values to variables...
`%tin%` <- function(x, y) {
  mapply(assign, as.character(substitute(x)[-1]), y,
         MoreArgs = list(envir = parent.frame()))
  invisible()
}

##try again when facing error..
list_re_crawl <-function(url, article_url_list, forum_name, i){
  tryCatch({
    Sys.sleep(runif(1, 15, 17)) 
    article_url <- read_html(url) %>% html_nodes(".title") %>% html_nodes("a") %>% html_attr('href')
    
    article_url_list <<- c(article_url_list, article_url)
    gc() 
    cat("\n ", forum_name, "- Page: ", i, "\n")
    return("success")
  }, error = function(e) {
    cat("\n ", forum_name, "failed. - Page: ", i, "\n")
    return("failure")
  })
}

##
content_re_crawler <- function(i, ptt_df){
  tryCatch({
    url       <- ptt_df$Url[i]
    total_css <- read_html(url) 
    
    content_css <- total_css %>% html_nodes("#main-content") %>% html_text() %>% toUTF8
    content <- substr(content_css, gregexpr("\n", content_css, fixed=TRUE)[[1]][1], gregexpr("發信站: 批踢踢實業坊(ptt.cc)", content_css, fixed=TRUE)[[1]][1] - 3)
    
    meta_value  <- total_css %>% html_nodes(".article-meta-value") %>% html_text() %>% toUTF8
    if(length(meta_value)==4){
      author <- meta_value[1]
      title  <- meta_value[3]
      date   <- meta_value[4]
    }
    
    reply_msg <- total_css %>% html_nodes(".push-content") %>% html_text() %>% 
      paste(., collapse="\n") %>% toUTF8
    reply_id  <- total_css %>% html_nodes(".push-userid") %>% html_text() %>% paste(., collapse="\n")
    
    ptt_df$Title[i]    <- title
    ptt_df$Date[i]     <- date
    ptt_df$Author[i]   <- author
    ptt_df$Content[i]  <- content
    ptt_df$Reply[i]    <- reply_msg
    ptt_df$Repliers[i] <- reply_id
    
    gc()
    #write.csv(ptt_df, paste0(".\\output\\", forum_name, "\\raw data\\tmp\\", forum_name,"_", min, "_", max, "_tmp.csv"), row.names=FALSE)
    
    ##df to json
    json_ptt <- toJSON(unname(split(ptt_df, 1:nrow(ptt_df))))#, "R")
    
    #cat(json_ptt)
    write(json_ptt, file(paste0(".\\output\\", forum_name, "\\raw data\\tmp\\", forum_name,"_", min, "_", max, "_tmp.json"), encoding="UTF-8"))
    
    cat("\r PTT article: ",i, " ==>", i/nrow(ptt_df)*100, "% completed.",paste(replicate(50, " "), collapse = ""))
    Sys.sleep(runif(1, 4, 6))
    
  }, error = function(e) {
    cat("\n ")
    cat("\n", forum_name, " PTT article: ", i, " failed. ", i/nrow(ptt_df)*100, "%")
    Sys.sleep(runif(1, 4, 6))
  })
  return(ptt_df)
}
