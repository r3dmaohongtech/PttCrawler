##index
rm(list = ls()) #Remove all objects in the environment
gc() ##Free up the memory

original_path <- getwd()
setwd("PttCrawler")

source('.\\rscript\\ptt_crawler.R', print.eval  = TRUE, encoding="UTF-8")
source('.\\rscript\\jiebaR_n.R', print.eval  = TRUE, encoding="UTF-8")

#ptt_df <- ptt_list_crawler("https://www.ptt.cc/bbs/Soft_Job/index.html")
#ptt_article_crawler(ptt_df)

##auto-crawling
#min <- 1
while(F){
  ptt      <- ptt_list_crawler("https://www.ptt.cc/bbs/Soft_Job/index.html", min)
  min         <- ptt$max
  output_ptt  <- ptt_article_crawler(ptt)
  
  # re-start by tmp file
  # output_ptt  <- ptt_article_crawler()
  
  sleep_time <- runif(1,86000,86400)
  print("next time to operate...")
  print(Sys.time() + sleep_time)
  Sys.sleep(sleep_time)
}

##jieaR

#x_data <- c(output_ptt$Content, output_ptt$Reply)
#jiebaR_n(x_data, forum_name = "ptt Soft_job")

#df <- readJSON2DF(file.choose())
#x_data <- c(df$Content, df$Reply)
#jiebaR_n(x_data, forum_name = "ptt Soft_job")
