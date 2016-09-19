##index
setwd("Volume-of-Internet-Posts")
source('.\\rscript\\ptt_crawler.R', print.eval  = TRUE, encoding="UTF-8")

#ptt_df <- ptt_list_crawler("https://www.ptt.cc/bbs/Soft_Job/index.html")
#ptt_article_crawler(ptt_df)

##auto crawling
#min <- 1
while(TRUE){
  ptt_df      <- ptt_list_crawler("https://www.ptt.cc/bbs/Soft_Job/index.html", min)
  min         <- ptt_df$max
  output_ptt  <- ptt_article_crawler(ptt_df)
  
  sleep_time <- runif(1,86000,86400)
  print("next time to operate...")
  print(Sys.time() + sleep_time)
  Sys.sleep(sleep_time)
}