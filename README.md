# A Crawler for ptt.cc
###Crawler for ptt.cc.

####This is a small project that can crawl content from ptt.cc (批踢踢實業坊). 
####You just have to give the function the url of a forum, and the function will crawl all the articles content out, like the example below:
```
ptt_df <- ptt_list_crawler(“https://www.ptt.cc/bbs/Soft_job/index.html”)
```
####This function will return all the articles’ urls. 
####If someone only wants to crawl the first to the fifth page, it can be set up as:
```
ptt_df <- ptt_list_crawler(https://www.ptt.cc/bbs/Soft_job/index.html, 1, 5)
```
####After crawling out the urls of the articles, you can then use ptt_article_crawler to crawl all their content.
```
output_ptt <- ptt_article_crawler(ptt_df)
```
####The output will be saved in ./output/XXXXX, and all output will be saved in the json fomat.
####The output will have attributes including the articles’ urls, titles, dates, authors, content, reply messages, and repliers’ IDs.

##to be continue...
