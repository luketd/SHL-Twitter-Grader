
library (rsconnect)
library(tidyverse)  
library(rvest)  
library(rtweet)
library(dplyr) 
library(purrr)



## store api keys (these are fake example values; replace with your own keys)
api_key <- "qiuhpw6xWBxLvhkbqlofyoN4m"
api_secret_key <- "hOE5NGbaU3uSStw7o08eaPjeaR4iZ4MKzMLiLDrX2SjsswnosK"
access_token <- "3270729511-UngtR9d7pPPNGWq6ZietCiZ1DminaRpxEsHuPIc"
access_token_secret <- "qlUU8WbSswh0IrFYDhO888iJGZGmU291WH6USb5pifDAW"

# ## authenticate via web browser
# token <- rtweet::create_token(
#   app = "shlTweetGrader",
#   consumer_key = api_key,
#   consumer_secret = api_secret_key)


## authenticate via web browser
token <- create_token(
  app = "rstatsjournalismresearch",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

#of pages(15 per page)
Pages <- 10
#End date(Sunday date + 1 day)
endDate <- as.POSIXct("2020-12-14 5:00:00")

#Thread link
threadLink <- "https://simulationhockey.com/showthread.php?tid=112564"
threadLinkG <- paste(threadLink,"&page=%f", sep="")


# getLast <- threadLink %>%
#   read_html() %>%
#   html_node(".pagination_last") %>%
#   html_text()
# 
# getLast
#endDate <- as.Date(endDate)
pastDate <- endDate - 622800






#testing <-searchTwitter('#shlhockey', since='2020-11-23', until='2020-11-28')
# 
test2 <- rtweet::get_timeline("FriedlandBAP", n = 100, check=FALSE)

filtered <- test2 %>%
  select(created_at, screen_name,hashtags, text ,replyToSN = reply_to_status_id) %>%
  filter(created_at >= pastDate & created_at <= endDate & grepl("shlhockey", tolower(hashtags)) == TRUE)

filtered$created_at <- filtered$created_at - 18000


if (dim(filtered)[1] == 0) {
  print("n/a")
}else {
  
  filtered$created_at <- as.Date(filtered$created_at, format="%y-%mm-%dd")
  filtered["Payout"] <- 0
  
  replyDate <- vector(mode = "character", length =1)
  tweetDate <- vector(mode = "character", length =1)
  reply <- 1
  tweet <- 1
  
  
  for (i in 1:length(filtered$created_at)){
    #Check to see if it is a reply
    if(is.na(filtered$replyToSN[i]) == TRUE) {
      if (length(tweetDate) == 3) {
        #checking if there is already 3 tweets claimed
        filtered$Payout[i] <- 0
      }else if (tweetDate[1]==""){
        #gsub removes emoji characters
        tweetDate[tweet] <- filtered$created_at[i]
        tweet <- tweet +1
        filtered$Payout[i] <- 150000
      } else if (is.na(match(filtered$created_at[i], tweetDate)) == FALSE) {
        #if date was already used
        filtered$Payout[i] <- 0
      } else {
        tweetDate[tweet] <- filtered$created_at[i]
        tweet <- tweet +1
        filtered$Payout[i] <- 150000
        
        
        
      }
      
      #else, checks if it is a reply, if so enter the if statement
    } else if (is.na(filtered$replyToSN[i]) == FALSE) {
      if (length(replyDate) == 3){
        filtered$Payout[i] <- 0
        #if there already are 3 replies that gets paid, then give that payout as 0
      }else if (replyDate[1]==""){
        #gsub removes emoji characters
        replyDate[reply] <- filtered$created_at[i]
        reply <- reply +1
        filtered$Payout[i] <- 50000
      } else if (is.na(match(filtered$created_at[i], replyDate)) == FALSE) {
        #if date was already used
        filtered$Payout[i] <- 0
      } else {
        replyDate[reply] <- filtered$created_at[i]
        reply <- reply +1
        filtered$Payout[i] <- 50000
      }
    }
    
  }
  #gives the summation of the payout
  temp <- filtered %>%
    dplyr::group_by(screen_name) %>%
    dplyr::summarize(Payout = format(sum(Payout), scientific=F))
}
# 
# print(SHL)


rt <- search_tweets(
  "#shlhockey", n = 2500, include_rts = FALSE
)

rtfiltered <- rt %>% 
  filter(created_at >= pastDate & created_at <= endDate & grepl("shlhockey", tolower(hashtags)) == TRUE)
