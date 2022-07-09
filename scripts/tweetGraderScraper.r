{
suppressMessages(suppressWarnings(require(rsconnect)))
suppressMessages(suppressWarnings(require(tidyverse)))  
suppressMessages(suppressWarnings(require(rvest)))
suppressMessages(suppressWarnings(require(rtweet)))
suppressMessages(suppressWarnings(require(dplyr)))
suppressMessages(suppressWarnings(require(purrr)))
#suppressMessages(suppressWarnings(require(dotenv)))
suppressMessages(suppressWarnings(require(googlesheets4)))
suppressMessages(suppressWarnings(require(discordr)))
options(dplyr.summarise.inform = FALSE)

conn_obj <- create_discord_connection(webhook = "https://discord.com/api/webhooks/991361873784082432/K_GG_WfkpdjBlJaK9VJtuqVu7L9zyrXUMqMt1tD2cHmQLyxCU83N-HYEisDmfenOm6MH" , username = 'BANKER', set_default = TRUE)
  
#End date(Sunday date + 1 day)
##################################################################
#ENTER THE MONDAY DATE YOU WANT TO PULL IT FROM 
#REMEMBER THAT ITS THE WEEK PRIOR MONDAY
endDate <- as.POSIXct(paste0(Sys.Date(), "5:00:00"))


#enter the path that you want to copy the files to
#when you copy the path from file explorer to R it will turn out as
# C:\Users\Luke\Desktop\SHL\Twitter
# change the \ to \\ and it will work
filePath <- "Output\\"
write.table(endDate, file = "Output\\Broken_Twitter_Accounts.txt", sep=',')
##################################################################

##----------------------------------------------------------------
##          Getting the tokens and base variables set up         -
##----------------------------------------------------------------
#load_dot_env(file = "scripts/.env")

api_key <- "qiuhpw6xWBxLvhkbqlofyoN4m"
api_secret_key <- "hOE5NGbaU3uSStw7o08eaPjeaR4iZ4MKzMLiLDrX2SjsswnosK"
access_token <- "3270729511-UngtR9d7pPPNGWq6ZietCiZ1DminaRpxEsHuPIc"
access_token_secret <- "qlUU8WbSswh0IrFYDhO888iJGZGmU291WH6USb5pifDAW"

## authenticate via web browser
token <- create_token(
  app = "rstatsjournalismresearch",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

#Thread link
threadLink <- "https://simulationhockey.com/showthread.php?tid=112765"
threadLinkG <- paste(threadLink,"&page=%f", sep="")

getLast <- threadLink %>%
   read_html() %>%
   html_node(".pagination_last") %>%
   html_text()
 
Pages <- as.integer(getLast)
pastDate <- endDate - 622800



##----------------------------------------------------------------
##        The function that runs the program for twitter         -
##----------------------------------------------------------------

getlist <- function(x) {
  website <- sprintf(threadLinkG,x)
  url <- read_html(website)
  
  
  if(x==1){
    userName <- url %>%
      html_nodes("style~ .classic .profile-username a") %>%
      html_text()
  } else {
    userName <- url %>%
      html_nodes(".profile-username a") %>%
      html_text()
  }
  
  
  profile <- url %>%
    html_nodes(".post_body > .mycode_url") %>%
    html_text()
  profile <- tolower(c(profile))
  getProfile <- profile %>% as.data.frame() %>%
     separate(1, into = c("handle", "extra"), sep = "[?]")
  getProfile$handle <- gsub("https://www.twitter.com/", replacement = "", x = getProfile$handle)
  getProfile$handle <- gsub("http://www.twitter.com/", replacement = "", x = getProfile$handle)
  getProfile$handle <- gsub("https://twitter.com/", replacement = "", x = getProfile$handle)
  getProfile$handle <- gsub("www.twitter.com/", replacement = "", x = getProfile$handle)
  getProfile$handle <- gsub("https://mobile.twitter.com/", replacement = "", x = getProfile$handle)
  getProfile$handle <- gsub("/with_replies", replacement = "", x = getProfile$handle)
  getProfile$handle <- gsub("/", replacement = "", x = getProfile$handle)
  
  
  payment <- vector(mode ="integer", length=1)
  payInc <-1
  payTime <- 0 
  for (value in getProfile$handle){
    getuser <- rtweet::get_timeline(value, n = 150, check=FALSE, fast=TRUE)
    
    catchTwitter <- tryCatch(
      {
        filtered <- getuser %>% 
          select(created_at, screen_name,hashtags, text ,replyToSN = reply_to_status_id) %>%
          filter(created_at >= pastDate & created_at <= endDate & grepl("shlhockey", tolower(hashtags)) == TRUE)
        
        filtered$created_at <- filtered$created_at - 18000
        twitter_name <- filtered$screen_name[1]
        if (dim(filtered)[1] == 0) {
          payTime <- 0

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
            dplyr::summarise(Payout = as.double(format(sum(Payout), .groups = 'drop_last', scientific=F)))
          
          
          payTime <- temp$Payout
        }
      },
      error=function(cond){
        write(paste0("Error getting user: ", value),
              file = "Output\\Broken_Twitter_Accounts.txt",
              append=TRUE)
        payTime <- 0 
      },
      finally={
        payment[payInc] <- payTime
        payInc <- payInc + 1
      }
    )
  }
  data.frame(userName,getProfile$handle, payment)
  
  
  
  
  
}
SHL <- purrr::map_df(1:Pages, getlist)
write.csv(SHL, paste0(filePath, "Twitter", gsub(" 05:00:00", replacement = "", x = endDate),".csv" ), row.names = FALSE)

options(gargle_oauth_cache = ".secrets",gargle_oauth_email="Lukedamato99@gmail.com")

#gs4_auth(cache = ".secrets")



write_sheet(SHL,
            ss="https://docs.google.com/spreadsheets/d/1WrvrErL0IAviglyX3FnXsnsoSoRtXB36v_gRbjPeqTo/edit?usp=sharing",
            sheet = "Original"
            )

send_webhook_message("<@178319208839380992> Twitter is in the bank thread bitch")
}

##---------------------------------------------------------------------------
##        Checks for duplicates of twitter accounts and duplicate posts     -
##---------------------------------------------------------------------------

{
duplicateOccur <- data.frame(table(SHL$getProfile.handle))
duplicateOccur[duplicateOccur$Freq > 1,]
UserOccur <- data.frame(table(SHL$userName))
UserOccur[UserOccur$Freq > 1,]
}
