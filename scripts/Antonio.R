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

conn_obj <- create_discord_connection(webhook = "https://discord.com/api/webhooks/989236566427832320/QeNAhpXULjACvgfeQzqFAhJA1rIg6oNFVVBq0BjTM5-lBBKVeC7Q-zq5B15kUNNaWwn4" , username = 'Is Antonio Funny', set_default = TRUE)

api_key <- "qiuhpw6xWBxLvhkbqlofyoN4m"
 api_secret_key <- "hOE5NGbaU3uSStw7o08eaPjeaR4iZ4MKzMLiLDrX2SjsswnosK"
 access_token <- "3270729511-UngtR9d7pPPNGWq6ZietCiZ1DminaRpxEsHuPIc"
 access_token_secret <- "qlUU8WbSswh0IrFYDhO888iJGZGmU291WH6USb5pifDAW"
#api_key <- Sys.getenv("API_KEY")
#api_secret_key <- Sys.getenv("API_SECRET_KEY")
#access_token <-  Sys.getenv("ACCESS_TOKEN")
#access_token_secret <- Sys.getenv("ACCESS_TOKEN_SECRET")

## authenticate via web browser
token <- create_token(
  app = "rstatsjournalismresearch",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)


endDate <- as.POSIXct(paste0(Sys.Date(), "5:00:00"))
pastDate <- endDate - 622800
#winstonwhirloo2
#freakyFPS
freaky <- rtweet::get_timeline("freakyFPS", n = 150, check=FALSE, fast=TRUE)

freakyFiltered <- filtered <- freaky %>% 
  select(created_at, is_retweet, screen_name,favorite_count,retweet_count,hashtags) %>%
  filter(created_at >= pastDate & is_retweet=="FALSE" )

sum(freakyFiltered$favorite_count)
sum(freakyFiltered$retweet_count)
nrow(freakyFiltered)

topFreaky <- freaky[which.max(freaky$favorite_count),]


send_webhook_message(paste0("For freakyFPS from the past week you achieved \n", sum(freakyFiltered$favorite_count), " Likes \n",
                            sum(freakyFiltered$retweet_count), " Retweet(s) \n", "From ", nrow(freakyFiltered), " Tweets " ))
send_webhook_message(paste0("Best Tweet\n", "https://twitter.com/freakyFPS/status/",topFreaky$status_id))

winstonwhirloo2 <- rtweet::get_timeline("winstonwhirloo2", n = 150, check=FALSE, fast=TRUE)

winstonwhirloo2Filtered <- filtered <- winstonwhirloo2 %>% 
  select(created_at, is_retweet, screen_name,favorite_count,retweet_count,hashtags) %>%
  filter(created_at >= pastDate & is_retweet=="FALSE" )

sum(winstonwhirloo2Filtered$favorite_count)
sum(winstonwhirloo2Filtered$retweet_count)
nrow(winstonwhirloo2Filtered)

topWinston <- winstonwhirloo2[which.max(winstonwhirloo2$favorite_count),]

send_webhook_message(paste0("\n\n For winstonwhirloo2 from the past week you achieved \n", sum(winstonwhirloo2Filtered$favorite_count), " Likes \n",
                            sum(winstonwhirloo2Filtered$retweet_count), " Retweet(s) \n", "From ", nrow(winstonwhirloo2Filtered), " Tweets " ))

send_webhook_message(paste0("Best Tweet\n", "https://twitter.com/winstonwhirloo2/status/", topWinston$status_id))
