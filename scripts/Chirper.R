suppressMessages(suppressWarnings(require(rsconnect)))
suppressMessages(suppressWarnings(require(tidyverse)))  
suppressMessages(suppressWarnings(require(dplyr)))
suppressMessages(suppressWarnings(require(plyr)))
suppressMessages(suppressWarnings(require(purrr)))
suppressMessages(suppressWarnings(require(discordr)))
suppressMessages(suppressWarnings(require(rvest)))
suppressMessages(suppressWarnings(require(httr)))
suppressMessages(suppressWarnings(require(jsonlite)))


conn_obj <- create_discord_connection(webhook = "https://discord.com/api/webhooks/991361873784082432/K_GG_WfkpdjBlJaK9VJtuqVu7L9zyrXUMqMt1tD2cHmQLyxCU83N-HYEisDmfenOm6MH" , username = 'BANKER', set_default = TRUE)
filePath <- "Output\\"

url <- 'https://simulationhockey.com/chirperapi.php'
#doing goofy shit to get jsonData into dataframe
res <- GET(url)
data <- rawToChar(res$content)
data <- str_remove(data, "</body>")
data <- str_remove(data, "<head></head><body>")
data <- gsub(" ", "", data)

data <- gsub("\n", "", data)

data <- fromJSON(data)
data <- data
data$datetime <- as.POSIXct(data$datetime, tz="EST", origin="1970-01-01") 
data$datetime <- as.Date(data$datetime, format ="%m/%d/%y")
#done the goofy shit

#getting the past week of data
today <- Sys.Date() - 7
data <- data %>%
  filter(as.Date(datetime) >= as.Date(today) )

#grading the past week for users
uniqueUsers <- unique(data$username)
finalPayout <- data.frame(col1="UserName", col2="Payout")

for (i in 1:length(uniqueUsers)){
  userData <- data %>%
    filter(username == uniqueUsers[i])
  payoutDF <- plyr::count(as.character(userData$datetime))
  payoutDF$freq[payoutDF$freq > 2] <- 2
  if( sum(payoutDF$freq) > 6){
    payout <- 600000
  } else {
    payout <- sum(payoutDF$freq) * 100000
  }
  #putting the payouts into a data frame
  finalPayout[i,] <- c(uniqueUsers[i],payout)
}
write.csv(finalPayout, paste0(filePath, "Chirper-", today ,".csv" ), row.names = FALSE)
quotes <- read.csv("inspiration.csv", header=TRUE, sep =";", row.names=NULL)
quotes <- quotes %>%
  filter(GENRE == "funny")
quote <- sample_n(quotes, 1)
send_webhook_message("<@178319208839380992> Chirper is in the bank thread")
send_webhook_file(paste0(filePath, "Chirper-", today ,".csv" ))
send_webhook_message(paste0(quote$QUOTE, "\n -", quote$AUTHOR))


