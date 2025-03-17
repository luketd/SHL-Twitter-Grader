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

chirperNum <- readRDS("chirperNum")


url <- 'https://simulationhockey.com/chirperapi.php'
#doing goofy shit to get jsonData into dataframe
res <- GET(url)
data <- rawToChar(res$content)

# More robust cleanup
data <- gsub("<head></head><body>|</body>", "", data)
data <- gsub("\n", " ", data)
data <- gsub("\\\\", "", data)
# Clean up any potential invalid characters
data <- gsub("[[:cntrl:]]", "", data)
# Handle quotes more carefully
data <- gsub('(?<!\\\\)"', '\\"', data, perl = TRUE)

# Add error handling for JSON parsing
tryCatch({
  data <- fromJSON(data)
}, error = function(e) {
  message("JSON parsing error: ", e$message)
  # You might want to add logging here
  return(NULL)
})
data$datetime <- as.POSIXct(data$datetime, tz="PST8PDT", origin="1970-01-01") 
data$datetime <- as.Date(format(data$datetime, format ="%Y-%m-%d"))
#done the goofy shit

#getting the past week of data
today  <- Sys.Date() - 7
sunday <- Sys.Date() - 1
data <- data %>%
  filter(as.Date(datetime) >= as.Date(today) &  as.Date(datetime) <= as.Date(sunday))

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

finalPayout <- finalPayout[order(finalPayout$col2),]
finalPayout[, 1] <- gsub("&quot;", "\"", finalPayout[, 1])

# Assuming finalPayout is your data frame and other variables are defined

# Create a new data frame with the desired column names
newData <- data.frame(username = finalPayout$col1, amount = finalPayout$col2)


# Write the new data frame to a CSV file with specified headers
write.csv(newData, paste0(filePath, "Chirper-", chirperNum, "-", today ,".csv"), row.names = FALSE)
quotes <- read.csv("inspiration.csv", header=TRUE, sep =";", row.names=NULL)
quotes <- quotes %>%
  filter(GENRE == "funny")
quote <- sample_n(quotes, 1)
send_webhook_message("<@709843103040798750> Chirper is in the bank thread")
send_webhook_file(paste0(filePath, "Chirper-", chirperNum, "-", today ,".csv" ))
send_webhook_message(paste0(quote$QUOTE, "\n -", quote$AUTHOR))

chirperNum <- chirperNum + 1
saveRDS(chirperNum, "chirperNum")

