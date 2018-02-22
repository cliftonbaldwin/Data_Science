setwd("C:\\Users\\Clifton Baldwin\\Documents\\RStudio\\Case Studies")

#Loading the rvest package
library('rvest')

# Men's Curling Olympics 2018
participants <- "Men"
#Specifying the url for desired website to be scrapped
url <- 'http://results.worldcurling.org/Championship/DisplayResults?tournamentId=560&associationId=0&teamNumber=0&drawNumber=0'
# Exponents from 1.9 to 4.1 (by 0.1) were used in the computations, and 3.3 had the lowest mean-squared-error of that set
exponent <- 3.3 # The exponent to use for the Pythagorean Expectation


### Scrape the website for the data

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
rank_data_html <- html_nodes(webpage,'.game-table')

num_games = length(rank_data_html)
games <- matrix(nrow = num_games, ncol = 4)

# Scrape the data from the webpage
for (i in seq_len(num_games)) {
  this_result <- rank_data_html[i]
  teams <- html_text(html_nodes(this_result,".game-team"))
  scores <- html_text(html_nodes(this_result,".game-total"))
  games[i,c(1,2)] <- trimws(gsub('\r\n', '', teams))
  games[i,c(3,4)] <- strtoi(trimws(gsub('\r\n', '', scores)))
}

rm(i, scores, teams, this_result, num_games, rank_data_html, webpage, url)

#######
## The data has been scraped, and now we can analyze it
#######
# Take a subset of the games that have been played (used in all sections)
completeGames <- games[!is.na(games)[,3],]

# Get a list of the teams that are playing
teams <- unique(games[,1])
if (!is.na(pmatch("To Be", teams))) teams = teams[-length(teams)]

# Take a subset of the games that are scheduled to come
scheduledGames <- matrix(nrow=nrow(games[is.na(games)[,3],]), ncol = 6)
scheduledGames[,1] <- games[is.na(games)[,3],1]
scheduledGames[,4] <- games[is.na(games)[,3],2]
scheduledGames <- scheduledGames[!grepl("To Be", scheduledGames[,1]),]

# Get a list of the teams that are scheduled to play
upcomingTeams <- unique(c(scheduledGames[,1],scheduledGames[,4]))

# Clean up
rm(games)

# Determine each team's standings to date

standings = matrix(nrow=length(teams), ncol = 5)
count <- 1L
for(team in teams) {
  wins <- sum(strtoi(completeGames[grepl(team, completeGames[,1]),3]) > strtoi(completeGames[grepl(team, completeGames[,1]),4])) +
    sum(strtoi(completeGames[grepl(team, completeGames[,2]),4]) > strtoi(completeGames[grepl(team, completeGames[,2]),3]))
  losses <- sum(strtoi(completeGames[grepl(team, completeGames[,1]),3]) < strtoi(completeGames[grepl(team, completeGames[,1]),4])) +
    sum(strtoi(completeGames[grepl(team, completeGames[,2]),4]) < strtoi(completeGames[grepl(team, completeGames[,2]),3]))
  standings[count,1] <- team
  standings[count,2] <- wins
  standings[count,3] <- losses
#  standings[count,4] <- wins / (wins + losses)
  standings[count,4] <- paste0(formatC(100 * (wins / (wins + losses)), format = "f", digits = 2), "%")
  count <- count + 1L
  print(paste(team, wins, losses, sep = ","))
}

# Clean up
rm(count, wins, losses, team)

# Compute the Pythagorean Expectation for each team
count <- 0L
for (team in teams) {
    count <- count + 1L
    scored <- sum(strtoi(completeGames[grepl(team, completeGames[,1]),3])) +
        sum(strtoi(completeGames[grepl(team, completeGames[,2]),4]))
    allowed <- sum(strtoi(completeGames[grepl(team, completeGames[,1]),4])) +
        sum(strtoi(completeGames[grepl(team, completeGames[,2]),3]))
    pythagorean <- (scored^exponent) / ((scored^exponent) + (allowed^exponent))
    pythagorean <- paste0(formatC(100 * pythagorean, format = "f", digits = 2), "%")
    standings[count,5] <- pythagorean
    print(paste(team, pythagorean, sep=" = "))
}

standings <- as.data.frame(standings)
names(standings) <- c("Team", "Wins", "Losses", "Standing", "Pythagorean")

fname <- paste(participants,"Standings", Sys.Date(), ".csv", sep = "")
write.csv(standings, fname)

rm(fname, count, pythagorean, allowed, scored, team, exponent, completeGames)

# List the upcoming matches with the probabilities
for (team in upcomingTeams) {
    scheduledGames[grep(team, scheduledGames[,1]),2] <- as.character(standings[grep(team, standings[,1]),4])
    scheduledGames[grep(team, scheduledGames[,1]),3] <- as.character(standings[grep(team, standings[,1]),5])
    scheduledGames[grep(team, scheduledGames[,4]),5] <- as.character(standings[grep(team, standings[,1]),4])
    scheduledGames[grep(team, scheduledGames[,4]),6] <- as.character(standings[grep(team, standings[,1]),5])
}
rm(team)

scheduledGames <- as.data.frame(scheduledGames)
names(scheduledGames) <- c("Team1", "Standings1", "Pythagorean1", "Team2", "Standings2", "Pythagorean2")

fname <- paste(participants,"ScheduledMatches", Sys.Date(), ".csv", sep = "")
write.csv(scheduledGames, fname)
rm(fname, participants, teams, upcomingTeams, scheduledGames, standings)
