###############################################################
# Read in regular season data from 2006-2007 to 2018-2019:
x <- read.csv("https://raw.githubusercontent.com/tvbassine/NBA_Tanking_Fun/master/data_2007_2019.csv", stringsAsFactors = F)
# Any signs of tanking?

# Get final record of each team:
x$final_wins_h <- 0
x$final_wins_a <- 0

tms <- unique(x$away_year)

for(i in tms){
  ind_a <- which(x$away_year == i)
  ind_h <- which(x$home_year == i)
  
  wins <- sum(x$h_margin[ind_a] < 0) + sum(x$h_margin[ind_h] > 0)
  x$final_wins_h[ind_h] = wins
  x$final_wins_a[ind_a] = wins
}

# Order schedule by date:
x$dateR <- as.Date(x$dateR)
x <- x[order(x$dateR),]
head(x)
tail(x)

# Are the home or away teams "bad" (less than 34 total season wins)
x$away_bad <- ifelse(x$final_wins_a < 34, 1, 0)
x$home_bad <- ifelse(x$final_wins_h < 34, 1, 0)

# Keep going from here:
x$bad_win <- 0
x$bad_win <- ifelse((x$home_bad == 1 & x$h_margin > 0) |
                    (x$away_bad == 1 & x$h_margin < 0),
                    1, 0)

# Get month:
x$month <- lubridate::month(x$dateR)

# Restrict to only games in which one of the teams is a bad team:
y <- x[((x$away_bad + x$home_bad) == 1) & x$year >= 2015,]

# Games before Jan
y_early <- y[y$month %in% c(10:12),]

# Games in March and April
y_late <- y[y$month %in% c(3:4),]

# Bad team records early in the season:
mean(y_early$bad_win)
table(y_early$bad_win)
mean(y_late$bad_win)
table(y_late$bad_win)

# Two-sample z-test:
prop.test(x = c(300, 149), n = c(1145, 689))
