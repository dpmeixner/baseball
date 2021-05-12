# Examination of using log5 and logistic regression to predict outcome of
# matchups between a given pitcher and batter. Initially uses the averages for
# each player, but more complex models are also evaluated using additional
# factors
##########

#source('globals.R')
#source('util.R')
#source('analysis.R')

# Load the data from the database
queryDB <- function(query) {
  mydb = dbConnect(MySQL(), user='user', password='password', dbname='mlb',
                   host='127.0.0.1')
  rs = dbSendQuery(mydb, query)
  res = dbFetch(rs, n=-1)
  dbClearResult(rs)
  dbDisconnect(mydb)
  return(res)
}

loadRetrosheetData <- function() {
  play_by_play = queryDB(sprintf("SELECT *
                                 FROM PlayByPlay
                                 WHERE SUBSTR(gameId,4,4)
                                 BETWEEN %s AND %s",
                                 START_YEAR, END_YEAR))
  play_by_play$year = as.integer(substr(play_by_play$gameId, 4, 7))

  # Filter on only plate apperances and features needed for this model
  columns = c('gameId', 'year', 'batter', 'batterHand', 'pitcher', 'pitcherHand', 'eventType')
  play_by_play = play_by_play[play_by_play$eventType %in% EVENT_PA, columns]
}

# Load the play-by-play data
play_by_play <- read.csv(unz("playByPlay.zip", "playByPlay.csv"))

# Calculate season averages for players and league
play_by_play = addAverages(play_by_play)

# Predict season averages for players and league
play_by_play = addForecast(play_by_play)

# Visually inspect how well forecast matches actual values
evaluateForecast(play_by_play)

# Split into training and testing sets
set.seed(1)
train_rows = sort(sample(nrow(play_by_play), nrow(play_by_play)*.8))
train_data = play_by_play[train_rows,]
test_data = play_by_play[-train_rows,]

# Log5 model with perfect knowledge of season averages
eval.log5 = evaluateModel(train_data, test_data,
                          'batterAvg', 'pitcherAvg', 'lgAvg',
                          model=log5Prediction)

# GLM with perfect knowledge of season averages
eval.glm = evaluateModel(train_data, test_data,
                         'batterAvg', 'pitcherAvg', 'lgAvg',
                         model=glmPrediction)

# Log5 model with forecasted season averages
eval.log5_fc = evaluateModel(train_data, test_data,
                             'batterForecast', 'pitcherForecast', 'leagueForecast',
                             model=log5Prediction)

# GLM with forecasted season averages
eval.glm_fc = evaluateModel(train_data, test_data,
                            'batterForecast', 'pitcherForecast', 'leagueForecast',
                            model=glmPrediction)

# Compare Brier Score between models
# Reference forecast is leagure forecast avreage
bs_ref = BrierScore(test_data$eventType == EVENT_K, test_data$leagueForecast)
printBrierSummary(list(eval.log5, eval.glm, eval.log5_fc, eval.glm_fc), bs_ref)

# Compare coefficients between models
# Model is of form P = x*B + y*P + z*L, where:
#   P = probability of strikeout
#   B/P/L = batter/pitcher/league strikeout season average
printCoefSummary(list(eval.log5, eval.glm, eval.log5_fc, eval.glm_fc))

# Display verficiation graphs to visualize how well models perform
reliability.plot(eval.log5$verification, titl='Log5 Model', legend.names='')
reliability.plot(eval.glm$verification, titl='GLM Model', legend.names='')
reliability.plot(eval.log5_fc$verification, titl='Log5 Forecast', legend.names='')
reliability.plot(eval.glm_fc$verification, titl='GLM Forecast', legend.names='')

