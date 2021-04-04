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

groupAvg <- function(play_by_play, group) {
  return(aggregate(play_by_play$eventType == EVENT_K, 
                   as.list(play_by_play[group]), 
                   mean))
}

addAverages <- function(data_set) {
  # Get number of PAs for batters and pitchers to filter out low use players
  batter_pa_cnt = data_set %>% count(batter, year, name = 'batterPA')
  pitcher_pa_cnt = data_set %>% count(pitcher, year, name = 'pitcherPA')
  
  data_set = 
    data_set %>%
    inner_join(batter_pa_cnt[batter_pa_cnt$batterPA > MIN_PA,], by=c('batter', 'year')) %>%
    inner_join(pitcher_pa_cnt[pitcher_pa_cnt$pitcherPA > MIN_PA,], by=c('pitcher', 'year'))
  
  batter_avgs = groupAvg(data_set, c('batter', 'year'))
  pitcher_avgs = groupAvg(data_set, c('pitcher', 'year'))
  league_avgs = groupAvg(data_set, c('year'))
  
  data_set = 
    data_set %>%
    inner_join(batter_avgs, by=c('batter', 'year')) %>%
    inner_join(pitcher_avgs, by=c('pitcher', 'year')) %>%
    inner_join(league_avgs, by='year') %>%
    rename(batterAvg = x.x, pitcherAvg = x.y, lgAvg = x)
  
  return(data_set)
}