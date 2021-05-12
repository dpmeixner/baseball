#' Title
#'
#' @param play_by_play
#' @param group
#'
#' @return
#' @importFrom stats aggregate
#' @export
#'
#' @examples
groupAvg <- function(play_by_play, group) {
  return(aggregate(play_by_play$eventType == EVENT_K,
                   as.list(play_by_play[group]),
                   mean))
}

#' Title
#'
#' @param data_set
#'
#' @return
#' @importFrom dplyr "%>%" count inner_join rename
#' @export
#'
#' @examples
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
