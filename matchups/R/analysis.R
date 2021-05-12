#' The log5 method
#'
#' @description This calculates the predicted outcome between two opponents
#'   based on their individual rates for that event. Originally derived by Bill
#'   James
#' @param train_data not used in this function, but required for compatibility
#'   with generic models
#' @param test_data the dataset containing the required 3 columns
#' @param c1 name of column in test_data containing rates for the first opponent
#'   (typically the pitcher)
#' @param c2 name of column in test_data containing rates for the second
#'   opponent (typically the batter)
#' @param c3 name of column in test_data containing rates for the population
#'   (typicallly the league)
#'
#' @return a dataset consisting of the predicted outcome based on the log5
#'   formula and coefficients which are all 1 for the log 5 model
#' @export
#'
#' @examples log5Prediction(NULL, data.frame('pitcher'=.200,
#'                                           'batter'=.300,
#'                                           'league'=.250),
#'                          'pitcher', 'batter', 'league')
log5Prediction <- function(train_data, test_data, c1, c2, c3) {
  # Calculate predicted values using log5
  # Formual is x*y/z / [x*y/z + (1-x)*(1-y)/(1-z)]
  x = test_data[,c1]
  y = test_data[,c2]
  z = test_data[,c3]
  result = NULL
  result$prediction = (x*y/z / (
                         x*y/z + (1-x)*(1-y)/(1-z)
                         )
                      )
  result$coefficients = c(x=1, y=1, z=-1)
  return(result)
}

#' Eevent prediction using logistic regression
#'
#' @description Use logistic regression to calculate the predicted outcome of an
#'   event between two opponents given their individual rates for that event
#'
#' @param train_data The set of data to train the emodel on
#' @param test_data The set of data to caluclate predicted outcomes
#' @param x The name of the column for the first opponent (e.g. pitcher)
#' @param y The name of the column for the second opponent (e.g. batter)
#' @param z The name of the column for the population average (e.g. league)
#'
#' @return A result containing the model that was derived, the predicted
#'   outcomes, and the coefficients for the model (useful for comparing to log5
#'   model which has coefficientts (1,1,1))
#' @importFrom stats as.formula glm binomial plogis predict
#' @export
glmPrediction <- function(train_data, test_data, x=x, y=y, z=z) {
  # Constraint that E=L if B=P=L. This is equivalent to
  #     E = c0*B + c1*P + c2*L, with c0+c1+c2=1
  #       = c0*B + c1*P + (1-c0-c1)*L
  #       = c0*(B-L) + c1*(P-L) + L
  # -1 sets the intercept to zero
  data_set = train_data
  formula = as.formula(sprintf("eventType == EVENT_K ~ -1 +
                               I(qlogis(%s) - qlogis(%s)) +
                               I(qlogis(%s) - qlogis(%s)) +
                               offset(qlogis(%s))", x, z, y, z, z))
  model = glm(formula, family=binomial, data=data_set)

  result = NULL
  result$model = model
  data_set = test_data
  result$prediction = plogis(predict(model, newdata=data_set))

  result$coefficients = model$coefficients
  result$coefficients[3] = 1 - sum(model$coefficients)
  names(result$coefficients) = c('B', 'P', 'L')

  return(result)
}

#' Add forecast using previous year's average
#'
#' @param play_by_play The dataset containing event data across the league
#' @param group The group by which averages should be calculated
#'
#' @return The previous year's averages, which will be used as the forecast for
#'   this year's averages
#' @export
previousYearForecast <- function(play_by_play, group) {
  season_avg = groupAvg(play_by_play, group)
  season_avg$year = season_avg$year+1

  return(season_avg)
}

#' Simple forecast using previous year's values
#'
#' @description Adds a forecast based on the previous year's values
#'
#' @param play_by_play The dataset to which the averages will be added
#'
#' @return
#' @importFrom dplyr inner_join rename
#' @export
addForecast <- function(play_by_play) {
  batter_season_avg = previousYearForecast(play_by_play, group=c('batter', 'year'))
  pitcher_season_avg = previousYearForecast(play_by_play, group=c('pitcher', 'year'))
  league_season_avg = previousYearForecast(play_by_play, group=c('year'))

  # Merge the forecast into the data
  data_set =
    play_by_play %>%
    inner_join(batter_season_avg, by=c('batter', 'year')) %>%
    inner_join(pitcher_season_avg, by=c('pitcher', 'year')) %>%
    inner_join(league_season_avg, by=c('year')) %>%
    rename(batterForecast = x.x, pitcherForecast = x.y, leagueForecast = x)
}

#' Graphically evaluate a forecast
#'
#' @description Plots the predicted vs. actual values to visually see how well
#'   the model performed.
#'
#' @param play_by_play The dataset containing actual and predicted values for an
#'   event
#'
#' @return
#' @importFrom dplyr distinct
#' @importFrom ggplot2 ggplot geom_point aes arrow unit scale_x_continuous ylab
#'   scale_x_continuous scale_colour_manual qplot stat_smooth geom_abline
#'   geom_text geom_segment
#' @importFrom gridExtra grid.arrange
#' @importFrom stats lm
#' @export
evaluateForecast <- function(play_by_play) {
  leagueAvgs = distinct(play_by_play[c('year', 'lgAvg', 'leagueForecast')])
  league_plot <- ggplot(data=leagueAvgs) +
    geom_point(size=3, aes(x=year, y=leagueForecast, color='red')) +
    geom_point(size=3, aes(x=year, y=lgAvg, color='blue')) +
    geom_segment(aes(x=year, y=leagueForecast, xend=year, yend=lgAvg),
                 arrow=arrow(length=unit(0.3,"cm"), type='closed'), size=1) +
    scale_x_continuous('', breaks=leagueAvgs$year) +
    ylab('') +
    scale_colour_manual(name='', values=c('red'='red','blue'='blue'), labels = c('Actual','Forecast'))
  print(league_plot)

  batterAvgs = distinct(play_by_play[c('batterForecast', 'batterAvg')])
  r2 = summary(lm(batterAvgs))$r.squared
  batter_lbl = substitute(~~italic(r)^2~"="~r2, list(r2=format(r2, digits=3)))
  p1 <- qplot(batterAvgs$batterForecast, batterAvgs$batterAvg) +
    stat_smooth(method='lm', se=FALSE, col='blue') +
    geom_abline(slope=1, intercept=0, col='red') +
    geom_text(aes(x=.37, y=.4, label='x=y'), col='red') +
    geom_text(aes(x=.35, y=.28, label=as.character(as.expression(batter_lbl))),
              col='blue', parse=TRUE)

  pitcherAvgs = distinct(play_by_play[c('pitcherForecast', 'pitcherAvg')])
  r2 = summary(lm(pitcherAvgs))$r.squared
  pitcher_lbl = substitute(~~italic(r)^2~"="~r2, list(r2=format(r2, digits=3)))
  p2 <- qplot(pitcherAvgs$pitcherForecast, pitcherAvgs$pitcherAvg) +
    stat_smooth(method='lm', se=FALSE, col='blue') +
    geom_abline(slope=1, intercept=0, col='red') +
    geom_text(aes(x=.37, y=.4, label='x=y'), col='red') +
    geom_text(aes(x=.35, y=.27, label=as.character(as.expression(pitcher_lbl))),
              col='blue', parse=TRUE)

  grid.arrange(p1, p2, nrow=1)
}

#' Evaluate matchup models
#'
#' @param train_data The data to train the model on
#' @param test_data The data ot test the model on
#' @param x The column name for the first opponent (e.g. pitcher)
#' @param y The column name for the second opponent (e.g. batter)
#' @param z The colunn name for the global average (e.g. league)
#' @param model A model object that will be evaluated
#'
#' @return A plot showing how well predictions match actual values, and a Brier Score which is a numerical value that can be used to compare across various models
#' @importFrom verification verify reliability.plot
#' @importFrom DescTools BrierScore
#' @export
evaluateModel <- function(train_data, test_data, x, y, z, model) {
  result = model(train_data, test_data, x, y, z)

  result$verification <- verify(test_data$eventType == EVENT_K,
                                result$prediction,
                                thresholds = seq(0,1,.05),
                                frcst.type = "prob", obs.type = "binary",
                                show=FALSE)

  result$plot = reliability.plot(result$verification, title = "Alternative plot")

  result$brierscore = BrierScore(test_data$eventType == EVENT_K, result$prediction)

  return(result)
}

#' Print summary of Brier Scores
#'
#' @param model_list A list of objects with a $brierscore attribute
#' @param bs_ref The reference brier score
#'
#' @return
#' @export
printBrierSummary <- function(model_list, bs_ref) {
  brier_summary = cbind(sapply(model_list, "[[", 'brierscore'))
  brier_summary = cbind(brier_summary, 1-brier_summary[,1]/bs_ref)
  colnames(brier_summary) = c( 'Brier Score', 'Skill Score')
  rownames(brier_summary) = c('log5','glm','log5_fc','glm_fc')
  print(brier_summary)
}

#' Print summary of coefficients
#'
#' @param model_list A list of objects with $coefficieints attribute
#'
#' @return
#' @export
printCoefSummary <- function(model_list) {
  coef_summary = t(sapply(model_list, "[[", 'coefficients'))
  rownames(coef_summary) = c('log5','glm','log5_fc','glm_fc')
  print(coef_summary)
}
