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

previousYearForecast <- function(play_by_play, group) {
  season_avg = groupAvg(play_by_play, group)
  season_avg$year = season_avg$year+1
  
  return(season_avg)
}

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

printBrierSummary <- function(model_list, bs_ref) {
  brier_summary = cbind(sapply(model_list, "[[", 'brierscore'))
  brier_summary = cbind(brier_summary, 1-brier_summary[,1]/bs_ref)
  colnames(brier_summary) = c( 'Brier Score', 'Skill Score')
  rownames(brier_summary) = c('log5','glm','log5_fc','glm_fc')
  print(brier_summary)
}

printCoefSummary <- function(model_list) {
  coef_summary = t(sapply(model_list, "[[", 'coefficients'))
  rownames(coef_summary) = c('log5','glm','log5_fc','glm_fc')
  print(coef_summary)
}