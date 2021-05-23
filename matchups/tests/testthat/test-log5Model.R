test_that("log5 model works", {
  train_data <- NULL
  test_data <- data.frame('pitcher'=.200,
                          'batter'=.300,
                          'league'=.250)

  res = log5Prediction(train_data, test_data, 'pitcher', 'batter', 'league')

  expect_setequal(res$coefficients, c(1,1,-1))
  expect_equal(res$prediction, 0.24324324)
})

test_that("glm model works", {
  train_data <- data.frame('pitcher'=c(.350,.250),
                          'batter'=c(.300,.200),
                          'league'=c(.250,.250),
                          'eventType'=c(1,0))
  test_data <- data.frame('pitcher'=.200,
                          'batter'=.300,
                          'league'=.250)

   res = glmPrediction(train_data, test_data,
                       x='pitcher', y='batter', z='league')

   expect_setequal(res$prediction, 1)
   expect_equal(res$coefficients, c('B'=-87.7752862,
                                    'P'=78.0982146,
                                    'L'=10.6770716))
})
