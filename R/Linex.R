#' @title Linex Regression Measure
#'
#' @description
#' Measure for regression tasks that calculates the mean absolute error using the Linex loss function.
#' The Linex loss function is a combination of the mean absolute error and mean squared error.
#'
#' @format A R6 generator object
#' @export Linex
Linex = R6::R6Class(
  "Linex",
  inherit = mlr3::MeasureRegr,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        # custom id for the measure
        id = "linex",

        # additional packages required to calculate this measure
        packages = character(),

        # properties, see below
        properties = character(),

        # required predict type of the learner
        predict_type = "response",

        # feasible range of values
        range = c(0, Inf),

        # minimize during tuning?
        minimize = TRUE
      )
    }
  ),

  private = list(
    # custom scoring function operating on the prediction object
    .score = function(prediction, ...) {
      linex = function(truth, response, a1 = 1, a2 = -1) {
        mlr3measures:::assert_regr(truth, response = response)
        if (a2 == 0) stop("Argument a2 can't be 0.")
        if (a1 <= 0) stop("Argument a1 must be greater than 0.")
        e = truth - response
        mean(abs(a1 * (exp(-a2*e) - a2*e - 1)))
      }

      linex(prediction$truth, prediction$response)
      # linex(c(0, 0.5, 1), c(0.5, 0.5, 0.5))
      # root_mse = function(truth, response) {
      #   mse = mean((truth - response)^2)
      #   sqrt(mse)
      # }
      #
      # root_mse(prediction$truth, prediction$response)
      # root_mse(c(0, 0.5, 1), c(0.5, 0.5, 0.5))
    }
  )
)
