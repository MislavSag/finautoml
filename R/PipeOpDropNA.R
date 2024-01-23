#' @export
PipeOpDropNA = R6::R6Class(
  "PipeOpDropNA",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "drop.na") {
      super$initialize(id)
    }
  ),

  private = list(
    .train_task = function(task) {
      self$state = list()
      private$compute_exclude(task)
    },

    .predict_task = function(task) {
      private$compute_exclude(task)
    },

    compute_exclude = function(task) {
      featuredata = task$data(cols = task$feature_names)
      exclude = apply(is.na(featuredata), 1, any)
      task$filter(task$row_ids[!exclude])
    }
  )
)
