PipeOpWinsorizeSimple = R6::R6Class(
  "PipeOpWinsorizeSimple",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    groups = NULL,
    initialize = function(id = "winsorization", param_vals = list()) {
      ps = ParamSet$new(list(
        ParamDbl$new("probs_low", lower = 0, upper = 1, default = 0.05, tags = c("winsorize_tag")),
        ParamDbl$new("probs_high", lower = 0, upper = 1, default = 0.95, tags = c("winsorize_tag")),
        ParamLgl$new("na.rm", default = TRUE, tags = c("winsorize_tag")),
        ParamInt$new("qtype", lower = 1L, upper = 9L, default = 7L, tags = c("winsorize_tag"))
      ))
      ps$values = list(qtype = 7L)
      super$initialize(id, param_set = ps, param_vals = param_vals, feature_types = c("numeric"))
    }
  ),

  private = list(

    .get_state_dt = function(dt, levels, target) {
      # debug
      # task = copy(tsk_aroundzero_month)
      # dt = tsk_aroundzero_month$data()
      # cols = tsk_aroundzero_month$feature_types[type %in% c("numeric", "integer"), id]
      # dt = dt[, ..cols]
      # pv = list(
      #   probs_low = 0.01, probs_high = 0.99, na.rm = TRUE, qtype = 7
      # )
      # self = list()

      # params
      pv = self$param_set$get_values(tags = "winsorize_tag")

      # state variables
      q = dt[, lapply(.SD,
                      quantile,
                      probs = c(pv$probs_low, pv$probs_high),
                      na.rm = pv$na.rm,
                      type = pv$qtype)]
      list(
        minvals = q[1],
        maxvals = q[2]
      )
    },

    .transform_dt  = function(dt, levels) {
      dt = dt[, Map(function(a, b) ifelse(a < b, b, a), .SD, self$state$minvals)]
      dt = dt[, Map(function(a, b) ifelse(a > b, b, a), .SD, self$state$maxvals)]
      dt
    }
  )
)
