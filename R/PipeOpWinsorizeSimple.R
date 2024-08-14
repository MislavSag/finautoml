#' @export PipeOpWinsorizeSimple
PipeOpWinsorizeSimple = R6::R6Class(
  "PipeOpWinsorizeSimple",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    groups = NULL,
    initialize = function(id = "winsorization", param_vals = list()) {
      ps = ps(
        probs_low = p_dbl(default = 0.05, lower = 0, upper = 1, tags = "winsorize_tag"),
        probs_high = p_dbl(default = 0.95, lower = 0, upper = 1, tags = "winsorize_tag"),
        na_rm = p_lgl(default = TRUE, tags = "winsorize_tag"),
        qtype = p_int(default = 7L, lower = 1L, upper = 9L, tags = "winsorize_tag")
      )
      ps$values = list(qtype = 7L, na.rm = TRUE, probs_low = 0.95, probs_high = 0.05)
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
      dt = dt[, Map(function(a, b) data.table::fifelse(a < b, b, a), .SD, self$state$minvals)]
      dt = dt[, Map(function(a, b) data.table::fifelse(a > b, b, a), .SD, self$state$maxvals)]
      dt
    }
  )
)
