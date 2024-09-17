`%check&&%` = function(lhs, rhs) {
  if (!isTRUE(lhs) && !isTRUE(rhs)) return(paste0(lhs, ", and ", rhs))
  if (isTRUE(lhs)) rhs else lhs
}

# check the `filter_formula` parameter of PipeOpFilterRows
# @param x [formula] whatever `filter_formula` is being set to
# checks that `filter_formula` is `formula` with only a rhs (or NULL)
check_filter_formulae = function(x) {
  check_formula(x, null.ok = TRUE) %check&&%
    if (!is.null(x) && length(x) != 2L) {
      sprintf("formula %s must not have a left hand side.",
              deparse(x, nlines = 1L, width.cutoff = 500))
    } else {
      TRUE
    }
}

# helper function to filter a task based on a formula
# the formula is evaluated within the frame of the data.table backend of a task where .SDcols is set to SDcols
# (but only if required)
# @param task [Task]
# @param frm [formula]
# @param SDcols [character]
filter_task = function(task, frm, SDcols) {
  row_ids = if (any(grepl(".SD", x = frm[[2L]]))) {
    task$row_ids[which(task$data()[, (eval(frm[[2L]], envir = as.list(environment(frm)))), .SDcols = SDcols])]
  } else {
    task$row_ids[which(task$data()[, (eval(frm[[2L]], envir = as.list(environment(frm))))])]
  }
  task$filter(row_ids)
}


#' @title PipeOpFilterRows
#'
#' @usage NULL
#' @name mlr_pipeops_filterrows
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Filter rows of the data of a [`Task`][mlr3::Task].
#'
#' @section Construction:
#' ```
#' PipeOpFilterRows$new(id = "filterrows", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)` \cr
#'   Identifier of resulting object, default `"filterrows"`.
#' * `param_vals` :: named `list` \cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `filter_formula` :: `formula` | `NULL` \cr
#'   Expression of the filtering to be performed, in the form of a `formula` that evaluates to `TRUE` or `FALSE`
#'   for each row within the frame of the [`data.table`] [`DataBackend`][mlr3::DataBackend] of the [`Task`][mlr3::Task].
#'   Rows for which the evaluation is `TRUE` are kept in the output [`Task`][mlr3::Task], others are removed.
#'   Initialized to `NULL`, i.e., no filtering is performed and all rows are kept.
#' * `SDcols` :: `function` | [`Selector`] \cr
#'   [`Selector`] function, takes a [`Task`][mlr3::Task] as an argument and returns a `character` vector of features.
#'   This character vector is set as the `.SDcols` argument when the formula above is evaluated within the frame of the
#'   [`data.table`] [`DataBackend`][mlr3::DataBackend] of the [`Task`][mlr3::Task].
#'   Initialized to [`selector_all()`], i.e., all features can be used as the `.SD` variable.
#' * `phase` :: `character(1)` \cr
#'   Character specifying the phase when filtering should be performed. Can either be `"always"`, `"train"`, or `"predict"`.
#'   Initialized to `"always"`, i.e., filtering is performed both during training and prediction.
#' @export
PipeOpFilterRows = R6::R6Class(
  "PipeOpFilterRows",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    #' @description Constructor.
    #' @param id [character(1)]
    #'  Identifier of the resulting object.
    #'  Default is `"filterrows"`.
    #' @param param_vals [named list]
    #'  List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
    #'  be set during construction. Default is `list()`.
    #' @return [`PipeOpFilterRows`].
    initialize = function(id = "filterrows", param_vals = list()) {
      ps = ps(
        filter_formula = p_uty(custom_check = check_filter_formulae,
                               tags =  c("train", "predict", "required")),
        SDcols = p_uty(custom_check = check_function, tags =  c("train", "predict", "required")),
        phase = p_fct(levels = c("always", "train", "predict"), tags = c("train", "predict", "required"))
      )
      ps$values = list(filter_formula = NULL,
                       SDcols = selector_all(),
                       phase = "always")
      super$initialize(id, param_set = ps, param_vals = param_vals)
    }
  ),
  private = list(
    .train_task = function(task) {
      self$state = list()
      if (self$param_set$values$phase %in% c("always", "train") &&
          length(self$param_set$values$filter_formula)) {
        print(self$param_set$values$filter_formula)
        filter_task(
          task,
          frm = self$param_set$values$filter_formula,
          SDcols = self$param_set$values$SDcols(task)
        )
      } else {
        task
      }
    },

    .predict_task = function(task) {
      if (self$param_set$values$phase %in% c("always", "predict") &&
          length(self$param_set$values$filter_formula)) {
        filter_task(
          task,
          frm = self$param_set$values$filter_formula,
          SDcols = self$param_set$values$SDcols(task)
        )
      } else {
        task
      }
    }
  )
)



mlr_pipeops$add("filterrows", PipeOpFilterRows)
