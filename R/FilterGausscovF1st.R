#' @title Gauss cov Feature Selection
#'
#' @name mlr_filters_gausscov_f1st
#'
#' @description Extract important features by calling [gausscov::f1st()] in
#' package \CRANpkg{gausscov}.
#'
#' @family Filter
#' @export
FilterGausscovF1st = R6::R6Class(
  "FilterGausscovF1st",
  inherit = mlr3filters::Filter,

  public = list(

    #' @description Create a GaussCov object.
    initialize = function() {
      param_set = ps(
        p0   = p_dbl(lower = 0, upper = 1, default = 0.01),
        kmn  = p_int(lower = 0, default = 0),
        kmx  = p_int(lower = 0, default = 0),
        mx   = p_int(lower = 1, default = 35),
        kex  = p_int(lower = 0, default = 0),
        sub  = p_lgl(default = TRUE),
        inr  = p_lgl(default = TRUE),
        xinr = p_lgl(default = FALSE),
        qq   = p_int(lower = 0, default = 0),
        save = p_lgl(default = FALSE),
        step = p_dbl(lower = 0, upper = 0.1, default = 0.01)
      )

      super$initialize(
        id = "gausscov_f1st",
        task_types = c("classif", "regr"),
        param_set = param_set,
        feature_types = c("integer", "numeric"),
        packages = "gausscov",
        label = "Gauss Covariance f1st",
        man = "mlr3filters::mlr_filters_gausscov_f1st"
      )
    }
  ),

  private = list(
    gausscov_ = function(x, y, pv) {
      res = mlr3misc::invoke(gausscov::f1st, y = y, x = x, .args = pv)
      res_1 = res[[1]]
      res_1 = res_1[res_1[, 1] != 0, , drop = FALSE]
      return(res_1)
    },

    .calculate = function(task, nfeat) {
      # debug
      # pv = list(
      #   p0   = 0.01,
      #   kmn  = 0,
      #   kmx  = 0,
      #   mx   = 21,
      #   kex  = 0,
      #   sub  = TRUE,
      #   inr  = TRUE,
      #   xinr = FALSE,
      #   qq   = 0,
      #   save = FALSE,
      #   step = 0.01
      # )

      # mlr_tasks
      # task = tsk("mtcars")
      # pv = list(); pv$p0 = 0.05

      # empty vector with variable names as vector names
      scores = rep(0, length(task$feature_names))
      scores = mlr3misc::set_names(scores, task$feature_names)

      # calculate gausscov pvalues
      pv = self$param_set$values
      x = as.matrix(task$data(cols = task$feature_names))
      if (task$task_type == "classif") {
        y = as.matrix(as.integer(task$truth()))
      } else {
        y = as.matrix(task$truth())
      }

      pv_gauss = pv[!names(pv) %in% c("step", "save")]
      gausscov_res = private$gausscov_(x, y, pv_gauss)
      while (nrow(gausscov_res) == 1) {
        pv$p0 = pv$p0 + pv$step
        print(pv$p0)
        gausscov_res = private$gausscov_(x, y, pv_gauss)
      }

      scores[gausscov_res[, 1]] = ceiling(abs(gausscov_res[, 4]))

      # save scores
      if (pv$save) {
        dir_name = "./gausscov_f1"
        if (!dir.exists(dir_name)) {
          dir.create(dir_name)
        }
        random_id <- paste0(sample(0:9, 15, replace = TRUE), collapse = "")
        file_name = paste0("gausscov_f1-", task$id, "-", random_id, ".rds")
        file_name = file.path(dir_name, file_name)
        saveRDS(scores, file_name)
      }

      sort(scores, decreasing = TRUE)
    }
  )
)

