#' @title Gauss cov Feature Selection f3
#'
#' @name mlr_filters_gausscov_f3st
#'
#' @description Extract important features by calling [gausscov::f3st()] in
#' package \CRANpkg{gausscov}.
#'
#' @family Filter
#' @export
FilterGausscovF3st = R6::R6Class(
  "FilterGausscovF3st",
  inherit = mlr3filters::Filter,

  public = list(

    #' @description Create a GaussCov object.
    initialize = function() {
      param_set = ps(
        m      = p_int(lower = 1, upper = 10, default = 1),
        kexmx	 = p_int(lower = 0, upper = 500, default = 0),
        p0     = p_dbl(lower = 0.0001, upper = 0.99, default = 0.01),
        kmn  = p_int(lower = 0, default = 0),
        kmx  = p_int(lower = 0, default = 0),
        mx   = p_int(lower = 1, default = 21),
        lm   = p_int(lower = 1, default = 100),
        kex  = p_int(lower = 0, default = 0),
        sub  = p_lgl(default = TRUE),
        inr  = p_lgl(default = TRUE),
        xinr = p_lgl(default = FALSE),
        qq   = p_int(lower = 0, default = 0),
        save = p_lgl(default = FALSE),
        step = p_dbl(lower = 0, upper = 0.1, default = 0.01)
      )

      super$initialize(
        id = "gausscov_f3st",
        task_types = c("classif", "regr"),
        param_set = param_set,
        feature_types = c("integer", "numeric"),
        packages = "gausscov",
        label = "Gauss Covariance f3st",
        man = "mlr3filters::mlr_filters_gausscov_f3st"
      )
    }
  ),

  private = list(
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
      #   qq   = 0
      # )

      # empty vector with variable names as vector names
      scores = rep(-1, length(task$feature_names))
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
      res = mlr3misc::invoke(gausscov::f3st, y = y, x = x, .args = pv_gauss)
      res_index <- tryCatch({unique(as.integer(res[[1]][1, ]))[-1]}, error = function(e) NULL)
      while (is.null(res_index)) {
        pv_gauss$p0 = pv_gauss$p0 + pv$step
        print(pv)
        res = mlr3misc::invoke(gausscov::f3st, y = y, x = x, .args = pv_gauss)
        res_index <- tryCatch({unique(as.integer(res[[1]][1, ]))[-1]}, error = function(e) NULL)
      }
      res_index  <- res_index [res_index  != 0]

      scores[res_index] = 1

      # save scores
      if (pv$save) {
        dir_name = "./gausscov_f3"
        if (!dir.exists(dir_name)) {
          dir.create(dir_name)
        }
        random_id <- paste0(sample(0:9, 15, replace = TRUE), collapse = "")
        file_name = paste0("gausscov_f3-", task$id, "-", random_id, ".rds")
        file_name = file.path(dir_name, file_name)
        saveRDS(scores, file_name)
      }

      # return scores
      sort(scores, decreasing = TRUE)
    }
  )
)
