# # Load necessary libraries
# library(mlr3)
# library(mlr3pipelines)
# library(forecast)
# library(data.table)
#
# # @export PipeOpDifferencing
# PipeOpDifferencing = R6::R6Class(
#   "PipeOpDifferencing",
#   inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
#   public = list(
#     initialize = function(id = "differencing", param_vals = list()) {
#       ps = ps(
#         test = p_fct(levels = c("kpss", "adf", "pp"), default = "kpss"),
#         replace = p_lgl(default = FALSE)
#       )
#       super$initialize(
#         id = id,
#         param_set = ps,
#         param_vals = param_vals,
#         feature_types = c("numeric")
#       )
#     }
#   ),
#   private = list(
#     .get_state = function(task) {
#       # Get numeric feature names (excluding the target)
#       numeric_features = setdiff(
#         task$feature_types[type %in% self$feature_types, id],
#         task$target_names
#       )
#       data = task$data(cols = numeric_features)
#       params = self$param_set$get_values()
#
#       # Compute ndiffs for all columns
#       diffs = data[, lapply(.SD, function(x) forecast::ndiffs(x, test = params$test))]
#       diffs = unlist(as.list(diffs))
#
#       list(diffs = diffs)
#     },
#
#     .transform = function(task) {
#       params = self$param_set$get_values()
#
#       # Get numeric feature names (excluding the target)
#       numeric_features = setdiff(
#         task$feature_types[type %in% self$feature_types, id],
#         task$target_names
#       )
#       data = task$data(cols = numeric_features)
#       target_data = task$data(cols = task$target_names)
#
#       # Get the number of differences for each feature
#       diffs = self$state$diffs
#
#       # Initialize a list to hold new differenced features
#       new_features = list()
#
#       # Apply differencing to each feature and add as new feature
#       for (feature in numeric_features) {
#         ndiff = diffs[[feature]]
#         if (ndiff > 0) {
#           x = data[[feature]]
#           x_diff = diff(x, differences = ndiff)
#           # Add differenced feature to the list
#           new_features[[paste0(feature, "_diff")]] = x_diff
#         }
#       }
#
#       # If there are new features, add them to the task
#       if (length(new_features) > 0) {
#         new_features_dt = as.data.table(new_features)
#         # Remove the first max_ndiff rows to align data
#         max_ndiff = max(diffs)
#         data = data[-(1:max_ndiff), , drop = FALSE]
#         target_data = target_data[-(1:max_ndiff), , drop = FALSE]
#         # Update the task with truncated original data and target
#         task$data(cols = numeric_features) = data
#         task$data(cols = task$target_names) = target_data
#         # Add new differenced features
#         task$cbind(new_features_dt)
#       }
#       if (params$replace == TRUE) {
#
#       } else {
#
#       }
#
#
#       return(task)
#     }
#   )
# )
#
#
# # Load necessary libraries
# library(mlr3)
# library(mlr3pipelines)
# library(forecast)
# library(data.table)
#
# # Sample data
# set.seed(123)
# n = 100
# dt = data.table(
#   x1 = cumsum(rnorm(n)),  # Non-stationary series
#   x2 = cumsum(rnorm(n)),  # Non-stationary series
#   x3 = rnorm(n),          # Stationary series
#   target = cumsum(rnorm(n))  # Non-stationary target
# )
#
# # Create a regression task
# task = TaskRegr$new(id = "regression_task", backend = dt, target = "target")
#
# # Initialize the custom PipeOp
# diff_pipeop = PipeOpDifferencing$new(param_vals = list(test = "kpss"))
#
# # Create a learner
# learner = lrn("regr.lm")
#
# # Build the pipeline graph
# graph = diff_pipeop %>>% learner
#
# # Create a GraphLearner
# graph_learner = GraphLearner$new(graph)
#
# # Train the model
# graph_learner$train(task)
#
# # Make predictions (using the same data for simplicity)
# prediction = graph_learner$predict(task)
#
# # View the predictions
# print(prediction$response)
#
