#' @title FinAutoML
#'
#' @description Create mlr3 graph as AUTOML for prediction in finance (investing).
#'
#' @param tasks mlr3 tasks.
#' @param workers number of workers in parallel.
#' @param outer_folds number of outer CV folds.
#' @param inner_evals number of iterations inside every fold.
#'
#' @import data.table
#' @import checkmate
#' @import mlr3verse
#' @import mlr3fselect
#' @import mlr3fselect
#' @import mlr3torch
#' @import checkmate
#' @import future
#' @import future.apply
#' @import paradox
#' @import mlr3
#'
#' @export
FinAutoML <- function(tasks, workers = 2L, outer_folds = 3L, inner_evals = 50L) {

  #
  branch_learners.selection = dimreduction_branch.selection =
    dimreduction_branch.selection = NULL

  # FILTERS -----------------------------------------------------------------
  lrn = lrn("classif.ranger")
  lrn$param_set$values = list(importance = "impurity")
  graph_filters = gunion(list(
    po("filter", mlr3filters::flt("disr"), filter.nfeat = 5, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("jmim"), filter.nfeat = 5, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("jmi"), filter.nfeat = 5, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("mim"), filter.nfeat = 5, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("mrmr"), filter.nfeat = 5, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("njmim"), filter.nfeat = 5, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("cmim"), filter.nfeat = 5, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("importance", learner = mlr3::lrn("classif.rpart")), filter.nfeat = 10, id = "importance_1"),
    po("filter", mlr3filters::flt("importance", learner = lrn), filter.nfeat = 10, id = "importance_2")
  )) %>>%
    po("featureunion", 9)

  # DEBUG
  # plot(graph_filters)


  # ENSEMBLE LEARNERS -------------------------------------------------------
  # define tabner individually because lrn function doesnt work for now, looh at mlr3torch
  lrn_tabnet_ens <- LearnerClassifTorchTabnet$new()
  lrn_tabnet_ens$id <- "lrn_tabnet"
  lrn_tabnet_ens$param_set$values$epochs <- 10
  lrn_tabnet_ens$predict_sets <- c("train", "test")
  lrn_tabnet_ens$predict_type <- "prob"

  # ensemble graph
  learners_l_ensemble = list(
    xgboost = lrn("classif.xgboost", predict_type = "prob", predict_sets = c("train", "test"), id = "xgboost_ens"),
    ranger = lrn("classif.ranger", predict_type = "prob", predict_sets = c("train", "test"), id = "ranger_ens"),
    glmnet = lrn("classif.glmnet", predict_type = "prob", predict_sets = c("train", "test"), id = "glmnet_ens"),
    kknn = lrn("classif.kknn", predict_type = "prob", predict_sets = c("train", "test"), id = "kknn_ens"),
    tabnet = lrn_tabnet_ens
  )
  ensemble_learners = learners_l_ensemble %>>%
    po("classifavg")
  ensemble_learners = as_learner(ensemble_learners)
  ensemble_learners$predict_sets <- c("train", "test")


  # LEARNERS ----------------------------------------------------------------
  # define tabner individually because lrn function doesnt work for now, looh at mlr3torch
  lrn_tabnet <- LearnerClassifTorchTabnet$new()
  lrn_tabnet$param_set$values$epochs <- 10
  lrn_tabnet$predict_sets <- c("train", "test")
  lrn_tabnet$predict_type <- "prob"
  lrn_tabnet$id = "tabnet"

  # learners
  learners_l = list(
    # learners
    # learner_cv_ranger = po("learner_cv", ranger$clone()),
    # learner_cv_glmnet = po("learner_cv", glmnet$clone()),
    # learner_cv_kknn = po("learner_cv", kknn$clone())
    xgboost = lrn("classif.xgboost", predict_type = "prob", predict_sets = c("train", "test"), id = "xgboost"),
    ranger = lrn("classif.ranger", predict_type = "prob", predict_sets = c("train", "test"), id = "ranger"),
    glmnet = lrn("classif.glmnet", predict_type = "prob", predict_sets = c("train", "test"), id = "glmnet"),
    # svm = lrn("classif.svm", predict_type = "prob", predict_sets = c("train", "test"), id = "svm", type = "C-classification"),
    kknn = lrn("classif.kknn", predict_type = "prob", predict_sets = c("train", "test"), id = "kknn"),
    # nnet = lrn("classif.nnet", predict_type = "prob", predict_sets = c("train", "test"), id = "nnet")
    # extralearners
    # gausspr = lrn("classif.gausspr", predict_type = "prob", predict_sets = c("train", "test"), id = "gausspr")

    # nop_ensemble = po("nop", id = "nop_ensemble")
    # kerasff = lrn("classif.kerasff", predict_type = "prob", predict_sets = c("train", "test"), id = "kerasff")
    tabnet = lrn_tabnet,
    ensemble_avg = ensemble_learners
  )
  learners_l$xgboost$predict_sets
  learners_l$ensemble_avg$predict_sets

  # create graph from list of learners
  learners = po("branch", options = c("xgboost", "ranger", "glmnet", "kknn", "tabnet", "ensemble_avg"), id = "branch_learners") %>>%
    gunion(learners_l) %>>%
    po("unbranch", id = "unbranch_learners")

  # DEBUG
  # plot(learners)


  # AUTOML GRAPH ------------------------------------------------------------
  # graph
  graph = po("removeconstants") %>>%
    po("filter", flt("find_correlation", method = "spearman"), filter.cutoff = 0.01) %>>%
    # filter features
    graph_filters %>>%
    # model matrix
    po("branch", options = c("nop_1", "modelmatrix"), id = "interaction_branch") %>>%
    gunion(list(po("nop", id = "nop_1"), po("modelmatrix", formula = ~ . ^ 2))) %>>%
    po("unbranch", id = "interaction_unbranch") %>>%
    # scaling
    po("branch", options = c("nop_2", "scale", "scalemaxabs"), id = "scale_branch") %>>%
    gunion(list(po("nop", id = "nop_2"), po("scale"), po("scalemaxabs"))) %>>%
    po("unbranch", id = "scale_unbranch") %>>%
    # dim reduction
    po("branch", options = c("nop_3", "pca", "ica"), id = "dimreduction_branch") %>>%
    gunion(list(po("nop", id = "nop_3"), po("pca"), po("ica"))) %>>%
    po("unbranch", id = "dimreduction_unbranch") %>>%
    # learners
    learners
  graph_learner = as_learner(graph)

  # DEBUG
  # plot(graph)



  # HYPERPARAMETERS ---------------------------------------------------------
  # DEBUG
  # as.data.table(graph_learner$param_set)[, .(id, class, lower, upper, levels)]

  # search space
  graph_learner$param_set$values$removeconstants.ratio = 0.01
  search_space = ps(
    # model matrix
    interaction_branch.selection = p_fct(levels = c("nop_1", "modelmatrix")),
    branch_learners.selection = p_fct(levels = c("ranger", "glmnet", "kknn", "xgboost", "tabnet")),
    # ensemble_branch.selection = p_fct(levels = c("cv_ranger", "cv_glmnet", "cv_kknn")),
    ranger.ranger.max.depth = p_int(lower = 2, upper = 10, depends = `branch_learners.selection` == "ranger"),
    ranger.ranger.num.trees = p_int(lower = 50, upper = 2000, depends = `branch_learners.selection` == "ranger"),  # , tags = "budget"
    ranger.ranger.mtry.ratio = p_dbl(lower = 0.1, upper = 0.9, depends = `branch_learners.selection` == "ranger"),
    ranger.ranger.sample.fraction = p_dbl(0.1, 1, depends = `branch_learners.selection` == "ranger"),
    glmnet.glmnet.alpha = p_dbl(0, 1, depends = `branch_learners.selection` == "glmnet"),
    glmnet.glmnet.s = p_dbl(1e-4, 1e3, logscale = TRUE, depends = `branch_learners.selection` == "glmnet"),
    # svm.kernel = p_fct(levels = c("linear", "polynomial", "radial", "sigmoid"), depends = branch.selection == "svm"),
    # svm.cost =  p_dbl(1e-4, 1e3, logscale = TRUE, depends = branch.selection == "svm" && branch.selection == "svm"),
    # svm.gamma = p_dbl(1e-4, 1e3, logscale = TRUE, depends = branch.selection == "svm" && svm.kernel %in% c("polynomial", "radial")),
    # svm.tolerance = p_dbl(1e-4, 2, logscale = TRUE, depends = branch.selection == "svm"),
    # svm.degree = p_int(2, 5, depends = branch.selection == "svm" && svm.kernel == "polynomial"),
    xgboost.xgboost.eta = p_dbl(1e-4, 1, depends = `branch_learners.selection` == "xgboost"),
    xgboost.xgboost.nrounds = p_int(1, 5000, depends = `branch_learners.selection` == "xgboost"),
    xgboost.xgboost.max_depth = p_int(1, 20, depends = `branch_learners.selection` == "xgboost"),
    xgboost.xgboost.colsample_bytree = p_dbl(0.1, 1, depends = `branch_learners.selection` == "xgboost"),
    xgboost.xgboost.lambda = p_dbl(0.1, 1, depends = `branch_learners.selection` == "xgboost"),
    xgboost.xgboost.gamma = p_dbl(1e-4, 1000, depends = `branch_learners.selection` == "xgboost"),
    xgboost.xgboost.alpha = p_dbl(1e-4, 1000, depends = `branch_learners.selection` == "xgboost"),
    xgboost.xgboost.subsample = p_dbl(0.1, 1, depends = `branch_learners.selection` == "xgboost"),
    kknn.kknn.k = p_int(1, 30, depends = `branch_learners.selection` == "kknn"),
    tabnet.tabnet.decision_width = p_int(5, 40, depends = `branch_learners.selection` == "tabnet"),
    tabnet.tabnet.attention_width = p_int(5, 40, depends = `branch_learners.selection` == "tabnet"),
    tabnet.tabnet.num_steps = p_int(3, 6, depends = `branch_learners.selection` == "tabnet"),
    # tabnet.tabnet.learn_rate = p_int(3, 6, depends = branch_learners.selection == "tabnet"),
    # bart.ntree = p_int(30, 300, depends = branch.selection == "bart"),
    # gausspr.kernel = p_fct(levels = c("rbfdot", "polydot", "vanilladot"), depends = branch.selection == "gausspr"), # etc
    # nnet.size = p_int(2, 5, depends = branch.selection == "nnet"),
    # nnet.decay = p_dbl(0, 0.001, depends = branch.selection == "nnet"),

    # scale
    scale_branch.selection = p_fct(levels = c("nop_2", "scale", "scalemaxabs")),

    # dim reduction
    `dimreduction_branch.selection` = p_fct(levels = c("nop_3", "pca", "ica")),
    pca.rank. = p_int(2, 10, depends = `dimreduction_branch.selection` == "pca"),
    ica.n.comp = p_int(2, 10, depends = `dimreduction_branch.selection` == "ica")

    # classif.ranger.max.depth = p_int(lower = 2, upper = 10),
    # classif.ranger.num.trees = p_int(lower = 50, upper = 2000),  # , tags = "budget"
    # classif.ranger.mtry.ratio = p_dbl(lower = 0.1, upper = 0.9),
    # classif.ranger.sample.fraction = p_dbl(0.1, 1)
  )


  # AUTORUNER ---------------------------------------------------------------
  # create auto tuner
  at_1 = auto_tuner(
    method = "random_search",                  # other optimization methods?
    learner = graph_learner,                   # out graph learner, other learners can be adde for benchmark
    resampling = rsmp('holdout', ratio = 0.8), # inner resampling
    measure = msr("classif.acc"),              # think on different measures, esc. for twoclass
    search_space = search_space,               #  search space defined above
    term_evals = inner_evals                   # number o evaluations inside CV
  )


  # BENCHMARK ---------------------------------------------------------------
  # define and tain benchmark
  design = benchmark_grid(tasks, list(at_1), rsmp("cv", folds = outer_folds))
  if (workers > 1L) plan("multisession", workers = workers)
  bmr = benchmark(design, store_models = TRUE)

  return(bmr)
}

