#' @title Graph
#'
#' @description Create mlr3 graph fo AUTOML for prediction in finance (investing).
#'
#' @import data.table
#' @import checkmate
#' @import mlr3verse
#' @import mlr3fselect
#' @import checkmate
#' @import future
#' @import future.apply
#' @import paradox
#' @import mlr3
#'
#' @export
Graph <- function() {
  # avoif error
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
  lrn_tabnet_ens$param_set$values$epochs <- 20
  lrn_tabnet_ens$predict_sets <- c("train", "test")
  lrn_tabnet_ens$predict_type <- "prob"

  # ensemble graph
  learners_l_ensemble = list(
    xgboost = lrn("classif.xgboost", predict_type = "prob", predict_sets = c("train", "test"), id = "xgboost_ens"),
    ranger = lrn("classif.ranger", predict_type = "prob", predict_sets = c("train", "test"), id = "ranger_ens"),
    glmnet = lrn("classif.glmnet", predict_type = "prob", predict_sets = c("train", "test"), id = "glmnet_ens"),
    kknn = lrn("classif.kknn", predict_type = "prob", predict_sets = c("train", "test"), id = "kknn_ens"),
    # lda = lrn("classif.lda", predict_type = "prob", predict_sets = c("train", "test"), id = "lda_ens"),
    nnet = lrn("classif.nnet", predict_type = "prob", predict_sets = c("train", "test"), id = "nnet_ens"),
    svm = lrn("classif.svm", predict_type = "prob", predict_sets = c("train", "test"), id = "svm_ens"),
    # extralearners
    # lda = lrn("classif.AdaBoostM1", predict_type = "prob", predict_sets = c("train", "test"), id = "adaboost_ens"),
    tabnet = lrn_tabnet_ens
  )
  ensemble_learners = learners_l_ensemble %>>%
    po("classifavg")
  ensemble_learners = as_learner(ensemble_learners)
  ensemble_learners$predict_sets <- c("train", "test")


  # LEARNERS ----------------------------------------------------------------
  # define tabner individually because lrn function doesnt work for now, looh at mlr3torch
  lrn_tabnet <- LearnerClassifTorchTabnet$new()
  lrn_tabnet$param_set$values$epochs <- 20
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
    svm = lrn("classif.svm", predict_type = "prob", predict_sets = c("train", "test"), id = "svm", type = "C-classification"),
    kknn = lrn("classif.kknn", predict_type = "prob", predict_sets = c("train", "test"), id = "kknn"),
    # lda = lrn("classif.lda", predict_type = "prob", predict_sets = c("train", "test"), id = "lda"),
    nnet = lrn("classif.nnet", predict_type = "prob", predict_sets = c("train", "test"), id = "nnet"),
    # extralearners
    # gausspr = lrn("classif.gausspr", predict_type = "prob", predict_sets = c("train", "test"), id = "gausspr")
    # adaboost = lrn("classif.AdaBoostM1", predict_type = "prob", predict_sets = c("train", "test"), id = "adaboost"),

    # nop_ensemble = po("nop", id = "nop_ensemble")
    # kerasff = lrn("classif.kerasff", predict_type = "prob", predict_sets = c("train", "test"), id = "kerasff")
    tabnet = lrn_tabnet,
    ensemble_avg = ensemble_learners
  )

  # create graph from list of learners
  learners = po("branch", options = c("xgboost", "ranger", "svm", "glmnet", "kknn", "nnet",
                                      # "adaboost",
                                      "tabnet", "ensemble_avg"), id = "branch_learners") %>>%
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
  plot(graph)

  return(graph_learner)
}


#' @title Parameters
#'
#' @description Define parameters for tuning.
#'
#' @import data.table
#' @import checkmate
#' @import mlr3verse
#' @import mlr3fselect
#' @import checkmate
#' @import paradox
#' @import mlr3
#'
#' @export
Parameters <- function() {
  # HYPERPARAMETERS ---------------------------------------------------------
  # DEBUG
  # head(as.data.table(graph_learner$param_set)[, .(id, class, lower, upper, levels)][grep("adab", id)], 10)

  # crate Graph
  graph_learner = Graph()

  # search space
  graph_learner$param_set$values$removeconstants.ratio = 0.01
  search_space = ps(
    # model matrix
    interaction_branch.selection = p_fct(levels = c("nop_1", "modelmatrix")),
    branch_learners.selection = p_fct(levels = c("ranger", "svm", "glmnet", "kknn", "xgboost", "nnet",
                                                 # "adaboost",
                                                 "tabnet", "ensemble_avg")),
    # ensemble_branch.selection = p_fct(levels = c("cv_ranger", "cv_glmnet", "cv_kknn")),
    ranger.ranger.max.depth = p_int(lower = 2, upper = 10, depends = `branch_learners.selection` == "ranger"),
    ranger.ranger.num.trees = p_int(lower = 50, upper = 2000, depends = `branch_learners.selection` == "ranger"),  # , tags = "budget"
    ranger.ranger.mtry.ratio = p_dbl(lower = 0.1, upper = 0.9, depends = `branch_learners.selection` == "ranger"),
    ranger.ranger.sample.fraction = p_dbl(0.1, 1, depends = `branch_learners.selection` == "ranger"),
    glmnet.glmnet.alpha = p_dbl(0, 1, depends = `branch_learners.selection` == "glmnet"),
    # glmnet.glmnet.s = p_dbl(1e-4, 1e3, logscale = TRUE, depends = `branch_learners.selection` == "glmnet"),
    svm.svm.kernel = p_fct(levels = c("linear", "polynomial", "radial", "sigmoid"), depends = branch_learners.selection == "svm"),
    svm.svm.cost =  p_dbl(1e-4, 1e3, logscale = TRUE, depends = branch_learners.selection == "svm"),
    svm.svm.gamma = p_dbl(1e-4, 1e3, logscale = TRUE,
                          depends = branch_learners.selection == "svm" && svm.svm.kernel %in% c("polynomial", "radial")),
    svm.svm.degree = p_int(2, 5, depends = branch_learners.selection == "svm" && svm.svm.kernel == "polynomial"),
    xgboost.xgboost.eta = p_dbl(1e-4, 1, depends = branch_learners.selection == "xgboost"),
    xgboost.xgboost.nrounds = p_int(1, 5000, depends = `branch_learners.selection` == "xgboost"),
    xgboost.xgboost.max_depth = p_int(1, 20, depends = branch_learners.selection == "xgboost"),
    xgboost.xgboost.colsample_bytree = p_dbl(0.1, 1, depends = `branch_learners.selection` == "xgboost"),
    xgboost.xgboost.lambda = p_dbl(0.1, 1, depends = `branch_learners.selection` == "xgboost"),
    xgboost.xgboost.gamma = p_dbl(1e-4, 1000, depends = `branch_learners.selection` == "xgboost"),
    xgboost.xgboost.alpha = p_dbl(1e-4, 1000, depends = `branch_learners.selection` == "xgboost"),
    xgboost.xgboost.subsample = p_dbl(0.1, 1, depends = `branch_learners.selection` == "xgboost"),
    kknn.kknn.k = p_int(1, 30, depends = `branch_learners.selection` == "kknn"),
    # lda.lda.method = p_fct(levels = c("moment", "mle", "mve", "t"), depends = branch_learners.selection == "lda"),
    tabnet.tabnet.decision_width = p_int(5, 40, depends = `branch_learners.selection` == "tabnet"),
    tabnet.tabnet.attention_width = p_int(5, 40, depends = `branch_learners.selection` == "tabnet"),
    tabnet.tabnet.num_steps = p_int(3, 6, depends = branch_learners.selection == "tabnet"),
    # tabnet.tabnet.learn_rate = p_int(3, 6, depends = branch_learners.selection == "tabnet"),
    # bart.ntree = p_int(30, 300, depends = branch.selection == "bart"),
    # gausspr.kernel = p_fct(levels = c("rbfdot", "polydot", "vanilladot"), depends = branch.selection == "gausspr"), # etc
    nnet.nnet.size = p_int(2, 5, depends = branch_learners.selection == "nnet"),
    nnet.nnet.decay = p_dbl(0, 0.001, depends = branch_learners.selection == "nnet"),
    # adaboost.adaboost.I = p_int(5, 25, depends = branch_learners.selection == "adaboost"),

    # scale
    scale_branch.selection = p_fct(levels = c("nop_2", "scale", "scalemaxabs")),

    # dim reduction
    dimreduction_branch.selection = p_fct(levels = c("nop_3", "pca", "ica")),
    pca.rank. = p_int(2, 10, depends = dimreduction_branch.selection == "pca"),
    ica.n.comp = p_int(2, 10, depends = dimreduction_branch.selection == "ica")
  )

  return(search_space)
}


#' @title Graph2
#'
#' @description Create mlr3 Graph2 fo AUTOML for prediction in finance (investing).
#'
#' @import data.table
#' @import checkmate
#' @import mlr3verse
#' @import mlr3fselect
#' @import checkmate
#' @import future
#' @import future.apply
#' @import paradox
#' @import mlr3
#'
#' @export
Graph2 <- function() {

  # avoid error
  branch_learners.selection = dimreduction_branch.selection =
    dimreduction_branch.selection = NULL


  # PREPROCESSING 1 -----------------------------------------------------------
  # preprocessing steps
  graph_prep = po("removeconstants", ratio = 0.03) %>>%
    po("filter", flt("find_correlation", method = "spearman"), filter.cutoff = 0.01)


  # FILTERS -----------------------------------------------------------------
  lrn = lrn("classif.ranger")
  lrn$param_set$values = list(importance = "impurity")
  f_n <- 5
  graph_filters = gunion(list(
    po("filter", mlr3filters::flt("disr"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("jmim"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("jmi"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("mim"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("mrmr"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("njmim"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("cmim"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("importance", learner = mlr3::lrn("classif.rpart")), filter.nfeat = 2, id = "rpart_imp"),
    po("filter", mlr3filters::flt("importance", learner = lrn), filter.nfeat = 2, id = "ranger_imp"),
    po("filter", mlr3filters::flt("importance", learner = lrn("classif.xgboost")), filter.nfeat = 2, id = "xgboost_imp")
  )) %>>%
    po("featureunion", 10, id = "union_filters") %>>%
    # po("copy", 10) %>>%
    po("branch", options = c("nop_filter", "modelmatrix"), id = "interaction_branch") %>>%
    gunion(list(po("nop", id = "nop_filter"), po("modelmatrix", formula = ~ . ^ 2))) %>>%
    po("unbranch", id = "interaction_unbranch")

  # DEBUG
  # plot(graph_filters)
  # task = tsk("german_credit")
  # graph_debug = graph_prep %>>%
  #   graph_filters
  # plot(graph_debug)
  # graph_debug = as_learner(graph_debug)
  # graph_debug$predict_sets <- c("train", "test")
  # x = graph_debug$train(task)
  # x$state$model$jmi
  # x$state$model

  # ADD FEATURES ------------------------------------------------------------
  # learners cv
  ranger_cv = po("learner_cv", lrn("classif.ranger", predict_type = "prob"), id = "ranger_cv", resampling.folds = 3)
  glmnet_cv = po("learner_cv", lrn("classif.glmnet", predict_type = "prob"), id = "glmnet_cv", resampling.folds = 3)
  # xgboost_cv = po("learner_cv",
  #                 lrn("classif.xgboost", predict_type = "prob", predict_sets = c("train", "test")))
  graph_features = gunion(list(
    ranger_cv = ranger_cv,
    glmnet_cv = glmnet_cv,
    # xgboost_cv = xgboost_cv
    po("nop", "nop_3")
  )) %>>% po("featureunion", id = "union_features")

  # DEBUG
  # plot(graph_features)


  # PREPROCESSING 2 -----------------------------------------------------------
  graph_prep_2 = po("branch", options = c("nop_prep", "yeojohnson", "pca", "ica"), id = "prep_branch") %>>%
    gunion(list(po("nop", id = "nop_prep"), po("yeojohnson"), po("pca", scale. = TRUE), po("ica"))) %>>%
    po("unbranch", id = "prep_unbranch")



  # LEARNERS ----------------------------------------------------------------
  learners_l = list(
    # xgboost = lrn("classif.xgboost", predict_type = "prob", predict_sets = c("train", "test"), id = "xgboost_cv_ens"),
    # ranger = lrn("classif.ranger", predict_type = "prob", predict_sets = c("train", "test"), id = "ranger_cv_ens"),
    # glmnet = lrn("classif.glmnet", predict_type = "prob", predict_sets = c("train", "test"), id = "glmnet_cv_ens")
    xgboost = lrn("classif.xgboost", predict_type = "prob", predict_sets = c("train", "test"), id = "xgboost"),
    ranger = lrn("classif.ranger", predict_type = "prob", predict_sets = c("train", "test"), id = "ranger"),
    glmnet = lrn("classif.cv_glmnet", predict_type = "prob", predict_sets = c("train", "test"), id = "cv_glmnet")

  )

  # create graph from list of learners
  graph_learners = gunion(learners_l) %>>%
    po("classifavg")

  # DEBUG
  # plot(graph_learners)


  # AUTOML GRAPH ------------------------------------------------------------
  # graph
  graph = graph_prep %>>%
    graph_filters %>>%
    graph_prep_2 %>>%
    # graph_features %>>%
    graph_learners
  plot(graph)
  graph_learner = as_learner(graph)
  graph_learner$predict_sets <- c("train", "test")

  return(graph_learner)
}


#' @title Parameters2
#'
#' @description Define parameters2 for tuning.
#'
#' @import data.table
#' @import checkmate
#' @import mlr3verse
#' @import mlr3fselect
#' @import checkmate
#' @import paradox
#' @import mlr3
#'
#' @export
Parameters2 <- function() {

  # crate Graph
  graph_learner = Graph2()

  # DEBUG
  # as.data.table(graph_learner$param_set)[, .(id, class, lower, upper, levels)][250:300]
  # as.data.table(graph_learner$param_set)[, .(id, class, lower, upper, levels)][grep("dimred", id)]

  # search space
  search_space = ps(
    # learners features
    interaction_branch.selection = p_fct(levels = c("nop_filter", "modelmatrix")),
    # ranger_cv.ranger_cv.max.depth = p_int(lower = 2, upper = 10),
    # ranger_cv.ranger_cv.num.trees = p_int(lower = 100, upper = 2000),
    # ranger_cv.ranger_cv.mtry.ratio = p_dbl(lower = 0.1, upper = 0.9),
    # glmnet_cv.glmnet_cv.alpha =p_dbl(0, 1),
    # preprocesing
    prep_branch.selection = p_fct(levels = c("nop_prep", "yeojohnson", "pca", "ica")),
    pca.rank. = p_int(2, 8, depends = prep_branch.selection == "pca"),
    ica.n.comp = p_int(2, 8, depends = prep_branch.selection == "ica"),
    yeojohnson.standardize = p_lgl(depends = prep_branch.selection == "yeojohnson"),
    # learners ensamble
    ranger.ranger.max.depth = p_int(lower = 2, upper = 10),
    ranger.ranger.num.trees = p_int(lower = 100, upper = 2000),
    ranger.ranger.mtry.ratio = p_dbl(lower = 0.1, upper = 0.9),
    glmnet.cv_glmnet.alpha = p_dbl(0, 1),
    xgboost.xgboost.eta = p_dbl(1e-4, 1),
    xgboost.xgboost.nrounds = p_int(1, 5000),
    xgboost.xgboost.max_depth = p_int(1, 20),
    xgboost.xgboost.colsample_bytree = p_dbl(0.1, 1),
    xgboost.xgboost.lambda = p_dbl(0.1, 1),
    xgboost.xgboost.gamma = p_dbl(1e-4, 1000),
    xgboost.xgboost.alpha = p_dbl(1e-4, 1000),
    xgboost.xgboost.subsample = p_dbl(0.1, 1)
  )

  return(search_space)
}

#' @title Graph3
#'
#' @description Create mlr3 Graph3 fo AUTOML for prediction in finance (investing).
#'
#' @import data.table
#' @import checkmate
#' @import mlr3verse
#' @import mlr3fselect
#' @import checkmate
#' @import future
#' @import future.apply
#' @import paradox
#' @import mlr3
#'
#' @export
Graph3 <- function() {

  # avoid error
  branch_learners.selection = dimreduction_branch.selection =
    dimreduction_branch.selection = NULL


  # PREPROCESSING 1 -----------------------------------------------------------
  # preprocessing steps
  graph_prep = po("removeconstants", ratio = 0.03) %>>%
    po("filter", flt("find_correlation", method = "spearman"), filter.cutoff = 0.01)


  # FILTERS -----------------------------------------------------------------
  lrn = lrn("classif.ranger")
  lrn$param_set$values = list(importance = "impurity")
  f_n <- 5
  graph_filters = gunion(list(
    po("filter", mlr3filters::flt("disr"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("jmim"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("jmi"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("mim"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("mrmr"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("njmim"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("cmim"), filter.nfeat = f_n, param_vals = list(affect_columns = selector_type("numeric"))),
    po("filter", mlr3filters::flt("importance", learner = mlr3::lrn("classif.rpart")), filter.nfeat = 2, id = "rpart_imp"),
    po("filter", mlr3filters::flt("importance", learner = lrn), filter.nfeat = 2, id = "ranger_imp"),
    po("filter", mlr3filters::flt("importance", learner = lrn("classif.xgboost")), filter.nfeat = 2, id = "xgboost_imp")
  )) %>>%
    po("featureunion", 10, id = "union_filters")

  # DEBUG
  # plot(graph_filters)
  # task = tsk("german_credit")
  # graph_debug = graph_prep %>>%
  #   graph_filters
  # plot(graph_debug)
  # graph_debug = as_learner(graph_debug)
  # graph_debug$predict_sets <- c("train", "test")
  # x = graph_debug$train(task)
  # x$state$model$jmi
  # x$state$model

  # ADD FEATURES ------------------------------------------------------------
  # learners cv
  ranger_cv = po("learner_cv", lrn("classif.ranger", predict_type = "prob"), id = "ranger_cv", resampling.folds = 3)
  glmnet_cv = po("learner_cv", lrn("classif.glmnet", predict_type = "prob"), id = "glmnet_cv", resampling.folds = 3)
  # xgboost_cv = po("learner_cv",
  #                 lrn("classif.xgboost", predict_type = "prob", predict_sets = c("train", "test")))
  graph_features = gunion(list(
    ranger_cv = ranger_cv,
    glmnet_cv = glmnet_cv,
    # xgboost_cv = xgboost_cv
    po("nop", "nop_3")
  )) %>>% po("featureunion", id = "union_features")

  # DEBUG
  # plot(graph_features)


  # PREPROCESSING 2 -----------------------------------------------------------
  graph_prep_2 = po("branch", options = c("nop_prep", "yeojohnson", "pca", "ica"), id = "prep_branch") %>>%
    gunion(list(po("nop", id = "nop_prep"), po("yeojohnson"), po("pca", scale. = TRUE), po("ica"))) %>>%
    po("unbranch", id = "prep_unbranch")



  # LEARNERS ----------------------------------------------------------------
  learners_l = list(
    # xgboost = lrn("classif.xgboost", predict_type = "prob", predict_sets = c("train", "test"), id = "xgboost_cv_ens"),
    # ranger = lrn("classif.ranger", predict_type = "prob", predict_sets = c("train", "test"), id = "ranger_cv_ens"),
    # glmnet = lrn("classif.glmnet", predict_type = "prob", predict_sets = c("train", "test"), id = "glmnet_cv_ens")
    xgboost = lrn("classif.xgboost", predict_type = "prob", predict_sets = c("train", "test"), id = "xgboost"),
    ranger = lrn("classif.ranger", predict_type = "prob", predict_sets = c("train", "test"), id = "ranger"),
    glmnet = lrn("classif.cv_glmnet", predict_type = "prob", predict_sets = c("train", "test"), id = "cv_glmnet")

  )

  # create graph from list of learners
  graph_learners = gunion(learners_l) %>>%
    po("classifavg")

  # DEBUG
  # plot(graph_learners)


  # AUTOML GRAPH ------------------------------------------------------------
  # graph
  graph = graph_prep %>>%
    graph_filters %>>%
    graph_prep_2 %>>%
    # graph_features %>>%
    graph_learners
  plot(graph)
  graph_learner = as_learner(graph)
  graph_learner$predict_sets <- c("train", "test")

  return(graph_learner)
}

#' @title FinAutoML
#'
#' @description Create mlr3 graph as AUTOML for prediction in finance (investing).
#'
#' @param tasks mlr3 tasks.
#' @param workers number of workers in parallel.
#' @param outer_folds number of outer CV folds.
#' @param inner_evals number of iterations inside every fold.
#' @param outer_cv outer CV.
#' @param folds Folds.
#' @param fixed_window Window ficed or not.
#'
#' @import data.table
#' @import checkmate
#' @import mlr3verse
#' @import mlr3fselect
#' @import mlr3fselect
#' @import checkmate
#' @import future
#' @import future.apply
#' @import paradox
#' @import mlr3
#'
#' @export
FinAutoML <- function(tasks, workers = 1L, outer_folds = 3L, inner_evals = 50L,
                      outer_cv = rsmp("RollingWindowCV", folds = outer_folds, fixed_window = FALSE)) {


  # crate Graph  and parameters
  graph_learner = Graph()
  parameters = Parameters()

  # AUTORUNER ---------------------------------------------------------------

  # # DEBUG RESAMPLEINGS
  # #Create a task with 10 observations
  # task_ = mlr3::tsk("airpassengers")
  # task_$filter(1:100)
  # #Instantiate Resampling
  # rfho = mlr3::rsmp("cv", folds = 3)
  # rfho = rsmp("RollingWindowCV", folds = 5, fixed_window = FALSE)
  # rfho$instantiate(task_)
  # # Individual sets:
  # rfho$train_set(1)
  # rfho$test_set(1)
  # rfho$instance #  list

  # create auto tuner
  at_1 = auto_tuner(
    method = "random_search",                          # other optimization methods?
    learner = graph_learner,                           # out graph learner, other learners can be adde for benchmark
    resampling = rsmp('forecastHoldout', ratio = 0.8), # inner resampling
    measure = msr("classif.acc"),                      # think on different measures, esc. for twoclass
    search_space = search_space,                       # search space defined above
    term_evals = inner_evals                           # number o evaluations inside CV
  )


  # BENCHMARK ---------------------------------------------------------------
  # define and tain benchmark
  design = benchmark_grid(tasks, list(at_1), rsmp("cv", folds = outer_folds))
  if (workers > 1L) plan("multisession", workers = workers)
  bmr = benchmark(design, store_models = TRUE)

  return(bmr)
}
