
test_that("Identical results when reusing the featuremodel in a new explain-call", {
  explained1 <- explain_mcce(model = model_lm_numeric,
                             x_explain = x_explain_numeric,
                             x_train = x_train_numeric,
                             c_int = c(-Inf,15),
                             fixed_features = "Wind",
                             fit.autoregressive_model = "ctree",
                             fit.seed = 1,
                             generate.seed = 2,
                             timing = FALSE,
                             return_featuremodel_object = TRUE) # Returning featuremodel

  explained2 <- explain_mcce(model = model_lm_numeric,
                             x_explain = x_explain_numeric,
                             x_train = x_train_numeric,
                             c_int = c(-Inf,15),
                             featuremodel_object = explained1$featuremodel_object, # Reuse featuremodel
                             fixed_features = "Wind",
                             fit.autoregressive_model = "ctree",
                             fit.seed = 1,
                             generate.seed = 2,
                             timing = FALSE,
                             return_featuremodel_object = TRUE)


  # The entire object is identical
  expect_equal(
    explained1,
    explained2
  )

})
