# lm_numeric with different approaches

test_that("output_lm_numeric_ctree", {
  set.seed(123)
  expect_snapshot_rds(
    {
      explain_mcce(model = model_lm_numeric,
                   x_explain = x_explain_numeric,
                   x_train = x_train_numeric,
                   c_int = c(-Inf,15),
                   fixed_features = "Wind",
                   fit.autoregressive_model = "ctree",
                   fit.seed = 1,
                   generate.seed = 2,
                   timing = FALSE)
    },
    "output_lm_numeric_ctree"
  )
})

test_that("output_lm_numeric_rpart", {
  set.seed(123)
  expect_snapshot_rds(
    {
      explain_mcce(model = model_lm_numeric,
                   x_explain = x_explain_numeric,
                   x_train = x_train_numeric,
                   c_int = c(-Inf,15),
                   fixed_features = "Wind",
                   fit.autoregressive_model = "rpart",
                   fit.seed = 1,
                   generate.seed = 2,
                   timing = FALSE)
    },
    "output_lm_numeric_rpart"
  )
})
