#' Tidy prediction
#'
#' @description
#'
#' Evaluates the model using new data and returns the prediction result in as a
#' data.table format that has a consistence format.
#'
#' ```r
#' data.table::data.table(choices = list(character()), probs = list(numeric()))
#' ```
#'
#' @param model
#' @param ...
#'
#' @return a tidy_prediction object which is essentially a data.table.
#' @export
#'
#' @examples
tidy_prediction <- function(model, ...) {
  UseMethod("tidy_prediction")
}

tidy_prediction.list <- function(model, newdata, ...) {
  checkmate::assert_list(
    x = model,
    types = "numeric",
    names = "strict",
    null.ok = FALSE,
    any.missing = FALSE
  )
  checkmate::assert_data_frame(newdata)

  prediction <-
    data.table(probs = as.numeric(model)) %>%
    transpose(.) %>%
    setnames(names(model)) %>% {
      rep(list(.), nrow(newdata))
    } %>%
    data.table::rbindlist(.)

  class(prediction) <- c("tidy_prediction", class(prediction))

  prediction
}

tidy_prediction.data.table <- function(model, newdata, ...) {

  RESERVED_COLNAMES <- c('prob', 'probs', 'choices')

  is_dynamic_rate_datatable_model <- function(x, .data) {
    checkmate::assert_data_table(x, min.rows = 1, col.names = 'strict')
    time_cols <- is_dynamic_rate_col(names(x))
    matching_var_cols <- names(x)[!time_cols][names(x)[!time_cols] %in% names(.data)]
    if (length(matching_var_cols) != 0 & any(time_cols)) {
      return(TRUE)
    }
    return(FALSE)
  }

  is_dynamic_rate_col <- function(x) {
    grepl("^t_[0-9]+$", x)
  }

  if (!is.data.table(newdata)) {
    newdata <- as.data.table(newdata)
  }

  matching_vars <- names(model)[!names(model) %in% RESERVED_COLNAMES & !is_dynamic_rate_col(names(model))]

  # static binary choices
  if (checkmate::test_names(names(model), must.include = "prob", disjunct.from = c("choices", "probs"))) {
    checkmate::assert_numeric(model[['prob']], lower = 0)
    browser()
    prediction <-
      merge(newdata, model, by = matching_vars, all.x = T, sort = FALSE) %>%
      .[, yes := prob] %>%
      .[, no := 1 - prob] %>%
      .[, .(yes, no)]
  }

  # static multiple choices
  if (checkmate::test_names(names(model), must.include = c("choices", "probs"))) {
    checkmate::assert_list(model[['choices']], any.missing = FALSE, types = "character")
    checkmate::assert_list(model[['probs']], any.missing = FALSE, types = "numeric")
    checkmate::assert_numeric(unlist(model[['probs']]), lower = 0)
    prediction <-
      merge(newdata, model, by = matching_vars, all.x = T, sort = FALSE) %>%
      .[, .(choices, probs)]
  }

  # dynamic binary choices
  if (is_dynamic_rate_datatable_model(model, newdata)) {
    stop("not implemented yet")
    current_sim_time <- .get_sim_time()
    rate_col_flags <- is_dynamic_rate_col(names(model))
    times <-
      names(model)[rate_col_flags] %>%
      gsub("t_", "", .) %>%
      as.integer()
    index_closest_time <- which.min(abs(times - current_sim_time))
    rate_col_with_closest_time <- grep(paste0("t_",times[index_closest_time],"$"), names(model), value = T)
    rate_col_indexes[which(names(model) == colname_with_closest_time)] <- FALSE
    # filter the dynamic rate model with just the matching variable and the current time rate column
    current_rate_model <- model[, .SD, .SDcols = names(model)[!rate_col_indexes]]
    # create a prediction table
    prediction <-
      merge(x = private$.sim_data[, .SD, .SDcols = c(id_col, matching_vars)],
            y = current_rate_model,
            by = matching_vars,
            all.x = T
      ) %>%
      # dropping matching variables
      .[, .SD, .SDcols = names(.)[!names(.) %in% matching_vars]] %>%
      # merge to prob to the original ordering of private$.sim_data
      .[private$.sim_data[, .SD, .SDcols = id_col], on = "pid"] %>%
      # rename the rate column to prob
      data.table::setnames(., old = colname_with_closest_time, new = "prob") %>%
      # create a data.frame that contains 'no' and 'yes' columns
      .[, .(yes = prob, no = 1 - prob)]
  }

  if (!exists("prediction")) {
    stop("The given 'data.table' model did not match any of the special formats. ",
         "See 'Using transition probabilities' sub-section ",
         "on https://core.dymium.org/articles/dymium-intro.html.")
  }

  class(prediction) <- c("tidy_prediction", class(prediction))

  prediction

}

tidy_prediction.train <- function(model, newdata, ...) {
  if (model$modelType != "Classification") {
    stop("Only works for model type 'Classification'.")
  }

  prediction <-
    predict(model, newdata, type = "prob") %>%
    data.table::setDT(.)

  class(prediction) <- c("tidy_prediction", class(prediction))

  prediction
}

tidy_prediction.WrappedModel <- function(model, newdata, ...) {
  if (model$learner$type != "classif") {
    stop("Only `mlr` models of type `classif` (Classification) is supported.")
  }
  if (model$learner$predict.type != "prob") {
    stop("`mlr` model object must have predict.type equal to 'prob' to simulate choice.")
  }
  if (!requireNamespace("mlr")) {
    stop("`mlr` is not installed. Please install the `mlr` package first.")
  }
  # WrappedModel's predict method doesn't accept data.table in newdata
  if (is.data.table(newdata)) {
    newdata <- as.data.frame(newdata)
  }
  pred <- predict(model, newdata = newdata)
  if (length(model$task.desc$class.levels) == 2) {
    choices <- model$task.desc$class.levels
    prob_choice_one <- mlr::getPredictionProbabilities(pred)
    prediction <-
      data.table(choice_one = prob_choice_one,
                 choice_two = 1 - prob_choice_one) %>%
      data.table::setnames(choices)
  } else {
    prediction <-
      mlr::getPredictionProbabilities(pred) %>%
      data.table::setDT(.)
  }

  class(prediction) <- c("tidy_prediction", class(prediction))

  prediction
}

tidy_prediction.glm <- function(model, newdata) {
  if (model$family$family != "binomial") {
    stop("Only `glm` objects of the binomial family can be used in `simulate_choice()`.")
  }

  choices <- levels(model$model[[1]])

  prediction <-
    predict(model, newdata, type = "response") %>%
    {data.table::data.table(x1 = .,
                            x2 = 1 - .)} %>%
    data.table::setnames(choices)

  class(prediction) <- c("tidy_prediction", class(prediction))

  prediction
}
