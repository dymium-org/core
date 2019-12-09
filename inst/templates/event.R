# It is recommended to assign this module in your run script to a variable
# called: 'event_{{{module}}}_{{{event}}}'
# for example:
#   event_{{{module}}}_{{{event}}} <- modules::use('modules/{{{module}}}/{{{event}}}.R')

# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::import('checkmate')
modules::expose(here::here('{{{module_path}}}/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
constants <- modules::use(here::here('{{{module_path}}}/constants.R'))
helpers <- modules::use(here::here('{{{module_path}}}/helpers.R'))

modules::export('^run$|^REQUIRED_MODELS$') # default exported functions


# Required models ---------------------------------------------------------
# NOTE: The names of the required models for the event go here. This will be used
#       to check whether the input 'world' object or the model argument contains
#       the models as required.
# Example:
#       REQUIRED_MODELS <- c("MyBinaryModel", "MyRegressionModel")
REQUIRED_MODELS <- c()

# Main function -----------------------------------------------------------
#' {{{event}}}
#'
#' Please see the module's README file for the details of this event function.
#'
#' @param world a [dymiumCore::World] object.
#' @param model a model object or a named list of model objects that are sip.
#' @param target a positive integers or a named list of positive integers.
#' @param time_steps a positive integer vector.
#'
#' @return x
run <- function(world, model = NULL, target = NULL, time_steps = NULL) {

  # early return if `time_steps` is not the current time
  if (!dymiumCore::is_scheduled(time_steps)) {
    return(invisible(world))
  }

  # logging to console
  lg$info('Running {{{event}}}')

  # check the model argument
  # Note:
  # 1) if the model argument is given, meaning not NULL by default, the given model
  #    objects will be used instead of the saved model objects inside 'world',
  #    if there are any.
  # 2) if the event is deterministic like 'ageing' or doesn't require models then
  #    you may remove the five lines below entirely and add a warning message to
  #    notify the user when the model argument is not NULL.
  if (is.null(model)) {
    model <- dymiumCore::get_models(world, REQUIRED_MODELS)
  } else {
    dymiumCore::check_required_models(model, REQUIRED_MODELS)
  }

  # uncomment the line belows if the event doesn't require `model`
  # eg. If the event is deterministic like ageing.
  # if (!is.null(model)) {
  #   lg$warn('`model` will not be used.')
  # }

  # uncomment the line belows if the event doesn't require `target`
  # eg. If the event is to be applied to all agents.
  # if (!is.null(target)) {
  #   lg$warn('`target` will not be used.')
  # }

  # (Recommended)
  # create a reference to the entity and model objects for easy access for examples:
  # To get entities..
  # Pop <- assign_reference(world, Population)
  # Hh <- assign_reference(world, "Household")
  # Hh <- world$get("Household")
  # To get models..
  # MyModel <- world$get("MyModel")
  # MyModel <- world$get_model("MyModel")

  # The beginning of the steps  -----------------------




  # The end of the steps -----------------------

  # always return the 'world' object invisibly.
  invisible(world)
}


# Customised Transition classes -------------------------------------------
# Note: If you need to add extra preprocessing steps to your Transition class
#       you will need to extend TransitionClassification or TransitionRegression
#       as necessary.
# Use the commented Transition codes below as a template for your own Transition class.
# TransitionDescription <-
#   R6::R6Class(
#     classname = "TransitionDescription",
#     inherit = dymiumCore::TransitionClassification,
#     public = list(
#       mutate = function(.data) {
#         .data %>%
#           # create five years age group
#           .[, age_group := cut(age, breaks = seq(0, 100, 5), include.lowest = TRUE, right = FALSE)]
#       },
#       filter = function(.data) {
#         .data %>%
#           # only keep all agents with age less than 100
#           .[age < 100, ]
#       }
#     )
#   )



# Utility functions -------------------------------------------------------
