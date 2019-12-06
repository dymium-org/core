# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::import('checkmate')
modules::expose(here::here('{{{module_path}}}/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
constants <- modules::use(here::here('{{{module_path}}}/constants.R'))
helpers <- modules::use(here::here('{{{module_path}}}/helpers.R'))

modules::export('^^run|^util|^test') # default exported functions

#' {{{event_name}}}
#'
#' @param object a dymium agent class object
#' @param model a model object or a list of model objects
#' @param target a positive integers or a list of positive integers
#' @param time_steps positive integer()
#'
#' @return object

run <- function(object, model = NULL, target = NULL, time_steps = NULL) {

  # early return if `time_steps` is not the current time
  if (!dymiumCore::is_scheduled(time_steps)) {
    return(invisible(object))
  }

  lg$info('Running {{{event_name}}}')

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
  # create a reference to the main agent object for easy access eg:
  # PopObj <- assign_reference(object, Pop)

  # (Recommended)
  # create a reference to ModelContainer for easy access eg:
  # ModObj <- assign_reference(object, ModelContainer)

  # TODO: Target object
  # create a reference to TargetContainer (Not yet implemented) for easy access
  # TargetObj <- assign_reference(object, TargetContainer)

  # return the first argument (`object`) to make event functions pipe-able.
  invisible(object)
}

TransitionEventname <-
  R6::R6Class(classname = 'TransitionEventname',
              inherit = dymiumCore::Transition,
              public = list(

              ))

util_function <- function(x) {}

.util_function <- function(x) {}
