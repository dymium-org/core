# All functions here are meant to use as templates for `dymiumCore::create_new_event_from_template()` -----
#' @noRd
template_event_run <-
"run <- function(object, model = NULL, target = NULL, time_steps = NULL) {

  # early return if `time_steps` is not the current time
  if (!dymiumCore::is_scheduled(time_steps)) {
    return(invisible(object))
  }

  lg$info('Running [event_name]')

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
}\n"

template_event_doc <-
"\n#' [TITLE]
#'
#' @param object a dymium agent class object
#' @param model a model object or a list of model objects
#' @param target a positive integers or a list of positive integers
#' @param time_steps positive integer()
#'
#' @return object"

template_event_imports <-
"# default setup, you may edit the below import statments to match your requirements.
modules::import('dymiumCore')
modules::expose(here::here('[module_path]/logger.R')) # import lgr's logger. To use the logger use 'lg' (default logger's name).
constants <- modules::use(here::here('[module_path]/constants.R'))
helpers <- modules::use(here::here('[module_path]/helpers.R'))
"

template_event_exports <-
"modules::export('^^run|^util|^test') # default exported functions"


template_event_transition <-
"TransitionEventname <-
  R6::R6Class(classname = 'TransitionEventname',
              inherit = dymiumCore::Transition,
              public = list(

              ))\n"

template_event_test <-
"# note: The 'Run tests' button in RStudio may not work (Please let me know if you can make it work!)
#       to run this test use # test_file('[test_path]')
# import the module
library(dymiumCore)
# set logger's threshold to 'warn' to mute info level loggings
dymiumCore:::lg$set_threshold(level = 'warn')
[event_function_name] <- modules::use(here::here('[event_path]'))

# write your on tests using testthat::test_that(...)
test_that('event works', {
  # for example
  expect_true(1 == 1)
})
"

template_event_util <-
  "util_function <- function(x) {}\n"

template_event_private_util <-
  ".util_function <- function(x) {}\n"

template_module_constants <-
"# Constant values to be used within the module can be declared here.
#
# To use these constants inside your event script you may import the constants script
# as the following 'constants <- modules::import('[module_path]/constants.R')'.
#
# The following codes are examples of how you may declare your constants.
modules::export('MYFIRST_CONSTANT') # must export your constants, individually.
MYFIRST_CONSTANT <- list()
MYFIRST_CONSTANT$SOMEVALUE1 <- 'VALUE1'
MYFIRST_CONSTANT$SOMEVALUE2 <- 'VALUE2'

modules::export('MYSECOND_CONSTANT') # must export your constants, individually.
MYSECOND_CONSTANT <- list()
MYSECOND_CONSTANT$SOMEVALUE1 <- 'VALUE1'
MYSECOND_CONSTANT$SOMEVALUE2 <- 'VALUE2'
"

template_module_helpers <-
"# The script is where helper functions of the module should reside.
# Sometimes you may have a function that are used across the module, in many events,
# this is the central place for you to store this type of function. Hence, in every
# event scrips that you create using 'dymiumCore::create_new_event' this script, helpers.R,
# will be imported. If not needed, you may remove the import line.
#
# To use these helper functions inside your event script I suggest you import the helper script
# as the following 'helpers <- modules::import('[module_path]/helpers.R')'.

# If the package dymimCore is not needed you may remove the line below which imports it
modules::import('dymiumCore')

# If you need your constants here uncomment the line below
# constants <- modules::use('[module_path]/constants.R')
"

template_module_logger <-
"# This script creates a logger to be used across the module.
# To customise and learn more out the logger package used here please read the
# vignette of 'lgr' package (https://github.com/s-fleck/lgr).
# To use the logger inside your event script you may import the constants script
# as the following 'modules::expose('[module_path]/logger.R')' the logger
# A neat modification that I like to do is to colorize the
# name of my module's logger by changing '{.logger$name}' to '{crayon::blue(.logger$name)}'
# this will make your module's logger name more standout on your R console. To make
# the color modification you will need to have 'crayon' package installed.
#
#
# TL;DR - to use logger put this 'modules::expose('[module_path]/logger.R')' in your
#         event script. Then use the following commands.
#
# > lg$info('I am the info-level logger')
# > lg$warn('I am the warn-level logger')
# > lg$error('I am the error-level logger')
#
# !! The codes below should work without any modifications, however if you are
# comfortable with how the 'lgr' and 'modules' packages work you may modify
# the codes below.
modules::import('lgr')
modules::export('lg')
lg <- lgr::get_logger_glue(name = '[module_name]')
lg$set_appenders(list(cons = lgr::AppenderConsole$new()))
lg$appenders$cons$set_layout(lgr::LayoutGlue$new(fmt = '[{format(timestamp, \\'%H:%M:%S\\')}] \\\\
                                                        {pad_right(colorize_levels(toupper(level_name)), 5)}\\\\
                                                        {.logger$name} {caller}: {msg}'))
lg$set_propagate(FALSE)
"
