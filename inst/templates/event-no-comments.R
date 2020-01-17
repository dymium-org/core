modules::import("dymiumCore")
modules::import("checkmate")
modules::expose(here::here("{{{module_path}}}/logger.R"))
constants <- modules::use(here::here("{{{module_path}}}/constants.R"))
helpers <- modules::use(here::here("{{{module_path}}}/helpers.R"))
modules::export("^run$|^REQUIRED_MODELS$")

REQUIRED_MODELS <- c()

run <- function(world, model = NULL, target = NULL, time_steps = NULL) {
  if (!dymiumCore::is_scheduled(time_steps)) {
    return(invisible(world))
  }
  lg$info("Running {{{event}}}")
  model <- pick_models(model, world, REQUIRED_MODELS)
  invisible(world)
}
