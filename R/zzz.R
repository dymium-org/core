# for storing variables that accessible and modifiable by all functions and objects
.DMevn <- new.env(parent = emptyenv())

.dymium_tempdir <- file.path(tempdir(), "dymium")

.dymium_options <- function() {
  return(list(
    scenario__dir = getOption('dymium.scenario_dir'),
    output_dir = getOption('dymium.output_dir'),
    input_dir = getOption('dymium.input_dir')
  ))
}

.dymium_options_msg = function() {
  glue::glue("
  *----- dymium's options -----*
  scenario_dir: {getOption('dymium.scenario_dir')}
  output_dir: {getOption('dymium.output_dir')}
  input_dir: {getOption('dymium.input_dir')}
  *----- dymium's options -----*")
}

.onLoad <- function(libname, pkgname) {

  # set global options
  opts <- options()
  opts.dymium <- list(
    dymium.scenario_dir = file.path(.dymium_tempdir),
    dymium.output_dir = file.path(.dymium_tempdir, "outputs"),
    dymium.input_dir = file.path(.dymium_tempdir, "inputs")
    # dymium.logFile = file.path(.dymium_tempdir, "log")
  )
  toset <- !(names(opts.dymium) %in% names(opts))
  if (any(toset)) options(opts.dymium[toset])

  # setup package global variables
  .DMevn[["sim_time"]] <- 0

  # create log file
  # _dir.create(_dirname(opts.dymium$dymium.logFile), recursive = T, showWarnings = FALSE)
  # file.create(opts.dymium$dymium.logFile)

  # setup logger
  assign("lg", lgr::get_logger_glue(name = pkgname), envir = parent.env(environment()))
  lg$set_appenders(list(cons = lgr::AppenderConsole$new()))
  # lg$add_appender(lgr::AppenderFile$new(opts.dymium$dymium.logFile, layout = lgr::LayoutJson$new()))
  lg$appenders$cons$set_layout(lgr::LayoutGlue$new(
    fmt = "[{format(timestamp, \'%H:%M:%S\')}] \\
            {pad_right(colorize_levels(toupper(level_name)), 5)} \\
            {crayon::yellow(.logger$name)} {caller}: {msg}"))
  lg$set_propagate(FALSE)

  # print to console
  packageStartupMessage(.dymium_options_msg())

  invisible()
}


.onUnload <- function(libpath) {
  ## if temp session _dir is being used, ensure it gets reset each session
  if (getOption("dymium.scenario_dir") == file.path(.dymium_tempdir)) {
    options(dymium.scenario_dir = NULL)
  }

  if (getOption("dymium.output_dir") == file.path(.dymium_tempdir, "outputs")) {
    options(dymium.output_dir = NULL)
  }

  if (getOption("dymium.input_dir") == file.path(.dymium_tempdir, "inputs")) {
    options(dymium.input_dir = NULL)
  }
}
