.dymium_tempdir <- file.path(tempdir(), "scenario")

.dymium_options <- function() {
  return(list(
    scenario_dir = getOption("dymium.scenario_dir"),
    output_dir = getOption("dymium.output_dir"),
    input_dir = getOption("dymium.input_dir")
  ))
}

.dymium_options_msg <- function() {
  opts <- grep("^dymium", names(options()), ignore.case = T, value = T)
  optsval <- sapply(opts, function(x) {
    getOption(x)
  })
  cli::cli_text(cli::rule(left = " * dymium's options * "))
  cli::cli_li(items = paste(names(optsval), optsval, sep = ": "))
}

.onLoad <- function(libname, pkgname) {

  # create temp directory
  fs::dir_create(.dymium_tempdir)
  fs::dir_create(fs::path(.dymium_tempdir, "inputs"))
  fs::dir_create(fs::path(.dymium_tempdir, "outputs"))

  # set global options
  opts <- options()
  opts.dymium <- list(
    dymium.simulation_clock = 0L,
    dymium.scenario_dir = file.path(.dymium_tempdir),
    dymium.input_dir = file.path(.dymium_tempdir, "inputs"),
    dymium.output_dir = file.path(.dymium_tempdir, "outputs"),
    dymium.simulation_scale = 1
  )
  toset <- !(names(opts.dymium) %in% names(opts))
  if (any(toset)) options(opts.dymium[toset])

  # setup logger
  assign("lg", lgr::get_logger_glue(name = pkgname), envir = parent.env(environment()))

  lg$set_appenders(list(
    cons = lgr::AppenderConsole$new(),
    buff = lgr::AppenderBuffer$new()
  ))

  lg$set_propagate(FALSE)

  # config console appender
  lg$appenders$cons$set_threshold("warn")
  lg$appenders$cons$set_layout(lgr::LayoutGlue$new(
    fmt = "[{format(timestamp, \'%H:%M:%S\')}] \\
            {pad_right(colorize_levels(toupper(level_name)), 5)} \\
            {crayon::yellow(.logger$name)} {caller}: {msg}"
  ))

  # config buffer appender
  # !! Event with a custom field "value" and without a 'msg' field  will be
  # logged as a simulation output to json
  sim_output_logfile <- paste0(opts.dymium$dymium.output_dir, "/sim_output.json")
  fs::file_create(sim_output_logfile)
  lg$appenders$buff$add_appender(lgr::AppenderJson$new(file = sim_output_logfile), name = "sim_output")
  filter_sim_output <- function(event) {
    event$msg == "SIM_OUTPUT"
  }
  lg$appenders$buff$appenders$sim_output$set_filters(list(filter_sim_output))

  # print to console
  .dymium_options_msg()

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
