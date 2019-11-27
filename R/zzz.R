# for storing variables that accessible and modifiable by all functions and objects
.DMevn <- new.env(parent = emptyenv())

.dymiumTempDir <- file.path(tempdir(), "dymium")

.onLoad <- function(libname, pkgname) {

  # set global options
  opts <- options()
  opts.dymium <- list(
    dymium.outputPath = file.path(.dymiumTempDir, "outputs")
    # dymium.logFile = file.path(.dymiumTempDir, "log")
  )
  toset <- !(names(opts.dymium) %in% names(opts))
  if (any(toset)) options(opts.dymium[toset])

  # setup package global variables
  .DMevn[["sim_time"]] <- 0

  # create log file
  # dir.create(dirname(opts.dymium$dymium.logFile), recursive = T, showWarnings = FALSE)
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
  packageStartupMessage(glue::glue("
  *----- dymium's options -----*
  outputPath: {getOption('dymium.outputPath')}
  *----- dymium's options -----*"))

  invisible()
}


.onUnload <- function(libpath) {
  ## if temp session dir is being used, ensure it gets reset each session
  if (getOption("dymium.outputPath") == file.path(.dymiumTempDir, "outputs")) {
    options(dymium.outputPath = NULL)
  }

  # if (getOption("dymium.logFile") == file.path(.dymiumTempDir, "log")) {
  #   unlink(getOption("dymium.logFile"))
  # }
}
