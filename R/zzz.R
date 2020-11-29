.onLoad <- function(libname, pkgname) {

  # setup logger
  assign("lg", lgr::get_logger_glue(name = pkgname), envir = parent.env(environment()))

  lg$set_appenders(list(cons = lgr::AppenderConsole$new(),
                        buff = lgr::AppenderBuffer$new()))

  lg$set_propagate(FALSE)

  # config console appender
  lg$appenders$cons$set_threshold("warn")
  lg$appenders$cons$set_layout(lgr::LayoutGlue$new(
    fmt = "[{format(timestamp, \'%H:%M:%S\')}] \\
            {pad_right(colorize_levels(toupper(level_name)), 5)} \\
            {crayon::yellow(.logger$name)} {caller}: {msg}"))

  invisible()
}

.onUnload <- function(libpath) {

}
