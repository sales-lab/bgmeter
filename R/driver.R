# Copyright 2025 Gabriele Sales

#' A monitor object.
#'
#' @importFrom R6 R6Class
BgMeter <- R6Class(
  "BgMeter",
  public = list(
    #' @field proc The handle for the background process taking measurements.
    proc = NULL,

    #' @description
    #' Create a new meter object.
    #' @param proc The handle of a background process.
    #' @return A new `BgMeter` object.
    initialize = function(proc = NA) {
      self$proc <- proc
    },
    #' @description
    #' Stop the background process and invalidate this object.
    close = function() {
      if (is.null(self$proc)) {
        return("stopped")
      }

      self$proc$interrupt()
      self$proc$wait(timeout = 1000L)

      if (self$proc$is_alive()) {
        self$proc$kill()
      }

      if (self$proc$get_exit_status() != 0) {
        return("failed")
      }

      self$proc <- NULL
      "ok"
    }
  )
)

#' Start taking repeated measures.
#'
#' @param func Callback to invoke for the measures. It should return a list of metrics.
#' @param period The time interval to wait between measurements, in seconds.
#' @param filename Measures will be appended to this file, using the JSONL format.
#' @return A `BgMeter` instance.
#'
#' @examples
#' measure <- function() list(cpu = 75, memory = 1024)
#' filename <- tempfile("bgmeter", fileext = ".jsonl")
#' meter <- bgmeter_start(measure, 1L, filename)
#' Sys.sleep(2L)
#' bgmeter_stop(meter)
#' unlink(filename)
#'
#' @export
bgmeter_start <- function(func, period, filename) {
  proc <- callr::r_bg(
    function(func, period, filename) {
      tryCatch(
        {
          con <- file(filename, "at")
          withr::defer(close(con))

          while (TRUE) {
            metrics <- func()
            writeLines(jsonlite::toJSON(metrics, pretty = FALSE), con)
            Sys.sleep(period)
          }
        },
        error = function(ex) {
          q(save = "no", status = 1)
        }
      )
    },
    args = list(func, period, filename)
  )

  BgMeter$new(proc)
}

#' Stop taking measurements.
#'
#' @param meter A `BgMeter` instance.
#'
#' @importFrom cli cli_abort
#' @export
bgmeter_stop <- function(meter) {
  switch(
    meter$close(),
    ok = NULL,
    stopped = cli_abort(c(
      "The meter is not active.",
      "i" = "It looks like the meter has already been stopped."
    )),
    failed = cli_abort(c(
      "The meter failed.",
      "x" = "The background process stopped with an unspecified error."
    ))
  )
}
