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
    initialize = function(proc = NULL) {
      self$proc <- proc
    },
    #' @description
    #' Stop the background process and invalidate this object.
    close = function() {
      if (is.null(self$proc)) {
        return(0)
      }

      kill_proc(self$proc)
      res <- self$proc$get_exit_status()

      self$proc <- NULL
      return(res)
    }
  )
)

kill_proc <- function(proc) {
  proc$interrupt()
  proc$wait(timeout = 1000L)

  if (proc$is_alive()) {
    proc$kill()
  }
}

#' Start taking repeated measures.
#'
#' @param func Callback to invoke for the measures. It should return a list of metrics.
#' @param period The time interval to wait between measurements, in seconds.
#' @param filename Measures will be appended to this file, using the JSONL format.
#' @param log Log the output of the background process to this file. Disabled if `NULL`.
#' @param startup_timeout Stop waiting for the background process startup after this timeout, in milliseconds.
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
bgmeter_start <- function(func, period, filename, log = NULL, startup_timeout = 2000L) {
  witness <- fs::file_temp(pattern = "bgmeter", ext = "startup")
  
  proc <- callr::r_bg(
    function(func, period, filename, witness) {
      fs::file_create(witness)
    
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
    args = list(func, period, filename, witness),
    stdout = if (is.null(log)) "|" else log,
    stderr = if (is.null(log)) "|" else log
  )

  good <- FALSE
  delay <- 100L
  for (i in seq_len(startup_timeout %/% delay)) {
    proc$wait(timeout = delay)

    started <- fs::file_exists(witness)
    if (started) {
      fs::file_delete(witness)
      good <- TRUE
      break
    }
        
    res <- proc$get_exit_status()
    if (!is.null(res)) {
      if (res == 0) {
        good <- TRUE
        break
      } else {
        msg <- c(
          "The meter failed.",
          "x" = "The background process failed.",
          "i" = paste0("Exit code: ", res)
        )
        if (!is.null(log)) {
          msg <- c(msg, "i" = "Check the log file for any error message.")
        }
      
        cli_abort(msg)
      }
    }
  }

  if (good) {
    return(BgMeter$new(proc))
  }

  kill_proc(proc)
  
  msg <- c(
    "The meter failed.",
    "x" = "The background process did not start properly."
  )
  if (!is.null(log)) {
    msg <- c(msg, "i" = "Check the log file for any error message.")
  }
  cli_abort(msg)
}

#' Stop taking measurements.
#'
#' @param meter A `BgMeter` instance.
#'
#' @importFrom cli cli_abort
#' @export
bgmeter_stop <- function(meter) {
  res <- meter$close()
  if (res != 0) {
    cli_abort(c(
      "The meter failed.",
      "x" = "The background process failed.",
      "i" = paste0("Exit code: ", res)
    ))
  }
}
