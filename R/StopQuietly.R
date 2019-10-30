#' StopQuietly
#' 
#' Internal -  Function for suppressing error messages on stop.
#' 
#' @param msg_txt
#' @return Console message indicating break in script.
#' @export
#' @keywords internal

## Stop quietly.  # Put this in its own script?
StopQuietly <- function(msg_txt) {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  message(msg_txt)
  stop()
}