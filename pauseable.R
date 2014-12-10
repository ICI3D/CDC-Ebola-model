# from: https://gist.github.com/jcheng5/cc6647a4971b8125c1bb
# See ?reactive for meanings of x, env, quoted, and domain.
# paused indicates the starting state of the pauseable reactive.
require(shiny)
pauseableReactive <- function(x, env = parent.frame(), quoted = FALSE,
  priority = 0, domain = getDefaultReactiveDomain(), paused = FALSE) {
  installExprFunction(x, "func", eval.env = env, quoted = quoted)
  
  vals <- reactiveValues(value = NULL)
  obs <- observe(vals$value <- func(), priority = priority,
    domain = domain, suspended = paused)
  return(structure(
    reactive(vals$value),
    pauseObserver = obs
  ))
}

pause <- function(pausableReactive, pause = TRUE) {
  obs <- attr(pausableReactive, "pauseObserver", exact = TRUE)
  if (isTRUE(pause))
    obs$suspend()
  else
    obs$resume()
}

invokeLater <- function(callback, delayMs,
  domain = getDefaultReactiveDomain()) {
  
  initialized <- FALSE
  obs <- observe({
    if (!initialized) {
      initialized <<- TRUE
      invalidateLater(delayMs, domain)
      return()
    }
    obs$destroy()
    callback()
  }, domain = domain)
}