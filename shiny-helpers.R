## helper functions for constructing shiny apps
nestedTabPanel <- function(title, ..., nested="left") {
	tabPanel(title, tabsetPanel(..., position=nested))
}

inputTabPanel <- function(title, ...) {
  tabPanel(title, inputPanel(...))
}

validators <- list(
	positive = function(what) return(list(
		f = function(res) all(res >= 0),
		msg = paste0(what, " not all positive.")
	)),
  integers = function(what) return(list(
    f = function(res) all.equal(res, as.integer(res), check.attributes = F),
    msg = paste0(what, " not all integers.")
  ))
)

inputGroup <- function(prefix, suffix, labels=NULL, values, ..., id.sep="_", type=numericInput) {
  ids <- paste(prefix, suffix, sep = id.sep)
  if (length(ids) != length(labels)) {
  	stop(paste0("id and labels lengths do not match: ids ",length(ids)," vs labels ",length(labels),
  							"\nother args: ", prefix, " ", suffix))
  }
  mapply(type, ids, labels, values, MoreArgs = list(...), SIMPLIFY = F)
}

probabilityInput <- function(id, label, value, min=0, max=1, step=0.01) {
  sliderInput(id, label, value=value, min=min, max=max, step=step)
}

customizedInput <- function(input, mouseover=NULL, units=NULL) {
  is.tag.list <- any(class(input) == "shiny.tag.list")
  if (!is.null(mouseover)) {
    if (is.tag.list) for (i in 1:length(input)) if (is.null(input[[i]])) next else {
      input[[i]]$attribs$`title` <- mouseover
    } else { ## it is a single tag instead
      input$attribs$`title` <- mouseover
    }
  }
  if (class(units) == "character") units <- shiny::tags$em(paste0("(",units,")"))
  if (class(units) == "shiny.tag") { ## accounts for !is.null case
    if (is.tag.list) {
      input[[length(input)+1]] <- units
    } else {
      input <- list(input, units)
      class(input) <- "shiny.tag.list"
      is.tag.list <- T
    }
  }
  input
}

# output[[sum_check_id]] <- renderText(sum(distros[[prefix]]()$pdf))
# defaults <- list(absolute_max, pdf_max)
inputPDF <- function(
  prefix, defaults, options,
  sum_check_id = paste0(prefix,"_pdf_sum"), cols.wide = 4
) with(defaults, { column(cols.wide, div(class = "pdf",
  customizedInput(
    numericInput(paste0(prefix,"_period_max"), "max", min=1, max=absolute_max, value=pdf_max, step=1),
    mouseover = "The maximum value attained for the distribution described by this PDF.",
    units = 'days'
  ),
  conditionalPanel("input.pdf_type == 'manual'",
                                 div(tags$label("day"), em("p")," (total: ",textOutput(sum_check_id, em),")"),
                                 manualPDF(prefix)
                ),
                conditionalPanel("input.pdf_type === 'functional'",
                                 div(selectInput(
                                   paste0(prefix,"_model"),
                                   "distribution",
                                   choices=choices, selected=def_model
                                 )),
                                 conditionalPanel(paste0("(input.", prefix, "_model === '", choices[1], "') || (input.", prefix, "_model === '", choices[2], "')"),
                                                  div(numericInput(
                                                    paste0(prefix,"_period_mean"),
                                                    "mean",
                                                    min=0.01, max=25, value=def_period_mean, step=0.01
                                                  ), em("(days)")),
                                                  div(numericInput(
                                                    paste0(prefix,"_period_sd"),
                                                    "std.dev.",
                                                    min=0.01, max=25, value=def_period_sd, step=0.01
                                                  ), em("(days)"))
                                 )
                )
  ))
})