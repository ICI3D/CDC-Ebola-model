## helper functions for constructing shiny apps
nestedTabPanel <- function(title, ..., nested="left") tabPanel(title, tabsetPanel(..., position=nested))

inputTabPanel <- function(title, ..., view = div(), download = NULL) if (is.null(download)) {
	tabPanel(title, inputPanel(...), view)
} else {
	tabPanel(title, inputPanel(...), view, div(downloadButton(download, "Download")))
}

inputTabPanelSolid <- function(title, ...) tabPanel(title, div(..., class="shiny-input-panel solid"))

inputCSV <- function(id, label="Upload", expect = NULL) {
	if (!is.null(expect)) customizeInput(
		fileInput, id, label, F, c('text/csv', 'text/comma-separated-values', '.csv', 'text/plain'),
		mouseover = expect
	) else fileInput(id, label, F, c('text/csv', 'text/comma-separated-values', '.csv', 'text/plain'))
}

makeVectorControl <- function(
	prefix, suffix, labels = NULL, values, 
	..., ## args to type
	id.sep = "_", type = numericInput, tests = list(), # null check automatically added
	env = parent.frame(), domain = getDefaultReactiveDomain()
) {
	custenv <- new.env(parent = env)
	custenv$ids   <- paste(prefix, suffix, sep = id.sep)
	custenv$fulltests <- c(list(validators$null_check(suffix)), tests)
	return(list(
		control = inputGroup(prefix, suffix, labels, values, ..., id.sep = id.sep, type = type),
		reactor = reactive(test_and_return(input_slice(input, ids), fulltests), env = custenv, domain = domain)
	))
}

inputIntervalPDF <- function(prefix, def_max, abs_max = 60, env = parent.frame(), domain = getDefaultReactiveDomain()) {
	control <- div(
		radioButtons(paste0(prefix,"_pdf_type"), "p.d.f. type", choices = c("manual","functional"), inline = T),
		numericInput(paste0(prefix,"_pdf_type"), "max", value = def_max, min=1, max=abs_max, step=1),
		conditionalPanel(paste0("input.",prefix,"_pdf_type == 'manual'"),
										 div("manual: display file upload, display day entries up to max")
		),
		conditionalPanel(paste0("input.",prefix,"_pdf_type == 'functional'"),
										 div("functional: no file upload, show function selector, inputs for it, renderPrint resulting PDF")
		),
		renderPlot(plotIntervalDistribution(infection_pdf$reactor()), width=300, height=600),
		div("download PDF"),
		div("download plots"),
		class="pdf_panel"
	)
	return(list(control = control, reactor = reactive({}, env=env, domain=domain)))
}



validators <- list(
	positive = function(what) return(list(
		f = function(res) all(res >= 0),
		msg = paste0(what, " not all positive.")
	)),
  integers = function(what) return(list(
    f = function(res) all(res == as.integer(res)),
    msg = paste0(what, " not all integers.")
  )),
	null_check = function(what) return(list(
		f = function(res) all(!sapply(res, is.null)),
		msg = paste0(what," info incomplete.")
	))
)

input_slice <- function(input, ids) sapply(ids, function(which) input[[which]])

test_and_return <- function(res, tests) {
	for (test in tests) with(test, validate(need(f(res), msg)))
	res
}

inputGroup <- function(prefix, suffix, labels=NULL, values, ..., id.sep="_", type=numericInput) {
  ids <- paste(prefix, suffix, sep = id.sep)
  if (length(ids) != length(labels)) {
  	stop(paste0("id and labels lengths do not match: ids ",length(ids)," vs labels ",length(labels),
  							"\nother args: ", prefix, " ", suffix))
  }
  div(mapply(type, ids, labels, values, MoreArgs = list(...), SIMPLIFY = F), class="input_group")
}

probabilityInput <- function(id, label, value, min=0, max=1, step=0.01, type=numericInput) {
  type(id, label, value=value, min=min, max=max, step=step)
}

customizeInput <- function(type, ..., mouseover=NULL, units=NULL) {
	input <- type(...)
	is.tag.list <- any(class(input) == "shiny.tag.list")
	if (!is.null(mouseover)) {
		if (is.tag.list) for (i in 1:length(input)) if (is.null(input[[i]])) next else {
			input[[i]]$attribs$`title` <- mouseover
		} else { ## it is a single tag instead
			input$attribs$`title` <- mouseover
		}
	}
	if (!is.null(units)) {
		if (is.tag.list) for (i in 1:length(input)) if (is.null(input[[i]]) || input[[i]]$name != "label") next else {
			input[[i]]$attribs$`data-units` <- units
		} else { ## it is a single tag instead
			input$attribs$`data-units` <- units
			if (!is.null(input$children))
				for (i in 1:length(input$children))
					if (is.null(input$children[[i]]) || is.null(input$children[[i]]$name) || input$children[[i]]$name != "label") next else {
						input$children[[i]]$attribs$`data-units` <- units
					}
		}
	}
	input
}

swappablePDFControl <- function(prefix, absolute_max = 50, max_value) {
	conditionedInput <- function(day, label, value, ...) conditionalPanel(
		paste0("input.", prefix, "_period_max >= ", day),
		probabilityInput(paste0(prefix, "_pdf_", day), label, value)
	)
	
	customizeInput(
		numericInput, paste0(prefix,"_period_max"), "max", min=1, max = absolute_max, value = max_value, step=1,
		mouseover = "The maximum allowed duration.",
		units = "days"
	)
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