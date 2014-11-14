############ CDC Meltzer et al. model
## re-implementation by CABP
## Core Functions

rm(list=ls())

############ MATH UTILITIES

lnorm_params <- function(mn, sd) {
	## lnorm uses mean, sd of the underlying normal distro, not the desired output lognormal distribution
	mn2 <- mn^2; sd2 <- sd^2
	return(list(meanlog = log(mn2/sqrt(sd2+mn2)), sdlog = sqrt(log(1+sd2/mn2))))
}

erlang_params <- function(mean, sd) {
	## using gamma, which takes k (shape) and lambda (rate) params;
	## erlang distros are gammas where k is an integer
	k <- round((mean/sd)^2) ## round to integer k
	lambda <- k/mean ## resulting distro will have exact mean, different sd
	return(list(shape=k, rate = lambda))
}

distribution <- function(model, period_max, ...) with(list(...), {
	mod <- switch(model, ## select/parameterize cdf based on requested model
    lnorm = list(cdf=plnorm, params = lnorm_params(period_mean, period_sd)),
    # log-normal; common waiting time in stochastic models
    gamma = list(cdf=pgamma, params = erlang_params(period_mean, period_sd)),
    # erlang; translates directly to typical ODE models
    delta = list(cdf=function(q) (q == period_max)+0, params=list())
    # delta; provides for fixed-day distribution
  )
	mod$params$q <- 0:(period_max-1)
  y <- c(with(mod, do.call(cdf, params)),1)
  return(list(
    days=mod$params$q,
    cdf=y,
    pdf=y[-1]-y[-(period_max+1)]
  ))
})

############ INITIALIZATION METHODS

## initialize an exposed pop. data structure
init_E <- function(E_day_PDF) {
	inc_period_max <- length(E_day_PDF)
  matrix(0,
    nrow = 1, # only one type of incubation
    ncol = inc_period_max, # one column for each days-left category
    dimnames = list(
      people = "incubating",
      days_left = 1:inc_period_max
    )
  )
}

## initialize an infectious pop. data structure
init_I <- function(I_day_PDF, treatment_allocation) {
	inf_period_max <- length(I_day_PDF)
  matrix(0,
  	nrow = length(treatment_allocation), # one row for each treatment
  	ncol = inf_period_max, # one column for each days-left category
    dimnames = list(
      treatment = names(treatment_allocation),
      days_left = 1:inf_period_max
    )
  )
}

## initialize a bed occupancy data structure
init_beds <- function(treatment_durations) {
  max_stay <- max(treatment_durations)
  matrix(0,
    ncol = max_stay, # a column for each future out-flow from beds
    nrow = 1, # one kind of bed occupation
    dimnames = list(
      occupancy = "count",
      days_left = 1:max_stay
    )
  )
}

############ ITERATION METHODS

## distribute new infections into time-until-infectious categories
add_E <- function(new_infection_count, E_day_PDF) {
  return(new_infection_count*E_day_PDF)
}

## increment exposed population days
advance_E <- function(current_E) {
  return(list(E=c(current_E[-1],0), add_I=current_E[1]))
}

## make a complete exposed pop. update
E_step <- function(current_E, new_infection_count, E_day_PDF) {
  update <- advance_E(current_E)
  update$E <- update$E + add_E(new_infection_count, E_day_PDF)
  return(update)
}

alloc_I <- function(add_I, treatment_distribution, I_day_PDF) {
	return(outer(treatment_distribution, I_day_PDF)*add_I)
}

## make an infectious population step
I_step <- function(current_I, add_I, treatment_allocation, I_day_PDF) {
	admits <- alloc_I(add_I, treatment_allocation, I_day_PDF)
  new_I <- cbind(current_I[,-1], 0) + admits
  colnames(new_I) <- 1:(dim(current_I)[2])
  return(list( I = new_I, admits = admits))
}

## make a beds step
beds_step <- function(current_beds, admits, treatment_durations, hosp_prob) {
	new_I <- rowSums(admits) ## all infectious assumed to use full hosp duration, regardless of infection period
  beds <- c(current_beds[-1], 0)
  beds[treatment_durations] <- beds[treatment_durations] + new_I*hosp_prob
  names(beds) <- 1:length(current_beds)
  return(beds)
}

## compute the infection rate based on infectious pop. in various
## treatment settings, transmission rates for those settings
infection_rate <- function(current_I, treatment_transmissions) {
  return(sum(rowSums(current_I)*treatment_transmissions))
}

## update I, E, and beds for a single step
step <- function(
  last_step_results, susceptible_proportion, # from last step results
  treatment_distribution, introductions, # from scenario
  treatment_transmissions, treatment_durations, hosp_proportions, E_day_PDF, I_day_PDF # assumed static parameters
) with(last_step_results, {
  new_infections <- 
    introductions + infection_rate(I, treatment_transmissions)*susceptible_proportion
  new_E <- E_step(E, new_infections, E_day_PDF)
  new_I <- I_step(I, new_E$add_I, treatment_distribution, I_day_PDF)
  new_beds <- beds_step(beds, new_I$admits, treatment_durations, hosp_proportions)
  return(list(
    I=new_I$I, E=new_E$E,
    incidence=new_E$add_I, infection=new_infections, beds=new_beds
  ))
})

simulate <- function(scenario = defaults) with(scenario, { # scenario provides all sim parameters; see defaults
  ## error checking
	if (!is.numeric(simulation_duration) || (simulation_duration <= 0) || (simulation_duration != floor(simulation_duration)))
		stop(paste0("simulation_duration is not a positive integer: ",simulation_duration))
	if (sum(I_day_PDF) != 1)
		stop("I_day_PDF does not sum to 1.")
	if (sum(E_day_PDF) != 1)
		stop("E_day_PDF does not sum to 1.")
	if ((length(treatment_durations) == 0) || any(treatment_durations != floor(treatment_durations)))
    stop("treatment durations not properly specified.")
	#################
	
	treatment_allocation <- treatment_distribution(1)
	
	######
	if (sum(treatment_allocation) != 1)
	  stop(paste0("treatment allocation at time 1 does not sum to 1."))
	######
  
	results <- array(0,
    dim = c(simulation_duration, 3),
    dimnames = list(
      day = 1:simulation_duration,
      results = c("currently_sick", "cumulative_sick", "beds_occupied")
    )
  )
	
	I0 <- init_I(I_day_PDF, treatment_allocation)
  E0 <- init_E(E_day_PDF)
  
  beds0 <- init_beds(treatment_durations)
	
  laststep <- step(list(I=I0, E=E0, incidence=0, infection=0, beds=beds0), 1,
  	treatment_allocation, introductions(1),
    treatment_transmissions, treatment_durations, hosp_proportions, E_day_PDF, I_day_PDF
  )
  
  results[1,] <- with(laststep, { c(sum(I), incidence, sum(beds)) })
  susceptibles <- N0 - laststep$infection
  susceptible_proportion <- susceptibles/N0
  for (t in 2:simulation_duration) {
  	treatment_allocation <- treatment_distribution(t)
    takestep <- step(
    	laststep, susceptible_proportion,
    	treatment_allocation, introductions(t),
      treatment_transmissions, treatment_durations, hosp_proportions, E_day_PDF, I_day_PDF
    )
    results[t,] <- with(laststep, { c(sum(I), results[t-1,2]+incidence, sum(beds)) })
    susceptibles <- max(susceptibles - takestep$infection, 0)
    susceptible_proportion <- susceptibles/N0
    laststep <- takestep
  }
  return(results)
})

## test code

accepted_file_types = c(
  'text/csv',
  'text/comma-separated-values',
  'text/tab-separated-values',
  'text/plain',
  '.csv',
  '.tsv'
)

treat_ref <- read.table(file = "./cdc_scenario1_distros.csv", header = T)
intro_ref <- read.table(file = "./cdc_scenario1_intros.csv", header = T)
cases_ref <- read.table(file = "./cdc_reported_cases.csv", header = F, col.names = c("date","cum.inc"))

yield_intervention <- function(ref.table) {
  return(function(t) {
    as.numeric(ref.table[max(which(ref.table$start_day <= t)),-1])
  })
}

yield_introduction <- function(ref.table) {
  return(function(t) {
    ind <- which(intro_ref[,1] == t)
    ifelse(length(ind) != 0, intro_ref[ind,2], 0)
  })
}

defaults <- list(
	choices = c(LogNormal = "lnorm", Erlang = "gamma", Delta = "delta"),
	inf_period_mean = 2.5,
	inf_period_sd = 1,
  inf_period_max = 6,
	inc_period_mean = 6.3,
	inc_period_sd = 3.31,
	inc_period_max = 25,
	infective_treatments = c(         # the treatment options
		hospital = "Hospital",
		other_isolating_care = "Other Isolating Care",
		non_isolating_care = "Non-isolating Care"
	),
	treatment_durations = c(         # the stay duration by treatment option
		hospital = 12,
		other_isolating_care = 7,
		non_isolating_care = 4
	),
	hosp_proportions = c(
		hospital = 1,
		other_isolating_care = 0.5,
		non_isolating_care = 0.2	
	),
	treatment_transmissions = c(     # the transmission rates by treatment options
		hospital = 0.02,
		other_isolating_care = 0.03,
		non_isolating_care = 0.3
	),
	N0 = 10^7,
	simulation_duration = 210,
	treatment_distribution = yield_intervention(treat_ref),
	introductions = yield_introduction(intro_ref),
  inf_pdf_ref = c(0,0,0,0,0,1.0),
  inc_pdf_ref = list(
    cdc =     c(0.0000, 0.0196, 0.0860, 0.1530, 0.1606, 0.1404, 0.1220, 0.0858, 0.0646, 0.0524, 0.0336, 0.0246, 0.0150, 0.0132, 0.0070, 0.0054, 0.0030, 0.0036, 0.0030, 0.0016, 0.0018, 0.0016, 0.0012, 0.0006, 0.0004),
    eichner = c(0.0000, 0.0000, 0.0000, 0.0000, 0.0036, 0.0144, 0.0302, 0.0604, 0.0904, 0.1000, 0.1034, 0.0966, 0.0958, 0.0868, 0.0686, 0.0538, 0.0470, 0.0422, 0.0266, 0.0210, 0.01648, 0.01668, 0.01048, 0.00828, 0.00728),
    legrand = c(0.0036, 0.0570, 0.1338, 0.1510, 0.1320, 0.1162, 0.0942, 0.0684, 0.0552, 0.0420, 0.0374, 0.0240, 0.0180, 0.0138, 0.0102, 0.0102, 0.0060, 0.0048, 0.0032, 0.0038, 0.00244, 0.00344, 0.00324, 0.00304, 0.00304)
  )
)

defaults$inc_model <- defaults$choices["LogNormal"]
defaults$inf_model <- defaults$choices["Delta"]
defaults$E_day_PDF <- with(defaults, distribution(inc_model, inc_period_max, period_mean = inc_period_mean, period_sd = inc_period_sd)$pdf)
defaults$I_day_PDF <- with(defaults, distribution(inf_model, inf_period_max)$pdf)
results <- data.frame(simulate(defaults))
