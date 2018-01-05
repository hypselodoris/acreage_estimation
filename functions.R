# Set the biomass density and standard error for the given rock coverage design based on data from the Phase I modules.
# Set a sequence of acreage numbers appropriate for the given coverage design.
if (rock.coverage.design == "LOW") {
  # Based on data through 2014 at a rock coverage of 41.11%:
  biomass.density <<- 20.543117
  standard.error <<- 8.571148
  acreage.sequence.prediction <<- seq(180, 280, bin.size)
} else if (rock.coverage.design == "MEDIUM") {
  # Based on data through 2014 at a rock coverage of 63.35%:
  biomass.density <<- 33.151901
  standard.error <<- 13.57446
  acreage.sequence.prediction <<- seq(100, 180, bin.size)
} else if (rock.coverage.design == "HIGH") {
  # Based on data through 2014 at a rock coverage of 80.57%:
  biomass.density <<- 42.286252
  standard.error <<- 22.85576
  acreage.sequence.prediction <<- seq(80, 180, bin.size)
}

# Aggregated adjusted biomass density based on Wheeler North Reef experimental rock module time series data. 
# Based on data through 2014 at a rock coverage of 47.645% (i.e. biomass density at low coverage + upscaling factor to true rock coverage):
# biomass.density.adjusted <<- 24.251482
# standard.error.adjusted <<- 9.213518
biomass.density.adjusted <<- 25.825521
standard.error.adjusted <<- 11.35985162

# ipak function: install and load multiple R packages.  check to see if
# packages are installed. Install them if they are not, then load them
# into the R session.
# SOURCE: https://gist.github.com/stevenworthington/3178163
# argument: pkg
ipak <- function(pkg) {
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
# usage: packages <- c('RMySQL', 'ggplot2', 'gridExtra', 'reshape2',
# 'scales', 'grid') ipak(packages)

# Function to generate n number of random deviates from the uniform distribution (between 0 and 1).
# argument: n
# return: numeric vector containing n random probabilities.
randomProbability <- function(n) {
  if (reproducible.results.mode == TRUE) {
    set.seed(200)
  } 
  result <- runif(n)
  return(result)
}

# Variable to store the user-defined degree of certainty in a format compatible with matrices.
desired.percentile <<- 100 - degree.of.certainty

# Unit conversion factors
# convert square meters to acres: 1 m2 = 0.000247105 acre
# convert grams to US tons: 1 US ton = 907185 grams
square.meters.per.acre <<- c(1/0.000247105)
us.tons.per.gram <<- c(1/907185)

# Given a value for acreage and grams/m2, calculate the amount of US tons.
# argument: acres
# argument: gpm2
# return: US tons
tonsFromAcreageAndBiomassDensity <- function(acres, gpm2) {
	result <- gpm2 * square.meters.per.acre * acres * us.tons.per.gram
	return(result)
}

# Set the desired number of random probability values to generate
number.rows <<- 1000

# Monte Carlo (MC) simulation function. 
# return: the percentile calculation for a given acreage.
simulation.function <- function(X, n){
  # Set the desired number of random probability values to generate
  # number.rows = 1000 
  # Call randomProbability function to generate n random probability values and store the results in a numeric vector.
  random.probability.value <- randomProbability(number.rows)
  # Calculate inverse of the standard normal cumulative distribution given a probability, random.probability.value. Store results in a numeric vector.
  inverse.value <- qnorm(random.probability.value) 
  # Calculate normalized values for biomass density using the inverse.value vector. Store results in a numeric vector.
  normalized.biomass.density <-  biomass.density + standard.error * inverse.value
  # Convert biomass density (g/m2) to US tons for each reef area in acreage.sequence. Store results in a matrix, tons.matrix.
  tons.matrix <- outer(normalized.biomass.density,acreage.sequence.prediction, FUN = tonsFromAcreageAndBiomassDensity)
  # Calculate 4-row moving average column-wise over tons.matrix. Store results in a matrix, four.trial.moving.average.matrix.
  four.trial.moving.average.matrix <- apply(tons.matrix[,1:length(acreage.sequence.prediction)], 2, SMA, n=4)
  # Get pairwise maxima for corresponding values in tons.matrix and four.trial.moving.average.matrix. Store results in a matrix, pairwise.max.matrix.
  pairwise.max.matrix <- pmax(tons.matrix, four.trial.moving.average.matrix)
  # Generate a probability sequence for calculating percentiles.
  probability.sequence <- seq(0.01, 0.99, 0.01)
  # Calculate column-wise percentiles from pairwise.max.matrix. Store results in a matrix, percentile.matrix.
  percentile.matrix <- apply(pairwise.max.matrix, 2, quantile, probs = probability.sequence, na.rm = TRUE)
  if (output == "tonnage") {
    # If the switch "output" is set to tonnage, return only the single calculated value from the percentile matrix corresponding to the acreage of interest.
    return(percentile.matrix[desired.percentile, 1])
  } else {
    # If the switch "output" is NOT set to tonnage, return the entire set of tonnage calculations at each acreage in acreage.sequence.
    return(percentile.matrix[desired.percentile, ])
  }
}

# Monte Carlo (MC) simulation function. NOTE: this function is very similar to simulation.function, it just forces the calculation to be made based
# on the acreage of the existing Wheeler North Reef: 175 acres.
# return: the percentile calculation for a given acreage.
wnr.scale.simulation.function <- function(X, n){
  wnr.scale.min.acreage = 175
  wnr.scale.max.acreage = 207
  wnr.scale.bin.size = 1
  wnr.scale.acreage.sequence <- seq(wnr.scale.min.acreage, wnr.scale.max.acreage, wnr.scale.bin.size)
  wnr.biomass.density.adjusted <<- biomass.density.adjusted
  wnr.standard.error.adjusted <<- standard.error.adjusted
  # Set the desired number of random probability values to generate
  # number.rows = 1000 
  # Call randomProbability function to generate n random probability values and store the results in a numeric vector.
  random.probability.value <- randomProbability(number.rows)
  # Calculate inverse of the standard normal cumulative distribution given a probability, random.probability.value. Store results in a numeric vector.
  inverse.value <- qnorm(random.probability.value) 
  # Calculate normalized values for biomass density using the inverse.value vector. Store results in a numeric vector.
  normalized.biomass.density <-  wnr.biomass.density.adjusted + wnr.standard.error.adjusted * inverse.value
  # Convert biomass density (g/m2) to US tons for each reef area in acreage.sequence. Store results in a matrix, tons.matrix.
  tons.matrix <- outer(normalized.biomass.density,wnr.scale.acreage.sequence, FUN = tonsFromAcreageAndBiomassDensity)
  # Calculate 4-row moving average column-wise over tons.matrix. Store results in a matrix, four.trial.moving.average.matrix.
  four.trial.moving.average.matrix <- apply(tons.matrix[,1:length(wnr.scale.acreage.sequence)], 2, SMA, n=4)
  # Get pairwise maxima for corresponding values in tons.matrix and four.trial.moving.average.matrix. Store results in a matrix, pairwise.max.matrix.
  pairwise.max.matrix <- pmax(tons.matrix, four.trial.moving.average.matrix)
  # Generate a probability sequence for calculating percentiles.
  probability.sequence <- seq(0.01, 0.99, 0.01)
  # Calculate column-wise percentiles from pairwise.max.matrix. Store results in a matrix, percentile.matrix.
  percentile.matrix <- apply(pairwise.max.matrix, 2, quantile, probs = probability.sequence, na.rm = TRUE)
  if (output == "tonnage") {
    # If the switch "output" is set to tonnage, return only the single calculated value from the percentile matrix corresponding to the acreage of interest.
    return(percentile.matrix[5, 1])
  } else {
    # If the switch "output" is NOT set to tonnage, return the entire set of tonnage calculations at each acreage in acreage.sequence.
    return(percentile.matrix[5, ])
  }
}
# This function runs the Monte Carlo simulation with the desired number of iterations per run (i.e. reps.per.run).
# argument: reps.per.run
# return: a matrix of statistics for a single MC run
runMC <- function(reps.per.run) {
  # Run MC simulation with the desired number of iterations per run, render progress to the console.
  mc.lst <- pblapply(1:reps.per.run, FUN = wnr.scale.simulation.function)
  # Store matrix of statistics for each run of the MC simulation.
  r = matrix(NA, nrow = 1, ncol = 6)
  r[, 1]  <- mean(unlist(lapply(mc.lst, mean)))  # mean
  r[, 2]  <- var(unlist(lapply(mc.lst, mean)))  # variance
  r[, 3]  <- r[, 1] - 1.96 * sqrt(r[, 2])  # lower CI 95 %
  r[, 4]  <- r[, 1] + 1.96 * sqrt(r[, 2])  # upper CI 95 %
  r[, 5]  <- reps.per.run # number of iterations
  r[, 6]  <- 28-r[, 1] # deficit tonnage with respect to 28-ton standard
  # Store the nth MC simulation result as a vector.
  nth.mc.run.result = matrix(NA, nrow = 1, ncol = 8)
  nth.mc.run.result[,1:6] <- round(r, 6) # round statistics to 6 decimal places
  nth.mc.run.result[, 7]  <- "47.7%" # user-selected rock coverage
  nth.mc.run.result[, 8]  <- reef.acreage # user-selected number of acres of reef on which to base fish standing stock expectation
  # Return the nth MC simulation run result
  return(nth.mc.run.result)
}

# Pre-allocate a results table for multiple MC simulation runs. 
multi.mc.results = matrix(NA, nrow = runs, ncol = 8, 
  dimnames = list(NULL, c("Mean", "Variance", "Lower 95% CL", "Upper 95% CL", "Number of Iterations", "Tonnage Deficit", "Rock Coverage", "Reef Acreage")))

# Get the position value from vector a that is closest to a given value b.
# return: vector element from a that is closest to given value b
getClosestValue <- function(a, b) {
  return(which.min(abs(a - b)))
}

# Find the acreage that corresponds to the mean deficit tonnage (with respect to the 28-ton standard) based on the series of MC simulation runs, given a percentile and rock coverage.
# argument: mean.deficit.tons
# return: closest acreage value from the percentile matrix to the given mean deficit tonnage
getAcresNeeded <- function(mean.deficit.tons) {
  # Calculate the total number of iterations of all MC simulation runs executed by for loop, store the value in a numeric vector, aggregate.reps.
  aggregate.reps <- reps.per.run*runs
  # Run MC simulation with "aggregate.reps" number of iterations. Render progress bar to the console.
  acres.test <- pblapply(1:aggregate.reps, FUN = simulation.function)
  # Make a matrix: rows = average value from MC run, columns = acreage bins  
  agg <- t(sapply(acres.test, as.matrix))
  # Calculate the mean of each column (i.e. acreage bin) from the agg matrix
  mean.agg <- apply(agg,2,mean)
  # Assign names attribute to each element in the mean.agg vector corresponding to acreage values in acreage.sequence.
  attr(mean.agg, "names") <- acreage.sequence.prediction
  # Get the value for acres that is closest to the calculated mean deficit tonnage (with respect to 28-ton standard).  
  closest.acreage <- getClosestValue(mean.agg, mean.deficit.tons)
  # Return the value for acres that is closest to the calculated mean deficit tonnage (with respect to 28-ton standard).
  return(paste(names(closest.acreage), sep = " ", "acres of", rock.coverage.design, "rock coverage reef required given a degree of certainty of", degree.of.certainty , "%,"))
}

# Generate a sequence of acreage numbers beginning with min.acreage, incrementing by bin.size, terminating with max.acreage. Store sequence in a numeric vector.
# argument: reps.per.run  # number of iterations within each MC simulation run
# return: vector containing named acreage/tonnage values
acresValidation <- function(reps.per.run) {
  # Calculate the total number of iterations of all MC simulation runs executed by for loop, store the value in a numeric vector, aggregate.reps.
  aggregate.reps <- reps.per.run*runs
  # Run MC simulation with "aggregate.reps" number of iterations. Render progress bar to the console.
  acres.test <- pblapply(1:aggregate.reps, FUN = simulation.function)
  # Make a matrix: rows = average value from MC run, columns = acreage bins  
  agg <- t(sapply(acres.test, as.matrix))
  # Calculate the mean of each column (i.e. acreage bin) from the agg matrix
  mean.agg <- apply(agg,2,mean)
  # Assign names attribute to each element in the mean.agg vector corresponding to acreage values in acreage.sequence.
  attr(mean.agg, "names") <- acreage.sequence.prediction
  message.prefix <- " The following are the mean tonnage values for a range of acreage values based on one MC simulation with"
  message.suffix <- aggregate.reps
  validation.message <- paste(message.prefix, sep = " ", message.suffix, "iterations, with a rock coverage of", rock.coverage.design, ", and a degree of certainty of", degree.of.certainty , "%. Use to validate acres needed estimate.")
  # Render a descriptive message to the console re: validation of acres needed estimate.
  print(validation.message)
  # Render the entire set of tonnage calculations at each acreage in acreage.sequence to the console. 
  print(mean.agg)
}

# Plot a histogram of normalized biomass density vector with a normal distribution curve overlay.
plotNormalFitHist <- function() {
  # Call randomProbability function to generate n random probability values and store the results in a numeric vector.
  random.probability.value <- randomProbability(number.rows)
  # Calculate inverse of the standard normal cumulative distribution given a probability, random.probability.value. Store results in a numeric vector.
  inverse.value <- qnorm(random.probability.value) 
  # Calculate normalized values for biomass density using the inverse.value vector. Store results in a numeric vector.
  normalized.biomass.density <-  biomass.density + standard.error * inverse.value
  # Set negative values = 0
  normalized.biomass.density[normalized.biomass.density < 0] <- 0
  # Make a histogram of the normalized.biomass.density
  histogram <- hist(normalized.biomass.density)
  # Overlay a normal distribution curve on histogram and render resulting graphic.
  xfit <- seq(min(normalized.biomass.density), max(normalized.biomass.density), length = 40) 
  yfit <- dnorm(xfit, mean = mean(normalized.biomass.density), sd = sd(normalized.biomass.density)) 
  yfit <- yfit * diff(histogram$mids[1:2]) * length(normalized.biomass.density) 
  lines(xfit, yfit, col = "red", lwd = 2)
}

# Execute a goodness-of-fit test on set of resampled biomass density numbers. 
# This test validates that the randomization algorithm that the resampling 
# protocol uses actually produces a set of normally distributed values.
testGoodnessOfFit <- function() {
  # Call randomProbability function to generate n random probability values and store the results in a numeric vector.
  random.probability.value <- randomProbability(number.rows)
  # Calculate inverse of the standard normal cumulative distribution given a probability, random.probability.value. Store results in a numeric vector.
  inverse.value <- qnorm(random.probability.value) 
  # Calculate normalized values for biomass density using the inverse.value vector. Store results in a numeric vector.
  normalized.biomass.density <-  biomass.density + standard.error * inverse.value
  # Set negative values = 0
  normalized.biomass.density[normalized.biomass.density < 0] <- 0
  # Show result summary for goodness-of-fit test in console
  goodness.of.fit.test <- goodfit(normalized.biomass.density)
  summary(goodness.of.fit.test)
  # Plot goodness-of-fit test on graphing device
  # plot(goodness.of.fit.test)
}

summary.message <<- "Monte Carlo simulation results: "