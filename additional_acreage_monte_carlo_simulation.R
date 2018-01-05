# Goal: estimate the additional acreage needed at a given rock coverage in order to sustain 
# the level of fish standing stock required in the SONGS permit: 28 US tons.
# 
# General approach: combine the expected future fish standing stock of the 
# existing 174-acre Wheeler North Reef with the expected future fish standing 
# stock of the additional acreage. Estimations of expected future fish standing
# stock are generated using a Monte Carlo (MC) simulation and selecting summary statistics
# for a given degree of certainty (e.g. 95%) and acreage (e.g. 175 acres).
#
# Author: Nathan Spindel
# Date: 2017-11-01

# Store the current directory
initial.dir<-getwd()

# Change to desired working directory
working.directory <- file.path("/Volumes", "workspaces", "nate", "public", "MC_acreage_estimation")
setwd(working.directory)

###########################################################################################################
################################ SET SIMULATION VARIABLES BELOW ###########################################
# Reproducible results mode setting. If this value is set to TRUE, then the MC simulation 
# will be given a specific seed value, yielding exactly reproducible results. If this value 
# is set to FALSE, then the MC simulation will run without a specific seed value, which will 
# yield slightly different results each time the simulation is run due to the fact that a 
# random number generator algorithm is involved.
reproducible.results.mode <<- FALSE
# reproducible.results.mode <<- TRUE
# Reef acreage used for estimation of expected future fish standing stock.
reef.acreage = 175
# Degree of certainty used for predictive modeling.
degree.of.certainty <<- 90
# Rock coverage design used for estimation of additional reef acreage ("LOW","MEDIUM","HIGH").
rock.coverage.design <<- "LOW"
# rock.coverage.design <<- "MEDIUM"
# rock.coverage.design <<- "HIGH"
# Set bin size for acreage estimate. (e.g. , 1-acre increments, 5-acre increments, 10-acre increments, ...)
bin.size <<- 1
# Store the desired number of iterations per MC simulation run.
reps.per.run <<- 5
# Store the desired amount of MC simulation runs.
runs <<- 5
###########################################################################################################
###########################################################################################################

# Switch for the output of the MC simulation (i.e. tonnage or acres). DO NOT ADJUST THIS.
output <<- "tonnage"
# Load functions
source('./functions.R')
# Load necessary packages, install them if they are not already installed.
packages <- c("TTR", "pbapply", "vcd")
ipak(packages)

# Execute the MC simulation the number of times set in "runs".
for (i in 1:runs) {
  print(i)
  # Call the function multiMC(), passing the desired number of iterations per run as an argument.
  # Store the results of each simulation run in a matrix called "multi.mc.results".
  multi.mc.results[i,] <- runMC(reps.per.run)
}

# Calculate average expected future fish standing stock on existing Wheeler North Reef, as well as
# tonnage deficit with respect to the 28-ton standard over the number of MC runs performed.
mean.wnr.expected.fish.standing.stock <<- mean(as.numeric(multi.mc.results[, 1]))
mean.deficit.tons <<- 28 - mean.wnr.expected.fish.standing.stock
existing.wnr.expectation.message <<- paste("After running", sep = " ", runs, "MC simulations with", reps.per.run, "iterations each, predictive model suggests with a degree of certainty of 95% that the average future fish standing stock on the existing Wheeler North Reef will be", mean.wnr.expected.fish.standing.stock, "US tons with an expected deficit of", mean.deficit.tons, "US tons with respect to the 28-ton standard.")

# Switch for the output of the MC simulation (i.e. tonnage or acres). DO NOT ADJUST THIS.
output <<- "acres"
# Get the closest acreage value from the percentile matrix that corresponds to the given the average tonnage deficit calculated above.
acres.needed <- getAcresNeeded(mean.deficit.tons)
# Render results summary
summary.message
multi.mc.results
print(existing.wnr.expectation.message)
print(acres.needed)
acresValidation(reps.per.run)

# Render a demonstration of the normality of the bootstrapping procedure
plotNormalFitHist()
testGoodnessOfFit()

# Cleanup. Reset to initial working directory upon execution of full script.
setwd(initial.dir)