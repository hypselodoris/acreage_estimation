# acreage_estimation
Monte Carlo simulation used to estimate additional acreage of artificial reef needed

# Goal: estimate the additional acreage needed at a given rock coverage in order to sustain 
# the level of fish standing stock required in the SONGS permit: 28 US tons.
# 
# General approach: combine the expected future fish standing stock of the 
# existing 174-acre Wheeler North Reef with the expected future fish standing 
# stock of the additional acreage. Estimations of expected future fish standing
# stock are generated using a Monte Carlo simulation and selecting summary statistics
# for a given percentile (e.g. 95%) and acreage (e.g. 175 acres).
#
# Script can yield exactly reproducible simulation results or unique simulation results 
# by setting the boolean value for "reproducible.results.mode".   
#
# Author: Nathan Spindel
# Date: 2017-10-31

** NOTE: Use University of California, Berkeley, CA CRAN mirror
   for any package downloads (https://cran.cnr.berkeley.edu/) **
   
1) Download and install R if you don't already have it.

2) Move directory MC_acreage_estimation to your desired location in your file system.
   (e.g. /Users/songs/Desktop)

2) Open MC_acreage_estimation/additional_acreage_monte_carlo_simulation.R with R.

3) In the file additional_acreage_monte_carlo_simulation.R: Correct working directory file 
   path for your computer.
	Example using your Desktop:
	working.directory <- file.path("/Users", "songs", "Desktop", "MC_acreage_estimation")
	setwd(working.directory)

4) Adjust simulation variable settings.
	
5) Select all text within additional_acreage_monte_carlo_simulation.R

6) Copy selected text.

7) Paste selected text into R console.

8) Hit Return. 
   Simulation script will execute on the R console and you should see progress bars at 
   each step. 
   
   Results you care about:
   "multi.mc.results"
   "existing.wnr.expectation.message"
   "acres.needed"
   "acresValidation(reps.per.run)"
