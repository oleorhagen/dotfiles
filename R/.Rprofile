# The .First function is called after everything else in .Rprofile is executed
.First <- function() {
  # Print a welcome message
  message("Welcome back ", Sys.getenv("USER"),"!\n","working directory is:", getwd())
}

options(digits = 12)                                          # Number of digits to print. Default is 7, max is 15
options(scipen = 2)                                           # Penalty applied to inhibit the use of scientific notation
options(show.signif.stars = FALSE)                            # Do not show stars indicating statistical significance in model outputs
local({
  n <- max(parallel::detectCores() - 2L, 1L)                  # Detect the number of cores available for use in parallelisation
  options(Ncpus = n)                                          # Parallel package installation in install.packages()
  options(mc.cores =  n)                                      # Parallel apply-type functions via 'parallel' package
})
error <- quote(dump.frames("${R_HOME_USER}/testdump", TRUE))  # Post-mortem debugging facilities
