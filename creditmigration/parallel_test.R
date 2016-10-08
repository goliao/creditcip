

# require('doParallel')
# getDoParWorkers()
# getDoParRegistered()
# getDoParName()
# getDoParVersion()




# max.eig <- function(N, sigma) {
#      d <- matrix(rnorm(N**2, sd = sigma), nrow = N)
#      #
#      E <- eigen(d)$values
#      #
#      abs(E)[[1]]
#  }
# max.eig(5, 1)
# foreach(n = 1:5) %dopar% max.eig(n, 1)


# # install.packages('doMC')
# # install.packages('rbenchmark')
# library(doMC)
# library(rbenchmark)

# registerDoMC(cores=4)
# benchmark(
#      foreach(n = 1:50) %do% max.eig(n, 1),
#      foreach(n = 1:50) %dopar% max.eig(n, 1)
#  )

