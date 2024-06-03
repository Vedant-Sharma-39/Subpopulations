library("deSolve")
library("reshape2")
library("dtwclust")
library("pbapply")

t = 4
del_t = 0.01
times <- seq(0,t,by=del_t)

generate_ts <- function(times, state, parameters){
  eqn <- function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dx <- rx * (1 - (c*x + y)) - a*x + b*y 
      dy <- ry * (1 - (x + d*y)) + a*x - b*y
      return(list(c(dx,dy)))})}

  out <- ode(y = init, times = times, eqn, parms = parameters, method = "lsoda")
  return(((as.matrix(out)[, 2:3])))}


init <- c(x = 0.01, y = 0.01)

# Generating parameter sets
rx_range = c(0.8, 1.2, 0.2)
ry_range = c(0.8, 1.2, 0.2)
a_range = seq(0.05, 0.6, 0.2)
b_range = seq(0.05 ,0.6 , 0.2)
c_range = seq(1, 5, 2)
d_range = seq(1, 5, 2)

param_sets <- expand.grid(rx = rx_range, ry = ry_range,a = a_range,b = b_range, c = c_range,d = d_range)
save(param_sets, file = "Data/param_sets.RData")

# Generating time series
result_list <- pblapply(
  1:nrow(param_sets), 
  function(i) generate_ts(times, init, as.list(param_sets[i,])))

names(result_list) <- apply(param_sets, 1, function(x) paste(x, collapse = "_"))

# Save time series
save(result_list, file = "Data/result_list.RData")

# Calculate total population time series from result_list
load("Data/result_list.RData")
total_pop <- lapply(result_list, function(x) rowSums(x))
save(total_pop, file = "Data/total_pop.RData")

