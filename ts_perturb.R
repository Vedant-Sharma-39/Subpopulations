library("deSolve")
library("reshape2")
library("dtwclust")
library("pbapply")

t  <-  10
del_t  <-  0.5
times <- seq(0,t,by=del_t)

generate_ts <- function(times, state, parameters){
  eqn <- function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dx <- rx * (1 - (c*x + y)) - b * (1 - a)* x + b * (1 + a)* y 
      dy <- ry * (1 - (x + d*y)) + b * (1 - a)* x - b * (1 + a)* y 
      return(list(c(dx,dy)))})}

  out <- ode(y = init, times = times, eqn, parms = parameters, method = "adams")
  return(((as.matrix(out)[, 2:3])))}


function  <- steadystate(a){
    return(c(x = a/(a + 1), y = 1/(a + 1)))
}
bias = 1
init <- steadystate(bias)

# Since steady state depends only on a, we will only change b

rx_range = c(0.5, 1, 1.5)
ry_range = c(0.5, 1, 1.5)
a_range = c(bias)
b_range = seq(0.1 ,0.9 , 1)
c_range = seq(1, 3, 0.5)
d_range = seq(1, 3, 0.5)

param_sets <- expand.grid(rx = rx_range, ry = ry_range,a = a_range,b = b_range, c = c_range,d = d_range)

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


