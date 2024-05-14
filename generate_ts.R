require("deSolve")
require("reshape2")
require("dtwclust")


t = 25
del_t = 0.1
times <- seq(0,t,by=del_t)

generate_ts <- function(times, state, parameters){
  eqn <- function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dx <- x * (1 - (c*x + y)) - a*x + b*y 
      dy <- ry *(1 - (x + d*y)) + a*x - b*y
      return(list(c(dx,dy)))})}

  out <- ode(y = init, times = times, eqn, parms = parameters)
  return((t(as.matrix(out)[, 2:3])))}


init <- c(x = 0.1, y = 0.1)
parameters <- c(ry = 1.25, a=0.1, b=0.1, c =1, d = 1)

# Generating parameter sets
ry_range = seq(1, 1.5, 0.5)
a_range = seq(0, 1, 0.5)
b_range = seq(0 ,1 , 0.5)
c_range = seq(1, 5, 2)
d_range = seq(1, 5, 2)

param_sets <- expand.grid(ry = ry_range,a = a_range,b = b_range, c = c_range,d = d_range)

# Apply the generate_ts function to each row of the parameter sets
result_list <- lapply(1:nrow(param_sets), function(i) generate_ts(times, init, as.list(param_sets[i,])))

subset_result_list <- result_list[sample(length(result_list), 50)]
clustering_result <- tsclust(series = subset_result_list)


cnfg <- compare_clusterings_configs(
    types = c("p"),
    k = 2L:10L
    )


sil = cvi_evaluators("Sil")

clustering_result <- compare_clusterings(
  series = result_list,
  types = c("p"),
  configs = cnfg,
  score.clus = sil$score,
  pick.clus = sil$pick,
  return.objects = TRUE
)

clustering_result$scores
