# Author: Alexander Fengler
# Date: Jan 23 2015

# Purpose: This code is designed to check the behavior of the noise term in the ddm when changing the size of timesteps 

# 1. Basic logic of the test is this: I take a simple random distribution with standard deviation as given. (We treat this as drift = 0, sd = x one timestep accumulation in the ddm)
# 2. Now I try to replicate this random distribution, but change the amount of noise terms I add together to produce the final distribution 
#    Essentially this simulates breaking 1. down into more timesteps (so what changes to the parameters if we go from 10ms to 1ms timesteps in the ddm for example)
#    The set of optimal standard deviations (as defined by minimizing the difference between the distribution generated under 1. and the distributions generated under 2. with various numbers of steps should then elucidate the relationship between amount of timesteps and the size of the associated noise term

# Step 1
# I have an initial set of parameters (only standard deviation for now)
# This can simply be determined, as done below and serves as our parameter set for the reference timestep size (unitless in our case):
std = 1
mean = 0 
step.num = 1

# Step 2
# Now I generate a distribution based on a random sampler with potentially varying number of timesteps involved to reach the final distribution

# I want to get out a vector of outcomes

# Define Functions 

SimpleDiffusion = function(std){
  out = rep(0,20000) # 10000 is number of runs of sim
  for (i in seq_len(20000)){
    for (j in seq_len(step.num)){
      out[i] = out[i] + rnorm(1,0,std)
    }
  }
  return(out)
}

LogLikDnorm = function(sd.var.inside){
  
  if (sd.var.inside <= 0){
    LogLik= 9999999 
  } else {
      out = dnorm(vec,0,sd.var.inside)
      LogLik = -sum(log(out))
  }
  return(LogLik)
}

LossFun = function(std){
  if (std <= 0){
    std.loss = 9999999
  return(std.loss)
  } else{
      vec <<- SimpleDiffusion(std)
      sol = fminsearch(LogLikDnorm,1)
      
      std.opti = as.vector(sol$optbase$xopt)
      std.loss = (std.opti - std.fixed)^2
  }
  return(std.loss)
}

# Run optimization and get outcomes
std.fixed = 1
steps = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
solutions = data.frame(steps = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),best.sd = rep(0,15))

for (i in steps){
  step.num <<- i
  sol.full = fminsearch(LossFun,1/step.num)
  solutions$best.sd[solutions$steps == i] = as.vector(sol.full$optbase$xopt)
}

p = ggplot(data=solutions,aes(x=steps,y=best.sd)) + theme_bw(base_size=16) + geom_point(shape = 0, size = 4) + geom_line(linetype="dashed",color="red",size = 2) + ggtitle("Optimal sd by number Steps")

#step.num <<- 10
#sol.full = fminsearch(FullOpti,0.01)

out  = data.frame(steps.1 = rep(0,20000),
            steps.2 = rep(0,20000),
            steps.3 = rep(0,20000),
            steps.4 = rep(0,20000),
            steps.5 = rep(0,20000))



for (i in seq(1:5)){
  step.num = i
  out[,c(paste("steps.",toString(i),sep=""))] = SimpleDiffusion(1)
}

out = gather(out,steps,values)

p.2 = ggplot(data=out,aes(x=values,y = ..density..,fill=steps)) + theme_bw(base_size=16) + geom_density(alpha=0.4) + scale_fill_brewer(palette="Set1")
