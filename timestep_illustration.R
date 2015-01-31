library(data.table)

SimpleDiffusion = function(std,step.num){
  # Initialize output vector with length equal to number of simulation runs
  out = data.table(simulation.number = rep(1:200,each=step.num+1),
                   state.number = rep(0:step.num,200),
                   integrator.state=rep(0,200*(step.num+1)))
  
  setkey(out,simulation.number,state.number)
                   
  # Now for each simulation run
  for (i in seq_len(200)){
    # And within run for each step up to step.num (existing global variable at the point of execution)
    out[J(i,1),integrator.state:=rnorm(1,0,std)]
    for (j in 2:step.num){
      # We caluculate the state of the accumulator
      
      out[J(i,j),integrator.state:=out[J(i,j-1),integrator.state] + rnorm(1,0,std)]
      print(i)
    }
  }
  # We return the vector with all final states
  return(out)
}

dat = SimpleDiffusion(1,5)


p = ggplot(data=dat,aes(y=integrator.state,x=state.number,group=simulation.number)) + geom_line(linetype="dashed", size=0.3) + theme_bw(base_size=16) + theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20)) + 
    ggtitle("EXAMPLE OF INTEGRATOR STATES BY TIMESTEPS") + 
    ylab("INTEGRATOR STATE") + 
    xlab("STEP NUMBER") + geom_vline(xintercept=1,linetype="dashed",color="red",size=2) + geom_vline(xintercept=5,linetype="dashed",color="blue",size=2) + scale_y_continuous(limits=c(-8,8))

p.2 = ggplot(data=dat[state.number==1],aes(x=integrator.state,y=..density..)) + geom_density(fill="red",alpha=0.4,size=0.5,color="red") + scale_x_continuous(limits=c(-8,8)) + coord_flip() + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p.3 = ggplot(data=dat[state.number==5],aes(x=integrator.state,y=..density..)) + geom_density(fill="blue",alpha=0.4,size=0.5,color="blue") + scale_x_continuous(limits=c(-8,8)) + coord_flip() + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


library(Cairo)
CairoSVG("densitystep5.svg", 6, 6, bg="transparent")
p.3
dev.off()

