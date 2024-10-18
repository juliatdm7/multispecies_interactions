#Practical 2 - Multi-species Interactions - Biodiversity Under Pressure     18/10/2024



rm(list = ls())
graphics.off()

library(ggplot2)


#############
##Model skeleton
#############

##set up parameter values
sP <- 0.5  # plant annual max growth potential
Kp <- 240  # plant carrying capacity
lP <- 2.68275  # annual plant requirement by an individual caribou
hp <- 300  # maximum rate of plant intake 
sL <- 0.07  # lichen annual max growth potential
Kl <- 870  # lichen carrying capacity
lL <- 2.8275  # annual lichen requirement by an individual caribou
hl <- 400  # maximum rate of lichen intake
sH <- 0.5  # forage annual max growth potential
Kh <- 970  # forage carrying capacity
lH <- 3.1682  # annual forage requirement by an individual moose
hh <- 1000  # maximum rate of moose forage intake
fC <- 1  # max fecundity for caribou (average nr of offspring per individual)
eC <- 1.85  # asymptotic kill rate on caribou per wolf
mC <- "?"
fM <- 1.5  # max fecundity for moose (average nr of offspring per individual)
eM <- 0.6  # asymptotic kill rate on moose per wolf
mM <- 0  # hunting rate on moose
mW <- 0  # hunting rate on wolf
b <- 0.8  # upper limit of annual rate of increase
g <- 0.38  # wolf death rate
dC <- 460  # predation efficieny on caribou
dM <- 46  # predation efficiency on moose



nsteps <- 200
pop.df.0 <- data.frame(time=1:nsteps,P=numeric(nsteps),L=numeric(nsteps),H=numeric(nsteps),C=numeric(nsteps),M=numeric(nsteps),W=numeric(nsteps))

pop.df.0 <- within(pop.df.0,{
  P[1] <- 0
  L[1] <- 0
  H[1] <- 0
  C[1] <- 0
  M[1] <- 0
  W[1] <- 0
})

#Writing down and storaging birth and death equations for all elements:


for(t in 1:(nsteps-1)){
  pop.df.0 <- within(pop.df.0,{
    plant_birth <- sP * pop.df.0$P[t] * (1-pop.df.0$P[t]/Kp) - ((C[t] * lP * pop.df.0$P[t])/(hp + pop.df.0$P[t]))
    lichen_birth <- sL * pop.df.0$L[t] * (1-pop.df.0$L[t]/Kl) - ((C[t] * lL * pop.df.0$L[t])/(hl + pop.df.0$L[t]))
    forage_birth <- sH * pop.df.0$H[t] * (1-pop.df.0$H[t]/Kh) - ((C[t] * lH * pop.df.0$H[t])/(hh + pop.df.0$H[t]))
    caribou_growth <- C[t] * fC * (L[t]/(L[t]+hl)) * (P[t]/(P[t]+hp))
    moose_growth <- M[t] * fM * (H[t]/(H[t]+hh))
    wolf_growth <- W[t] * b * ((C[t]/(C[t]+dC)) + (M[t]/(M[t]+dM)))
    
    plant_death <- (C[t]*lP*P[t])/(hp+P[t])
    lichen_death <- (L[t]*lL*L[t])/(hl+L[t])
    forage_death <- (H[t]*lH*H[t])/(hh+H[t])
    caribou_death <- C[t] * (1 - P[t]/(P[t]+hp)) * (1 - L[t]/(L[t]+hl))
    caribou_pred <- C[t] * ((W[t]*eC*C[t])/(C[t]+dC))
    moose_death <- M[t] * (1 - H[t]/(H[t]+hh))
    moose_pred <- M[t] * ((W[t]*eM*M[t])/(M[t]+dM))
    wolf_death <- W[t] * g
    
    
    
    P[t+1] <- max(0, (P[t] + plant_birth + plant_death)) ##plants consumed by Caribou
    L[t+1] <- max(0, (L[t] + lichen_birth + lichen_death)) ##lichen consumed by Caribou
    H[t+1] <- max(0, (H[t] + forage_birth + forage_death)) ##plants consumed by Moose
    
    C[t+1] <- max(0, (C[t] + caribou_growth - caribou_death - caribou_pred))
    
    M[t+1] <- max(0, (M[t] + moose_growth - moose_death - moose_pred))
    
    W[t] <- max(0, (W[t] + wolf_growth - wolf_death))
  })
}

#When initial population size for all of them is 0, it stays in 0.


colors <- c("Plants"="brown","Lichen"="orange","Shrubs"="green","Caribou"="purple","Moose"="blue","Wolf"="red")
ggplot(data = pop.df.0)+
  geom_line(mapping=aes(x=time,y=P,color="Plants")) +
  geom_line(mapping=aes(x=time,y=L,color="Lichen")) +
  geom_line(mapping=aes(x=time,y=H,color="Shrubs")) +
  geom_line(mapping=aes(x=time,y=C,color="Caribou")) +
  geom_line(mapping=aes(x=time,y=M,color="Moose")) +
  geom_line(mapping=aes(x=time,y=W,color="Wolf")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "Densities", color="Legend")+
  scale_color_manual(values = colors)




















