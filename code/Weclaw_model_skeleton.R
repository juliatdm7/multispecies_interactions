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
  P[1] <- 240
  L[1] <- 870
  H[1] <- 970
  C[1] <- 7
  M[1] <- 25
  W[1] <- 8
})

#Writing down and storaging birth and death equations for all elements:


for(t in 1:(nsteps-1)){
  pop.df.0 <- within(pop.df.0,{
    plant_birth <- sP * P[t] * (1 - P[t]/Kp) 
    lichen_birth <- sL * L[t] * (1 - L[t]/Kl) 
    forage_birth <- sH * H[t] * (1 - H[t]/Kh)
    caribou_growth <- C[t] * fC * (L[t]/(L[t]+hl)) * (P[t]/(P[t]+hp))
    moose_growth <- M[t] * fM * (H[t]/(H[t]+hh))
    wolf_growth <- W[t] * b * ((C[t]/(C[t]+dC)) + (M[t]/(M[t]+dM)))
    
    plant_death <- (C[t]*lP*P[t])/(hp+P[t])
    lichen_death <- (C[t]*lL*L[t])/(hl+L[t])
    forage_death <- (M[t]*lH*H[t])/(hh+H[t])
    caribou_death <- C[t] * (1 - P[t]/(P[t]+hp)) * (1 - L[t]/(L[t]+hl))
    caribou_pred <- ((W[t]*eC*C[t])/(C[t]+dC))
    moose_death <- M[t] * (1 - H[t]/(H[t]+hh))
    moose_pred <- ((W[t]*eM*M[t])/(M[t]+dM))
    wolf_death <- W[t] * g
    
    
    
    P[t+1] <- max(0, (P[t] + plant_birth - plant_death)) ##plants consumed by Caribou
    L[t+1] <- max(0, (L[t] + lichen_birth - lichen_death)) ##lichen consumed by Caribou
    H[t+1] <- max(0, (H[t] + forage_birth - forage_death)) ##plants consumed by Moose
    
    C[t+1] <- max(0, (C[t] + caribou_growth - caribou_death - caribou_pred))
    
    M[t+1] <- max(0, (M[t] + moose_growth - moose_death - moose_pred))
    
    W[t+1] <- max(0, (W[t] + wolf_growth - wolf_death))
  })
}


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


#When initial population size for all of them is 0, it stays in 0.

#When we set the initial populations as suggested in the tutorial (except for the wolves, who remain at 0), moose perform better than caribou, reaching a population of around 82 individuals. Caribous, on the other hand stay with around 8 individuals.

#When we set the initial populations as suggested in the tutorial (including the wolves), we can see that the moose oscillate mostly between almost 73 individuals and around 23 individuals. 
#However, the caribou population, although it experiences a slight increase in the first few timesteps (reaching a maximum population size of 13 individuals), then it decreases and stays mostly between 3,4 and 5 individuals in the population (which, in reality, it's not trully biologically viable).
#The moose population seems to suffer the most impact, as it goes from being more or less stable, to have oscillations. Caribou populations are affected, definitely, but they were very low to begin with (although, no matter how small, the decrease might as well be what drives them to extinction).


###
#Now, let's test different conditions and management options.
###

##We implement a measurement that limits the number of wolves to 10 individuals/ha maximum.

for(t in 1:(nsteps-1)){
  pop.df.0 <- within(pop.df.0,{
    #Plant populations
    plant_birth <- sP * P[t] * (1 - P[t]/Kp)
    plant_death <- (C[t]*lP*P[t])/(hp+P[t]) 
    
    #Lichen populations
    lichen_birth <- sL * L[t] * (1 - L[t]/Kl)
    lichen_death <- (C[t]*lL*L[t])/(hl+L[t])
    
    #Forage populations
    forage_birth <- sH * H[t] * (1 - H[t]/Kh)
    forage_death <- (M[t]*lH*H[t])/(hh+H[t])
    
    #Caribou populations
    caribou_growth <- C[t] * fC * (L[t]/(L[t]+hl)) * (P[t]/(P[t]+hp))
    caribou_death <- C[t] * (1 - P[t]/(P[t]+hp)) * (1 - L[t]/(L[t]+hl))
    caribou_pred <- ((W[t]*eC*C[t])/(C[t]+dC))
    
    #Moose populations
    moose_growth <- M[t] * fM * (H[t]/(H[t]+hh))
    moose_death <- M[t] * (1 - H[t]/(H[t]+hh))
    moose_pred <- ((W[t]*eM*M[t])/(M[t]+dM))
    
    #Wolf populations
    wolf_growth <- W[t] * b * ((C[t]/(C[t]+dC)) + (M[t]/(M[t]+dM)))
    wolf_death <- W[t] * g
    
    
    P[t+1] <- max(0, (P[t] + plant_birth - plant_death)) ##plants consumed by Caribou
    L[t+1] <- max(0, (L[t] + lichen_birth - lichen_death)) ##lichen consumed by Caribou
    H[t+1] <- max(0, (H[t] + forage_birth - forage_death)) ##plants consumed by Moose
    
    C[t+1] <- max(0, (C[t] + caribou_growth - caribou_death - caribou_pred))
    
    M[t+1] <- max(0, (M[t] + moose_growth - moose_death - moose_pred))
    
    W[t+1] <- max(0, min(10, (W[t] + wolf_growth - wolf_death)))
  })
}


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
  labs(x = "Time", y = "Densities", color="Legend", title = "Limiting wolf max. size at 10")+
  scale_color_manual(values = colors)

#With this measurement, moose populations do not oscillate anymore and stay at around 68 individuals. Caribous mostly stay at 7 individuals (so, they're initial population size).



##We now limit the moose population size at 30 max.

for(t in 1:(nsteps-1)){
  pop.df.0 <- within(pop.df.0,{
    #Plant populations
    plant_birth <- sP * P[t] * (1 - P[t]/Kp)
    plant_death <- (C[t]*lP*P[t])/(hp+P[t]) 
    
    #Lichen populations
    lichen_birth <- sL * L[t] * (1 - L[t]/Kl)
    lichen_death <- (C[t]*lL*L[t])/(hl+L[t])
    
    #Forage populations
    forage_birth <- sH * H[t] * (1 - H[t]/Kh)
    forage_death <- (M[t]*lH*H[t])/(hh+H[t])
    
    #Caribou populations
    caribou_growth <- C[t] * fC * (L[t]/(L[t]+hl)) * (P[t]/(P[t]+hp))
    caribou_death <- C[t] * (1 - P[t]/(P[t]+hp)) * (1 - L[t]/(L[t]+hl))
    caribou_pred <- ((W[t]*eC*C[t])/(C[t]+dC))
    
    #Moose populations
    moose_growth <- M[t] * fM * (H[t]/(H[t]+hh))
    moose_death <- M[t] * (1 - H[t]/(H[t]+hh))
    moose_pred <- ((W[t]*eM*M[t])/(M[t]+dM))
    
    #Wolf populations
    wolf_growth <- W[t] * b * ((C[t]/(C[t]+dC)) + (M[t]/(M[t]+dM)))
    wolf_death <- W[t] * g
    
    
    P[t+1] <- max(0, (P[t] + plant_birth - plant_death)) ##plants consumed by Caribou
    L[t+1] <- max(0, (L[t] + lichen_birth - lichen_death)) ##lichen consumed by Caribou
    H[t+1] <- max(0, (H[t] + forage_birth - forage_death)) ##plants consumed by Moose
    
    C[t+1] <- max(0, (C[t] + caribou_growth - caribou_death - caribou_pred))
    
    M[t+1] <- max(0, min(30, (M[t] + moose_growth - moose_death - moose_pred)))
    
    W[t+1] <- max(0, (W[t] + wolf_growth - wolf_death))
  })
}


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
  labs(x = "Time", y = "Densities", color="Legend", title = "Limiting moose max. size at 30")+
  scale_color_manual(values = colors)


#If we apply this new measure plus the wolf size limit at 10, we see that around time step 70, wolf population gets dangerously close to 0.
#Around the first time steps (14-15), Caribous increase up to 15 individuals, but then they go back down although they do stay close to 9 individuals, which is more than their initizal size of 7.


##Promote wolf hunting
mW <- 0.1

for(t in 1:(nsteps-1)){
  pop.df.0 <- within(pop.df.0,{
    #Plant populations
    plant_birth <- sP * P[t] * (1 - P[t]/Kp)
    plant_death <- (C[t]*lP*P[t])/(hp+P[t]) 
    
    #Lichen populations
    lichen_birth <- sL * L[t] * (1 - L[t]/Kl)
    lichen_death <- (C[t]*lL*L[t])/(hl+L[t])
    
    #Forage populations
    forage_birth <- sH * H[t] * (1 - H[t]/Kh)
    forage_death <- (M[t]*lH*H[t])/(hh+H[t])
    
    #Caribou populations
    caribou_growth <- C[t] * fC * (L[t]/(L[t]+hl)) * (P[t]/(P[t]+hp))
    caribou_death <- C[t] * (1 - P[t]/(P[t]+hp)) * (1 - L[t]/(L[t]+hl))
    caribou_pred <- ((W[t]*eC*C[t])/(C[t]+dC))
    
    #Moose populations
    moose_growth <- M[t] * fM * (H[t]/(H[t]+hh))
    moose_death <- M[t] * (1 - H[t]/(H[t]+hh))
    moose_pred <- ((W[t]*eM*M[t])/(M[t]+dM))
    
    #Wolf populations
    wolf_growth <- W[t] * b * ((C[t]/(C[t]+dC)) + (M[t]/(M[t]+dM)))
    wolf_death <- W[t] * g
    
    
    P[t+1] <- max(0, (P[t] + plant_birth - plant_death)) ##plants consumed by Caribou
    L[t+1] <- max(0, (L[t] + lichen_birth - lichen_death)) ##lichen consumed by Caribou
    H[t+1] <- max(0, (H[t] + forage_birth - forage_death)) ##plants consumed by Moose
    
    C[t+1] <- max(0, (C[t] + caribou_growth - caribou_death - caribou_pred))
    
    M[t+1] <- max(0, min(30, (M[t] + moose_growth - moose_death - moose_pred)))
    
    W[t+1] <- max(0, (W[t] + wolf_growth - wolf_death))
  })
}


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
  labs(x = "Time", y = "Densities", color="Legend", title = "Limiting moose max. size at 30")+
  scale_color_manual(values = colors)












