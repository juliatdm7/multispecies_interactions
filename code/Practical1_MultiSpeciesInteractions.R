#Multi-species Interactions Models        15/10/2024


#1.The predator-prey Lotka-Volterra model
#Implementation of the base Lotka-Volterra model
#To do so, we will use the ode() function from deSolve package
graphics.off()


library(deSolve)

#Firstly, we can define the function that we want our ode() function to solve:

LV <- function(t,state,parameters){ ##logistic grown function, that takes a set of parameter values, initial conditions and atime sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    dPx <- alfa*x - beta*x*y ##this is the equation governing the rate of change of our prey population (x)...
    dPy <- delta*x*y - gamma*y ##...and this is the equation governing the rate of change of our predator population (y)
    return(list(c(dPx,dPy))) ## return the rate of change - it needs to be a list
  }
  ) # end with(as.list ...
}

#And, as we can see from the code above, we need to define the parameters that we will input in this function:

state <- c(x=10,y=10) ## the initial population values for both x (prey) and y (predator) populations
parameters <- c(alfa=0.1, ## alfa is the growth rate of the x population,
                beta=0.02, ## beta is the effect of y on the x growth rate,
                delta=0.02, ## gamma is the effect of x on the y growth rate
                gamma=0.4)  ## and gamma is the predator death rate.
times <- seq(0,500,by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y = state, times = times, func = LV, parms = parameters)
out.df <- data.frame(out)

#And now we plot the results given by ode()

library(ggplot2)
ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")

#This plots the evolution of both prey (x) and predator (y) population throughout time.
#However, it's also useful to plot  the changes in populations through time in the phase space (basically plotting y vs. x).
#To do so, we can use the following code:

ggplot(data = out.df)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")

#Now, we can try and change our parameters, and see what happens.

#What happens if we increase alfa (our prey population) quite a bit?... 
state <- c(x=10,y=10) ## the initial population values for both x (prey) and y (predator) populations
parameters <- c(alfa=0.5, ## alfa is the growth rate of the prey (x) population,
                beta=0.02, ## beta is the effect of predator (y) on the prey (x) growth rate,
                delta=0.02, ## gamma is the effect of prey (x) on the predator (y) growth rate
                gamma=0.4)  ## and gamma is the predator (y) death rate.
times <- seq(0,500,by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y = state, times = times, func = LV, parms = parameters)
out.df1 <- out.df
out.df3 <- data.frame(out)
out.df <- cbind(out.df1,out.df2[,c(2,3)],out.df3[,c(2,3)])
names(out.df) <- c("time","x1","y1","x2","y2","x3", "y3")
ggplot(data = out.df)+
  geom_path(mapping=aes(x=x1,y=y1),color="darkred") +
  geom_path(mapping=aes(x=x2,y=y2),color="red") +
  geom_path(mapping=aes(x=x3,y=y3),color="orange") +
  xlim(0,200) +
  ylim(0,300) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")

#Let's increase alfa a bit more...

state <- c(x=10,y=10) ## the initial population values for both x (prey) and y (predator) populations
parameters <- c(alfa=1, ## alfa is the growth rate of the prey (x) population,
                beta=0.02, ## beta is the effect of predator (y) on the prey (x) growth rate,
                delta=0.02, ## gamma is the effect of prey (x) on the predator (y) growth rate
                gamma=0.4)  ## and gamma is the predator (y) death rate.
times <- seq(0,500,by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y = state, times = times, func = LV, parms = parameters)
out.df1 <- out.df
out.df3 <- data.frame(out)
out.df <- cbind(out.df1,out.df2[,c(2,3)],out.df3[,c(2,3)])
names(out.df) <- c("time","x1","y1","x2","y2","x3", "y3")
ggplot(data = out.df)+
  geom_path(mapping=aes(x=x1,y=y1),color="darkred") +
  geom_path(mapping=aes(x=x2,y=y2),color="red") +
  geom_path(mapping=aes(x=x3,y=y3),color="orange") +
  xlim(0,150) +
  ylim(0,150) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator", title="Predator vs. Prey population (increasing alfa)")


#Prey Growth Rate: exponential vs. logistic

LV_LG <- function(t,state,parameters){ ##logistic grown function, that takes a set of parameter values, initial conditions and atime sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    dPx <- alfa*x*(1-x/K) - beta*x*y ##this is the equation governing the rate of change of our prey population (x)...
    dPy <- delta*x*y - gamma*y ##...and this is the equation governing the rate of change of our predator population (y)
    return(list(c(dPx,dPy))) ## return the rate of change - it needs to be a list
  }
  ) # end with(as.list ...
}

state <- c(x=10,y=10) ## the initial population values for both x (prey) and y (predator) populations
parameters <- c(alfa=0.1, ## alfa is the growth rate of the x population,
                beta=0.02, ## beta is the effect of y on the x growth rate,
                delta=0.02, ## gamma is the effect of x on the y growth rate
                gamma=0.4,
                K=30)  ## and gamma is the predator death rate.
times <- seq(0,500,by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y = state, times = times, func = LV_LG, parms = parameters)
out.df <- data.frame(out)
ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")
ggplot(data = out.df)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")
#What we see now it's that both prey and predator populations reach an equilibrium: prey population reaches it at size 20 (which is 2/3 of its carrying capacity K) while the predator population reaches it much lower (at around 2)
#Starting at the same population size, the predator preys heavily on the prey population, which makes it heavily decrease and, as a consequence, the predator population also decreases fast. 
#This allows the prey population to rebounce even overshooting its carrying capacity. 
#The predator peaks as well, as a consequence, but at a lower peak, which has an impact on the prey population, but not as heavy as before.
#The prey population finally reaches an equilibrium at a lower carrying capacity (which makes sense considering that the predator population is negatively impacting it), and so does the predator as a consequence.
#However, it almost seems as the predator reaches the equilibrium even before than the prey. Why?


#Incorporating functional response

#Prey consumption by predators is not always linear.
#The rate at which predators can consume preys is called a functional response.
#There are three types of functional response:
##Type I: linear
##Type II: logistic, where A controls the slope. When A is high, it A indicates poor hunting efficiency by the predator.
#Let's see the effect of differen values of A:
x <- seq(0,50,0.1)
A <- 0.1
y <- x/(1+A*x) 
ggplot()+
  geom_line(mapping=aes(x=x,y=x/(1+A*x)),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey population", y = "Prey consumed")


A <- 1
ggplot()+
  geom_line(mapping=aes(x=x,y=x/(1+A*x)),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey population", y = "Prey consumed")

#As we increase A, we can see that although the curves seems to reach its plateau faster, said plateau is smaller and smaller

#Let's implement this to our model now.

LV_FR <- function(t,state,parameters){ 
  with(as.list(c(state, parameters)),{ 
    dPx <- alfa*x - (beta*x*y)/(1+A*x) 
    dPy <- (delta*x*y)/(1+A*x) - gamma*y 
    return(list(c(dPx,dPy))) 
  }
  ) 
}
state <- c(x=10,y=10) ## the initial population values for both x (prey) and y (predator) populations
parameters <- c(alfa=0.1, ## alfa is the growth rate of the x population,
                beta=0.02, ## beta is the effect of y on the x growth rate,
                delta=0.02, ## gamma is the effect of x on the y growth rate
                gamma=0.4, ## and gamma is the predator death rate.
                K=30,##carrying capacity
                A=0.01) #predator efficiency at hunting 
times <- seq(0,1000,by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y = state, times = times, func = LV_FR, parms = parameters)
out.df <- data.frame(out)
ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")
ggplot(data = out.df)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")
#When we increase A (we lower the predator efficiency), the predator population goes to almost 0.

#Now let's combine both the logistic growth and the functional response in our code

LV_LG_FR <- function(t,state,parameters){ 
  with(as.list(c(state, parameters)),{ 
    dPx <- alfa*x*(1-x/K) - (beta*x*y)/(1+A*x) 
    dPy <- (delta*x*y)/(1+A*x) - gamma*y 
    return(list(c(dPx,dPy))) 
  }
  ) 
}
state <- c(x=10,y=10) ## the initial population values for both x (prey) and y (predator) populations
parameters <- c(alfa=0.1, ## alfa is the growth rate of the x population,
                beta=0.02, ## beta is the effect of y on the x growth rate,
                delta=0.02, ## gamma is the effect of x on the y growth rate
                gamma=0.4, ## and gamma is the predator death rate.
                K=30,##carrying capacity
                A=0.01) #predator efficiency at hunting 
times <- seq(0,500,by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y = state, times = times, func = LV_LG_FR, parms = parameters)
out.df <- data.frame(out)
ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")
ggplot(data = out.df)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")
#In this scenario, both populations reach an equilibrium at x=~25 and y=~2 (although, in reality, the predator population would have probably become extinct as it almost reaches 0 when t=~25)
#This differs from the previous model, where we saw that both populations had very intense oscillations as t increased, never being close to reaching and equilibrium.
#In fact, if we increase A, in the previous model we can see how the prey population grows exponentially, while in this model it reaches an equilibrium at its carrying capacity (K=30)



#A three-species competition Lotka-Volterra model: limiting similarity

#Now, let's change our predator-preyLotka-Volterra Model to a competition Lotka-Volterra model:

LV_C <- function(t,state,parameters){ 
  with(as.list(c(state, parameters)),{ 
    dPx1 <- r1*x1*(1-(x1/K1)-((a12*x2)/K1)) 
    dPx2 <-  r2*x2*(1-(x2/K2)-((a21*x1)/K2))
    return(list(c(dPx1,dPx2))) 
  }
  ) 
}
state <- c(x1=25,x2=25) ## the initial population values for both x (prey) and y (predator) populations
parameters <- c(r1=0.3, ## growth rate of the x1 population,
                r2=0.3, ## growth rate of the x2 population,
                a12=0.9, ## effect of x2 on x1?
                a21=0.9, ## effect of x1 on x2?
                K1=50,##carrying capacity
                K2=30) #predator efficiency at hunting 
times <- seq(0,1000,by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y = state, times = times, func = LV_C, parms = parameters)
out.df <- data.frame(out)
ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x1),color="blue") +
  geom_line(mapping=aes(x=time,y=x2),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")
ggplot(data = out.df)+
  geom_path(mapping=aes(x=x1,y=x2),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Competitor 1", y = "Competitor 2")
#Apparently, species 2 is a better competitor than species 1, as we can see that, as t increases, it slowly reaches its carrying capacity while species 1 slowly reaches extinction (it doesn't simply because the continuous model will not allow the function to reach 0, but still).
#If we change the parameters so that a12=a21=0.9, but do not change initial population size, species 1 decreases its population size and eventually reaches a plateu at size=~15.3. Species 2, on the other hand, decreases at the beginning but then quickly recovers and stabilises at the same value as species 1.
#Both species reach an equilibrium at the same population size, which is around half of their carrying capacity.
#If, f.e., species 1 has a higher carrying capacity (k1=50, k2=30), then said species reaches a plateau at k1, while species 2 becomes extinct, even though, supposedly, they both have the same effect on each other (a12=a21=0.9)
#If, with the same k1 and k2 as before, we set both starting populations at 25 (x1(0)=x2(0)=25), even then species 1 still reaches K1 while species 2 becomes extinct.

#Let's now add a third species to this system

LV_C_3 <- function(t,state,parameters){ 
  with(as.list(c(state, parameters)),{ 
    dPx1 <- r1*x1*(1-(x1/K1)-((a12*x2)/K1)) 
    dPx2 <- r2*x2*(1-(x2/K2)-((a21*x1)/K2))
    dPx3 <- r3*x3*(1-(x3+a31*x1+a32*x2)/K1) #I am not sure why it is K1
    return(list(c(dPx1,dPx2,dPx3))) 
  }
  ) 
}
state <- c(x1=10,x2=10,x3=10) ## the initial population values for both x (prey) and y (predator) populations

alpha.func <- function(mu1,sig1,mu2,sig2,K1,K2,start,end){ ##this is the function to compute the alpha coefficients from the mean and standard deviations of the Gaussian niches of the species and the start and end values of the environment
  niche1 <- K1 * dnorm(seq(start,end,length.out=100),mean=mu1,sd=sig1) ##dnorm() generates the values of the Gaussian. Check ?dnorm
  niche2 <- K2 * dnorm(seq(start,end,length.out=100),mean=mu2,sd=sig2)
  a <- sum(niche1*niche2)/sum(niche1*niche1) ##because we have discrete values, we use a sum to approximate the integral
  return(a)
}
##Let's try different parameter values
D <- 5 ##distance between the niche optima
mu1 <- 10 ##niche optima of species 1
mu2 <- mu1+D ##niche optima of species 2
mu3 <- mu1+2*D ##niche optima of species 3
sig1 <- sig2 <- sig3 <- 10 ##all species niches have the same standard deviation for simplicity
K1 <- 200 ##carrying capacity species 1 and 3
K2 <- 250 ##carrying capacity species 2
start <- 0
end <- 30
a12 <- alpha.func(mu1,sig1,mu2,sig2,K1,K2,start,end)
a13 <- alpha.func(mu1,sig1,mu3,sig3,K1,K1,start,end)
a21 <- alpha.func(mu2,sig2,mu1,sig1,K2,K1,start,end)
a23 <- alpha.func(mu2,sig2,mu3,sig3,K2,K1,start,end)
a31 <- alpha.func(mu3,sig3,mu1,sig1,K1,K1,start,end)
a32 <- alpha.func(mu3,sig3,mu2,sig2,K1,K2,start,end)

##visualise the niches
resource <- seq(start,end,length.out=100)
niche1 <- dnorm(resource,mean=mu1,sd=sig1)*K1
niche2 <- dnorm(resource,mean=mu2,sd=sig2)*K2
niche3 <- dnorm(resource,mean=mu3,sd=sig3)*K1
ggplot()+
  geom_line(mapping=aes(x=resource,y=niche1),color="blue")+
  geom_line(mapping=aes(x=resource,y=niche2),color="red")+
  geom_line(mapping=aes(x=resource,y=niche3),color="darkgreen")

##setup and solve the system of differential equations
parameters <- c(a12=a12, #probability of overlaping niches for species 1 and 2
                a13=a13, #probability of overlaping niches for species 1 and 2
                a21=a21, #probability of overlaping niches for species 2 and 1
                a23=a23, #probability of overlaping niches for species 2 and 3
                a31=a31, #probability of overlaping niches for species 3 and 1
                a32=a32, #probability of overlaping niches for species 3 and 2
                r=0.3, 
                K1 = K1, 
                K2 = K2)
state <- c(x1=10, x2=10, x3=10)

LV_C_3 <- function(t,state,parameters){ 
  with(as.list(c(state, parameters)),{ 
    dPx1 <- r*x1*(1-(x1+a12*x2+a13*x3)/K1) 
    dPx2 <- r*x2*(1-(x2+a21*x1+a23*x3)/K2)
    dPx3 <- r*x3*(1-(x3+a31*x1+a32*x2)/K1) #I am not sure why it is K1
    return(list(c(dPx1,dPx2,dPx3))) 
  }
  ) 
}

times <- seq(0,300,by=0.01) ##a sequence of time steps – uses function seq()
out <- ode(y = state, times = times, func = LV_C_3, parms = parameters)
out.df <- data.frame(out)
ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x3),color="green") +
  geom_line(mapping=aes(x=time,y=x1),color="blue") +
  geom_line(mapping=aes(x=time,y=x2),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")

#With these parameters, the only species that survives seems to be species 2, which seems to be the best competitor

