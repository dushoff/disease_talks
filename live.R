################## Measles in London #######################

## Parameters

### Key
R0 <- 18 ## unitless

D <- 10 ## day
beta <- R0/D ## 1/day

mu <- 0
nu <- 0

#### Other
N <- 7.8e5 ## people
deltaT <- 0.1 ## day
propSusc <- 0.5
FinTime <- 100

## State variables

S <- propSusc*N ## people
I <- 100  ## people
R <- N - S - I ## people
time <- seq(0, FinTime, by=deltaT)

print(Reff <- R0*S/N)

## Simulate
Sv <- Iv <- Rv <- numeric(length(time))

for (i in 1:length(time)){
	Sv[[i]] <- S
	Iv[[i]] <- I
	Rv[[i]] <- R

	## Transition rates (population level) [people/day]
	inf <- beta*S*I/N
	recov <- I/D

	## Move the people
	S <- S + (-inf)*deltaT
	I <- I + (inf-recov)*deltaT
	R <- R + (recov)*deltaT
}

sim <- data.frame(time
	, S=Sv
	, I=Iv
	, R=Rv
)

print(sim)

library(tidyr)

longsim <- gather(sim, class, people, S:R)
print(longsim)

library(ggplot2)
theme_set(theme_bw())

print(
	ggplot(longsim, aes(x=time, y=people, color=class))
	# + geom_point()
	+ geom_line()

)
