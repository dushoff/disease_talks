################## Measles in London #######################

## Parameters

N <- 5e8
beta <- 1.8
D <- 10

#### Minor
deltaT <- 1
FinTime <- 100

## State variables

I <- 1
R <- 0

S <- N-R-I

## Simulate
time <- seq(0, FinTime, by=deltaT)
Sv <- Iv <- Rv <- numeric(length(time))

for (i in 1:length(time)){
	Sv[[i]] <- S
	Iv[[i]] <- I
	Rv[[i]] <- R
	N <- S + I + R

	inf <- beta*S*I/N
	recov <- I/D

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
