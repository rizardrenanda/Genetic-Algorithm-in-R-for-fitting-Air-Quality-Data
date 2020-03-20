library("GA", lib.loc="C:/Users/User/Documents/R/win-library/3.4")
#GA TuTorial 1
f <-function(x) abs(x)+cos(x)
curve(f, -20, 20)
fitness <-function(x) -f(x)
GA <-ga(type = "real-valued", fitness = fitness, min = -20, max = 20)
summary(GA)
plot(GA)
curve(f, -20, 20)
abline(v = GA@solution, lty= 3)

#GA TuTorial 2
f <-function(x) (x^2+x)*cos(x)
curve(f, -10, 10)
monitor <-function(obj)
{
  curve(f, -10, 10, main = paste("iteration =", obj@iter))
  points(obj@population, obj@fitness, pch= 20, col = 2)
  rug(obj@population, col = 2)
  Sys.sleep(0.2)
}
GA <-ga(type = "real-valued", fitness = f, min = -10, max = 10, monitor = monitor)
GA <-ga(type = "real-valued", fitness = f, min = -10, max = 10, monitor = NULL)
summary(GA)
abline(v = GA@solution, lty= 3)

#GA TuTorial 3
Rastrigin<-function(x1, x2)
{
  #Sys.sleep(0.1)
  20 + x1^2 + x2^2 -10*(cos(2*pi*x1) + cos(2*pi*x2))
}
system.time(GA1 <-ga(type = "real-valued",
                     fitness = function(x) -Rastrigin(x[1], x[2]),
                     min = c(-5.12, -5.12), max = c(5.12, 5.12),
                     popSize= 50, maxiter= 100, monitor = FALSE,
                     seed = 12345))

system.time(GA2 <-ga(type = "real-valued",
                     fitness = function(x) -Rastrigin(x[1], x[2]),
                     min = c(-5.12, -5.12), max = c(5.12, 5.12),
                     popSize= 50, maxiter= 100, monitor = FALSE,
                     seed = 12345, parallel = TRUE))

# Constrained optimization: knapsack problem
p <-c(6, 5, 8, 9, 6, 7, 3) # profit
w <-c(2, 3, 6, 7, 5, 9, 4) # weight
W <-9 # capacity
knapsack <-function(x)
{
  f <-sum(x*p)
  penalty <-sum(w)*abs(sum(x*w)-W)
  f -penalty
}
GA <-ga("binary", fitness = knapsack, nBits= length(w),
        maxiter= 1000, run = 200, popSize= 20, seed = 12345)

summary(GA)
sum(p*GA@solution)
sum(w*GA@solution)