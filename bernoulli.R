## Bernoulli data

y <- c(0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0)


## likelihood function
pv <- seq(0, 1, by = 0.01)
L <- sapply(pv, function(p) prod(dbinom(y, size = 1, prob = p)))

plot(pv, L, type="l")
abline(v=pv[which.max(L)], col=2)

plot(pv, log(L), type="l")
abline(v=pv[which.max(L)], col=2)


## Frequentist GLM
summary(m <- glm(y ~ 1, family=binomial))
plogis(coef(m))
mean(y)
confint(m)


## Bayesian MCMC
library(dclone)
library(rjags)
model <- custommodel("model {
    for (i in 1:n) {
        #Y[i] ~ dbin(p, 1) # Binomial(N,p)
        Y[i] ~ dbern(p) # Bernoulli(p)
    }
    #p ~ dunif(0.001, 0.999)
    p ~ dbeta(1, 3)
}")
dat <- list(Y = y, n = length(y))
fit <- jags.fit(data = dat, params = "p", model = model)
summary(fit)
coef(fit)
plot(fit)

## RUN THE APP
##
## Beta priors, including Uniform (Beta(1, 1))
## and Jeffrey's prior (Beta(0.5, 0.5)) we can see that:
## * different priors, same data lead to different posteriors,
## * same prior, different data lead to different posteriors,
## * as the sample size increases, the posterior is invariant to the prior
##   (eventually degenerate at the true value).

## Frequentist DC
model <- custommodel("model {
    for (k in 1:K) { # -----------------!!! here is DC
        for (i in 1:n) {
            Y[i,k] ~ dbin(p, 1)
        }
    }
    logit_p <- log(p/(1-p))
    p ~ dbeta(0.001, 0.999)
}")
dat <- list(Y = dcdim(data.matrix(y)), n = length(y), K = 1)
dcfit <- dc.fit(data = dat, params = "logit_p", model = model,
    n.clones = c(1,100), unchanged = "n", multiply = "K")
summary(dcfit)
plot(dcfit)
dctable(dcfit)
plot(dctable(dcfit))
dcdiag(dcfit)
plot(dcdiag(dcfit))

dcsd(dcfit)
summary(m)$coef

confint(dcfit)
confint(m)

## What did just happen?
## GO BACK TO SLIDES ----------------------------------------------


set.seed(4321)
n <- 100
p <- 0.6
phi <- 0.4
y <- rbinom(n = n, size = 1, prob = phi)
w <- rbinom(n = n, size = y, prob = p)
table(Y = y, W = w)


## setting up the grid for p and phi
grid <- expand.grid(p = seq(0, 1, by = 0.01),
    phi = seq(0, 1, by = 0.01),
    L = NA)
## the likelihood function
L_fun <- function(w, p, phi) {
    prod((p * phi)^w * (1 - p * phi)^(1 - w))
}
## calculating the likelihood for the grid
for (i in 1:nrow(grid)) {
    grid$L[i] <- L_fun(w = w, p = grid$p[i], phi = grid$phi[i])
}
## plot the likelihood surface
dcpal_reds <- colorRampPalette(c("#f9f2f4", "#c7254e"))
L_mat <- matrix(grid$L, sqrt(nrow(grid)))
image(L_mat,
    xlab = "p", ylab = expression(varphi),
    col = dcpal_reds(12))
abline(h = phi, v = p)
curve((p * phi) / x, 0, 1, add = TRUE, col = 4, lwd = 2)

library(rgl)
open3d()
bg3d("white")
material3d(col = "black")
dcpal_grbu <- colorRampPalette(c("#18bc9c", "#3498db"))
Col <- rev(dcpal_grbu(12))[cut(L_mat, breaks = 12)]
persp3d(L_mat / max(L_mat), col = Col,
    theta=50, phi=25, expand=0.75, ticktype="detailed",
    xlab = "p", ylab = "phi", zlab = "L")

## Bayesian MCMC
model <- custommodel("model {
    for (i in 1:n) {
        Y[i] ~ dbern(phi)
        W[i] ~ dbern(Y[i] * p)
    }
    #p ~ dunif(0.001, 0.999)
    #phi ~ dunif(0.001, 0.999)
    p ~ dbeta(1, 1)
    phi ~ dbeta(0.5, 0.5)
}")
dat <- list(W = w, n = n)
#ini <- list(Y = w)
ini <- list(Y = rep(1, n))
fit <- jags.fit(data = dat, params = c("p", "phi"),
    model = model, init = ini)
summary(fit)
plot(fit)
pairs(fit)


## DC
model <- custommodel("model {
    for (k in 1:K) {
        for (i in 1:n) {
            Y[i,k] ~ dbern(phi)
            W[i,k] ~ dbern(Y[i,k] * p)
        }
    }
    #p ~ dunif(0.001, 0.999)
    #phi ~ dunif(0.001, 0.999)
    p ~ dbeta(1, 1)
    phi ~ dbeta(0.5, 0.5)
}")
dat <- list(W = dcdim(data.matrix(w)), n = n, K = 1)
ini <- list(Y = dcdim(data.matrix(w)))
## need to clone the initial values
ifun <- function(model, n.clones) {
    dclone(list(Y = dcdim(data.matrix(w))),
        n.clones)
}
dcfit <- dc.fit(data = dat, params = c("p", "phi"),
    model = model, inits = ini,
    n.clones = c(1,2,4,8), unchanged = "n", multiply = "K",
    initsfun = ifun, n.iter = 10000)
summary(dcfit)
plot(dcfit)

dctable(dcfit)
plot(dctable(dcfit))

dcdiag(dcfit)
plot(dcdiag(dcfit))

plot(dcdiag(dcfit))
pairs(dcfit)

## GO BACK TO THE SLIDES! ----------------------------------------------

set.seed(1234)
n <- 50
T <- 5
p <- 0.6
phi <- 0.4
y <- rbinom(n = n, size = 1, prob = phi)
w <- matrix(NA, n, T)
for (t in 1:T)
    w[,t] <- rbinom(n = n, size = y, prob = p)

## setting up the grid for p and phi
grid <- expand.grid(p = seq(0, 1, by = 0.01),
    phi = seq(0, 1, by = 0.01),
    L = NA)
## the likelihood function
L_fun <- function(w, p, phi) {
    wdot <- rowSums(w)
    T <- ncol(w)
    prod(phi * (choose(T, wdot) * p^wdot * (1 - p)^(T - wdot)) +
        (1 - phi) * (wdot == 0))
}
## calculating the likelihood for the grid
for (i in 1:nrow(grid)) {
    grid$L[i] <- L_fun(w = w, p = grid$p[i], phi = grid$phi[i])
}
## plot the likelihood surface
dcpal_reds <- colorRampPalette(c("#f9f2f4", "#c7254e"))
L_mat <- matrix(grid$L, sqrt(nrow(grid)))
image(L_mat,
    xlab = "p", ylab = expression(varphi),
    col = dcpal_reds(12))
abline(h = phi, v = p)

open3d()
bg3d("white")
material3d(col = "black")
dcpal_grbu <- colorRampPalette(c("#18bc9c", "#3498db"))
Col <- rev(dcpal_grbu(12))[cut(L_mat, breaks = 12)]
persp3d(L_mat / max(L_mat), col = Col,
    theta=50, phi=25, expand=0.75, ticktype="detailed",
    ylab = "p", xlab = "phi", zlab = "L")



model <- custommodel("model {
    for (k in 1:K) {
    for (i in 1:n) {
        Y[i,k] ~ dbern(phi)
        for (t in 1:T) {
            W[i,t,k] ~ dbern(Y[i,k] * p)
        }
    }
    }
    p ~ dunif(0.001, 0.999)
    phi ~ dunif(0.001, 0.999)
}")
dat <- list(W = dcdim(array(w, c(n,T,1))), n = n, T = T, K = 1)
ini <- list(Y = data.matrix(rep(1, n)))
ifun <- function(model, n.clones) {
    list(Y = dclone(dcdim(data.matrix(rep(1, n))), n.clones))
}
dcfit <- dc.fit(data = dat, params = c("p", "phi"),
    model = model, inits = ini,
    n.clones = c(1,2,4,8), multiply = "K", unchanged = c("n","T"),
    initsfun = ifun)

summary(dcfit)
plot(dcfit)
dctable(dcfit)
plot(dctable(dcfit))
dcdiag(dcfit)
plot(dcdiag(dcfit))
pairs(dcfit)

## GO BACK TO THE SLIDES! ----------------------------------------------

# run chanis parallel

library(parallel)

model <- function() {
    for (i in 1:N) {
        Y[i] ~ dnorm(mu[i], tau)
        mu[i] <- alpha + beta * (x[i] - x.bar)
    }
    x.bar <- mean(x[])
    alpha ~ dnorm(0.0, 1.0E-4)
    beta ~ dnorm(0.0, 1.0E-4)
    sigma <- 1.0/sqrt(tau)
    tau ~ dgamma(1.0E-3, 1.0E-3)
}
set.seed(1234)
N <- 100
alpha <- 1
beta <- -1
sigma <- 0.5
x <- runif(N)
linpred <- crossprod(t(model.matrix(~x)), c(alpha, beta))
Y <- rnorm(N, mean = linpred, sd = sigma)
jdata <- list(N = N, Y = Y, x = x)
jpara <- c("alpha", "beta", "sigma")

## jags model on parallel workers
## n.chains must be <= no. of workers
cl <- makePSOCKcluster(4)
parJagsModel(cl, name="res", file=model, data=jdata,
    n.chains = 2, n.adapt=1000)
parUpdate(cl, "res", n.iter=1000)
m <- parCodaSamples(cl, "res", jpara, n.iter=2000)
stopifnot(2==nchain(m))
## with data cloning
dcdata <- dclone(list(N = N, Y = Y, x = x), 2, multiply="N")
parJagsModel(cl, name="res2", file=model, data=dcdata,
    n.chains = 2, n.adapt=1000)
parUpdate(cl, "res2", n.iter=1000)
m2 <- parCodaSamples(cl, "res2", jpara, n.iter=2000)
stopifnot(2==nchain(m2))
nclones(m2)
stopCluster(cl)

summary(m)
summary(m2)

## use the wrapper: jags.parfit
cl <- makePSOCKcluster(4)
tmp <- clusterEvalQ(cl, library(dclone))
parLoadModule(cl, "glm")
pm <- jags.parfit(cl, jdata, "beta", model)
summary(pm)
stopCluster(cl)

# Size balancing

K <- c(1, 2, 4, 8, 16)
ncl <- 2

opar <- par(mfrow=c(2, 2))
plotClusterSize(ncl, K, "none")
plotClusterSize(ncl, K, "load")
plotClusterSize(ncl, K, "size")
plotClusterSize(ncl, K, "both")
par(opar)
# watch the larges chunk!

# dc.parfit combines SB with parallel chains if needed


## GO BACK TO THE SLIDES! ----------------------------------------------
