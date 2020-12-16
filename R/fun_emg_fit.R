fitemgs <- function(signal, scantime = NULL, w = NULL, 
                         seed=list(mu=0,sigma=1,lambda=7),
                         lower=list(mu=-10,sigma=0,lambda=0.5),
                         upper=list(mu=10, sigma=2, lambda=20),
                         plot.fit=T)
{
  n <- unique(c(sapply(seed,length),sapply(lower,length),sapply(upper, length)))
  if(length(n)>1) stop("all elements in seed, lower, and upper should have the same length")
  if(any(c(sapply(seed, function(x) x<=0),sapply(lower,function(x) x<=0),sapply(upper, function(x) x<=0)))) {
    stop("At lest one seed, lower, or upper is negative or 0")
  }
  if (is.null(scantime)) scantime <- 1:length(signal)
  if (is.null(w)) w <- rep(1, length(signal))/length(signal)
  
  scantime.center <- median(scantime)
  area.estim <- sum((signal[-length(signal)]+diff(signal)/2)*diff(scantime))
  if(area.estim<0) stop("the estimated total area is lower than 0")
  x <- scantime-scantime.center
  y <- signal/area.estim
  
  seed$mu <- seed$mu-scantime.center
  lower$mu <- lower$mu-scantime.center
  upper$mu <- upper$mu-scantime.center
  
  areas.seed <- sapply(seed$mu, function(mu) y[which.min(abs(x-mu))])
  areas.seed <- areas.seed/sum(areas.seed)
  
  #pars[1:n] are centered mus
  #pars[(n+1):2n] are log(sigmas)
  #pars[(2n+1):3n] are log(lambdas)
  #pars[(3n+1):4n] are log(areas)
  
  ui.mu <- matrix(0,nrow=2*n,ncol=4*n)
  ui.mu[,1:n] <- rbind(diag(n),-diag(n))
  ci.mu <- c(lower$mu,-upper$mu)
  ui.sigma <- matrix(0,nrow=2*n,ncol=4*n)
  ui.sigma[,(n+1):(2*n)] <- rbind(diag(n),-diag(n))
  ci.sigma <- c(log(lower$sigma),-log(upper$sigma))
  ui.lambda <- matrix(0,nrow=2*n,ncol=4*n)
  ui.lambda[,(2*n+1):(3*n)] <- rbind(diag(n),-diag(n))
  ci.lambda <- c(log(lower$lambda),-log(upper$lambda))
  ui.areas <- matrix(0,nrow=n,ncol=4*n)
  ui.areas[,(3*n+1):(4*n)] <- -diag(n)
  ci.areas <- rep(-log(2/n),n)
  ui <- rbind(ui.mu,ui.sigma,ui.lambda,ui.areas)
  ci <- c(ci.mu,ci.sigma,ci.lambda,ci.areas)
  
  pars.optim <- constrOptim(theta=c(seed$mu,
                                    log(seed$sigma),
                                    log(seed$lambda),
                                    log(areas.seed)),
                            f=c_minfunc_2,
                            grad=c_mingrad,
                            ui=ui,
                            ci=ci,
                            method="BFGS",
                            x = x,
                            y = y,
                            w = w,
                            n = n)
  
  if(plot.fit)
  {
    pars.m <- matrix(pars.optim$par, nrow=n)
    pars.m[,2:4] <- exp(pars.m[,2:4])
    pred <- cemgsmat(pars.m, .x = x)
    plot(signal~scantime,xlab="scantime", ylab="signal")
    lines(area.estim*apply(pred,1,sum)~scantime)
    m <- lapply(1:n, function(i) lines(area.estim*pred[,i]~scantime,col=i+1))
  }
  
  data.frame(mu = pars.optim$par[1:n]+scantime.center,
             sigma = exp(pars.optim$par[(n+1):(2*n)]),
             lambda = exp(pars.optim$par[(2*n+1):(3*n)]),
             area = exp(pars.optim$par[(3*n+1):(4*n)])*area.estim,
             convergence.optim=pars.optim$convergence)
}