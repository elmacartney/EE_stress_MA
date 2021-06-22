# coustm functions

#' Title: helper functiuon for lnRR_set 
# we may need this a bit later
folded_error <-function(mean, variance){
  mu <- mean
  sigma <- sqrt(variance)
  fold_mu <- sigma*sqrt(2/pi)*exp((-mu^2)/(2*sigma^2)) + mu*(1 - 2*pnorm(-mu/sigma))
  fold_se <- sqrt(mu^2 + sigma^2 - fold_mu^2)
  # adding se to make bigger mean
  fold_v <-fold_se^2
  fold_v
} 

# lnRR, lnVR, and lnCVR

effect_set <- function(CC_n, CC_mean,CC_SD,
                       EC_n, EC_mean,EC_SD,
                       CS_n, CS_mean,CS_SD,
                       ES_n, ES_mean,ES_SD,
                       data = data){
  
  # main effect 1
  lnRR1 <- as.numeric((1/2) * log((data[,m1_1]*data[,m1_2]) / (data[,m2_1]*data[,m2_2])))
  alnRR1 <- abs(lnRR1)
  
  # the error has 1/4 
  lnRR1V <-  as.numeric((1/4) * (((data[,sd1_1])^2 / ((data[,m1_1])^2*data[,n1_1])) + 
                                   ((data[,sd1_2])^2 / ((data[,m1_2])^2*data[,n1_2])) + 
                                   ((data[,sd2_1])^2 / ((data[,m2_1])^2*data[,n2_2])) +
                                   ((data[,sd2_2])^2 / ((data[,m2_2])^2*data[,n2_2]))))
  alnRR1V <- folded_error(lnRR1, lnRR1V)
  
  # main effect 2
  lnRR2 <- as.numeric( 0.5 * log((data[,m1_1]*data[,m2_1]) / (data[,m1_2]*data[,m2_2])))
  alnRR2 <- abs(lnRR2)
  
  lnRR2V <- lnRR1V
  alnRR2V <-  alnRR1V 
  
  # interaction
  lnRR12 <-   as.numeric((log(data[,m1_1]) - log(data[,m2_1])) - 
                           (log(data[,m1_2]) - log(data[,m2_2])))
  alnRR12 <- abs(lnRR12)
  
  
  lnRR12V <- 
    as.numeric(((data[,sd1_1]^2)/(data[,n1_1]*(data[,m1_1]^2))) + 
                 ((data[,sd1_2]^2)/(data[,n1_2]*(data[,m1_2]^2))) + 
                 ((data[,sd2_1]^2)/(data[,n2_1]*(data[,m2_1]^2))) + 
                 ((data[,sd2_2]^2)/(data[,n2_2]*(data[,m2_2]^2))))
  alnRR12V <-folded_error(lnRR12, lnRR12V)
  
  # lnVR
  
  
  # lnCVR
  
  
  effect <- tibble(lnRR1 = lnRR1,
                   lnRR1V = lnRR1V, 
                   lnRR2 = lnRR2, 
                   lnRR2V = lnRR2V, 
                   lnRR12 =lnRR12, 
                   lnRR12V = lnRR12V,
                   alnRR1 = alnRR1,
                   alnRR1V = alnRR1V, 
                   alnRR2 = alnRR2, 
                   alnRR2V = alnRR2V, 
                   alnRR12 = alnRR12, 
                   alnRR12V = alnRR12V)
  effect
  
}


#' Title: getting effect sizes 
lnRR_set <- function(n1_1, m1_1, sd1_1,  n1_2, m1_2, sd1_2, n2_1, m2_1, sd2_1,n2_2, m2_2, sd2_2, data){
  
  # main effect 1
  lnRR1 <- as.numeric((1/2) * log((data[,m1_1]*data[,m1_2]) / (data[,m2_1]*data[,m2_2])))
  alnRR1 <- abs(lnRR1)
  
  # the error has 1/4 
  lnRR1V <-  as.numeric((1/4) * (((data[,sd1_1])^2 / ((data[,m1_1])^2*data[,n1_1])) + 
                                   ((data[,sd1_2])^2 / ((data[,m1_2])^2*data[,n1_2])) + 
                                   ((data[,sd2_1])^2 / ((data[,m2_1])^2*data[,n2_2])) +
                                   ((data[,sd2_2])^2 / ((data[,m2_2])^2*data[,n2_2]))))
  alnRR1V <- folded_error(lnRR1, lnRR1V)
  
  # main effect 2
  lnRR2 <- as.numeric( 0.5 * log((data[,m1_1]*data[,m2_1]) / (data[,m1_2]*data[,m2_2])))
  alnRR2 <- abs(lnRR2)
  
  lnRR2V <- lnRR1V
  alnRR2V <-  alnRR1V 
  
  # interaction
  lnRR12 <-   as.numeric((log(data[,m1_1]) - log(data[,m2_1])) - 
                           (log(data[,m1_2]) - log(data[,m2_2])))
  alnRR12 <- abs(lnRR12)
  
  
  lnRR12V <- 
    as.numeric(((data[,sd1_1]^2)/(data[,n1_1]*(data[,m1_1]^2))) + 
                 ((data[,sd1_2]^2)/(data[,n1_2]*(data[,m1_2]^2))) + 
                 ((data[,sd2_1]^2)/(data[,n2_1]*(data[,m2_1]^2))) + 
                 ((data[,sd2_2]^2)/(data[,n2_2]*(data[,m2_2]^2))))
  alnRR12V <-folded_error(lnRR12, lnRR12V)
  
  effect <- tibble(lnRR1 = lnRR1,
                   lnRR1V = lnRR1V, 
                   lnRR2 = lnRR2, 
                   lnRR2V = lnRR2V, 
                   lnRR12 =lnRR12, 
                   lnRR12V = lnRR12V,
                   alnRR1 = alnRR1,
                   alnRR1V = alnRR1V, 
                   alnRR2 = alnRR2, 
                   alnRR2V = alnRR2V, 
                   alnRR12 = alnRR12, 
                   alnRR12V = alnRR12V)
  effect
}



#' Title: getting variance of effects 
lnCVR_set <- function(n1_1, m1_1, sd1_1,  n1_2, m1_2, sd1_2, n2_1, m2_1, sd2_1,n2_2, m2_2, sd2_2, data){
  
  # main effect 1
  
  CV11 <- data[,sd1_1]/data[,m1_1]
  CV12 <- data[,sd1_2]/data[,m1_2]
  CV21 <- data[,sd2_1]/data[,m2_1]
  CV22 <- data[,sd2_2]/data[,m2_2]
  
  lnCVR1 <- as.numeric(0.5 * log((CV11*CV12) / (CV21*CV22)) + 
                         0.5 * ( (1/(2*(data[,n1_1] -1))) - (1/(2*(data[,n1_2] -1))) +
                                   (1/(2*(data[,n2_1] -1))) - (1/(2*(data[,n2_2] -1)))) )
  alnCVR1 <- abs(lnCVR1)
  
  
  lnCVR1V <-  as.numeric((1/4) * (((data[,sd1_1])^2 / ((data[,m1_1])^2*data[,n1_1])) +
                                    ((data[,sd1_2])^2 / ((data[,m1_2])^2*data[,n1_2])) +
                                    ((data[,sd2_1])^2 / ((data[,m2_1])^2*data[,n2_2])) +
                                    ((data[,sd2_2])^2 / ((data[,m2_2])^2*data[,n2_2])))) +
    as.numeric((1/4) * ( (1/(2*(data[,n1_1] -1))) + (1/(2*(data[,n1_2] -1))) +
                           (1/(2*(data[,n2_1] -1))) + (1/(2*(data[,n2_2] -1))) ))
  
  alnCVR1V <- folded_error(lnCVR1, lnCVR1V)
  
  # main effect 2
  lnCVR2 <- as.numeric(0.5 * log((CV11*CV21) / (CV12*CV22)) + 
                         0.5 * ( (1/(2*(data[,n1_1] -1))) - (1/(2*(data[,n2_1] -1))) +
                                   (1/(2*(data[,n1_2] -1))) - (1/(2*(data[,n2_2] -1)))) )
  alnCVR2 <- abs(lnCVR2)
  
  lnCVR2V <- lnCVR1V
  alnCVR2V <-  alnCVR1V 
  
  # interaction
  
  lnCVR12 <- as.numeric( (log(CV11) - log(CV21)) - (log(CV12) - log(CV22)) 
                         + ((1/(2*(data[,n1_1] -1))) - (1/(2*(data[,n1_2] -1))) -
                              (1/(2*(data[,n2_1] -1))) + (1/(2*(data[,n2_2] -1)))  ) )
  alnCVR12 <- abs(lnCVR12)
  
  
  lnCVR12V <-
    as.numeric(((data[,sd1_1]^2)/(data[,n1_1]*(data[,m1_1]^2))) +
                 ((data[,sd1_2]^2)/(data[,n1_2]*(data[,m1_2]^2))) +
                 ((data[,sd2_1]^2)/(data[,n2_1]*(data[,m2_1]^2))) +
                 ((data[,sd2_2]^2)/(data[,n2_2]*(data[,m2_2]^2)))) +
    as.numeric(( (1/(2*(data[,n1_1] -1))) + (1/(2*(data[,n1_2] -1))) +
                   (1/(2*(data[,n2_1] -1))) + (1/(2*(data[,n2_2] -1))) ))
  alnCVR12V <-folded_error(lnCVR12, lnCVR12V)
  
  effect <- tibble(lnCVR1 = lnCVR1,
                   lnCVR1V = lnCVR1V,
                   lnCVR2 = lnCVR2,
                   lnCVR2V = lnCVR2V,
                   lnCVR12 =lnCVR12,
                   lnCVR12V = lnCVR12V,
                   alnCVR1 = alnCVR1,
                   alnCVR1V = alnCVR1V, 
                   alnCVR2 = alnCVR2, 
                   alnCVR2V = alnCVR2V, 
                   alnCVR12 = alnCVR12, 
                   alnCVR12V = alnCVR12V)
  effect
}

#' Title: getting folded normal mean and variance for metafor

folded_set <-function(mean, tau, se){
  mu <- mean
  sigma <- tau
  error <- tau + se
  fold_mu <- sigma*sqrt(2/pi)*exp((-mu^2)/(2*sigma^2)) + mu*(1 - 2*pnorm(-mu/sigma))
  fold_sd <- sqrt(mu^2 + sigma^2 - fold_mu^2)
  # adding se to make bigger mean
  fold_mu_post <- error*sqrt(2/pi)*exp((-mu^2)/(2*error^2)) + mu*(1 - 2*pnorm(-mu/error))
  fold_se <- fold_mu_post - fold_mu
  folded_set<-c(fold_mu, fold_sd, fold_se)
  folded_set
} 

#' Title: getting prediction from models
get_pred <- function(model, mod = " ") {
  name <- as.factor(str_replace(row.names(model$beta), mod, ""))
  len <- length(name)
  
  if (len != 1) {
    newdata <- matrix(NA, ncol = len, nrow = len)
    for (i in 1:len) {
      # getting the position of unique case from X (design matrix)
      pos <- which(model$X[, i] == 1)[[1]]
      newdata[, i] <- model$X[pos, ]
    }
    pred <- predict.rma(model, newmods = newdata)
  } else {
    pred <- predict.rma(model)
  }
  lowerPR <- pred$cr.lb
  upperPR <- pred$cr.ub
  
  table <- tibble(name = name, lowerPR = lowerPR, upperPR = upperPR)
}
