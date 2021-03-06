# custom functions

# TODO this function needs to be updated - including 2nd order adjustments
# TODO get Shinichi to check variance VCV function

# lnRR, SMD and lnCVR

effect_set <- function(CC_n, CC_mean, CC_SD,
                       EC_n, EC_mean, EC_SD,
                       CS_n, CS_mean, CS_SD,
                       ES_n, ES_mean, ES_SD,
                       data){
  
  # lnRR
  # main effect Environment enrichment
  lnRR_E <- as.numeric(0.5 * log((data[[ES_mean]]*data[[EC_mean]]) / (data[[CS_mean]]*data[[CC_mean]])))
  

  lnRRV_E <-  as.numeric(0.25 * (((data[[ES_SD]])^2 / ((data[[ES_mean]])^2*data[[ES_n]])) + 
                                   ((data[[EC_SD]])^2 / ((data[[EC_mean]])^2*data[[EC_n]])) + 
                                   ((data[[CS_SD]])^2 / ((data[[CS_mean]])^2*data[[CS_n]])) +
                                   ((data[[CC_SD]])^2 / ((data[[CC_mean]])^2*data[[CC_n]]))))
  
  # alternative method
  lnRR_e <- as.numeric(log(0.5*(data[[ES_mean]] + data[[EC_mean]])) - 
                         log(0.5*(data[[CS_mean]]+ data[[CC_mean]])))
  
  
  lnRRV_e <-  as.numeric((1/(data[[ES_mean]] + data[[EC_mean]]))^2*(data[[ES_SD]]^2 / data[[ES_n]] + data[[EC_SD]]^2 / data[[EC_n]]) +  (1/(data[[CS_mean]] + data[[CC_mean]]))^2*(data[[CS_SD]]^2 / data[[CS_n]] + data[[CC_SD]]^2 / data[[CC_n]])
    )
  
# main effect Stress
  lnRR_S <- as.numeric( 0.5 * log((data[[ES_mean]]*data[[CS_mean]]) / (data[[EC_mean]]*data[[CC_mean]])))
  
  lnRRV_S <- lnRRV_E
  
  # alternative method
  lnRR_s <- as.numeric(log(0.5*(data[[ES_mean]] + data[[CS_mean]])) - 
                         log(0.5*(data[[EC_mean]]+ data[[CC_mean]])))
  lnRRV_s <- lnRRV_e
  
  # interaction
  lnRR_ES <-   as.numeric((log(data[[ES_mean]]) - log(data[[CS_mean]])) - 
                           (log(data[[EC_mean]]) - log(data[[CC_mean]])))
  


  lnRRV_ES <- 
    as.numeric((((data[[ES_SD]])^2 / ((data[[ES_mean]])^2*data[[ES_n]])) + 
                  ((data[[EC_SD]])^2 / ((data[[EC_mean]])^2*data[[EC_n]])) + 
                  ((data[[CS_SD]])^2 / ((data[[CS_mean]])^2*data[[CS_n]])) +
                  ((data[[CC_SD]])^2 / ((data[[CC_mean]])^2*data[[CC_n]]))))
  
  
# lnVR
  # main effect Environment enrichment
  lnVR_E <- as.numeric(0.5 * log((data[[ES_SD]]*data[[EC_SD]]) / (data[[CS_SD]]*data[[CC_SD]])) + 
                         0.5 * ( (1/(2*(data[[ES_n]] -1))) + (1/(2*(data[[EC_n]] -1))) -
                                   (1/(2*(data[[CS_n]] -1))) - (1/(2*(data[[CC_n]] -1)))))
  

  lnVRV_E <- as.numeric(0.25 * ( (1/(2*(data[[ES_n]] -1))) + (1/(2*(data[[EC_n]] -1))) +
                                    (1/(2*(data[[CS_n]] -1))) + (1/(2*(data[[CC_n]] -1))) ))
  
  
  
  # main effect Stress
  lnVR_S <- as.numeric(0.5 * log((data[[ES_SD]]*data[[CS_SD]]) / (data[[EC_SD]]*data[[CC_SD]])) + 
                         0.5 * ( (1/(2*(data[[ES_n]] -1))) - (1/(2*(data[[EC_n]] -1))) +
                                   (1/(2*(data[[CS_n]] -1))) - (1/(2*(data[[CC_n]] -1)))))

  
  
  lnVRV_S <- lnVRV_E
  
  # interaction
  lnVR_ES <- as.numeric( (log(data[[ES_SD]]) - log(data[[CS_SD]])) - 
                           (log(data[[EC_SD]]) - log(data[[CC_SD]])) 
                         + ( (1/(2*(data[[ES_n]] -1))) - (1/(2*(data[[EC_n]] -1))) -
                               (1/(2*(data[[CS_n]] -1))) + (1/(2*(data[[CC_n]] -1)))) )
  
  
  lnVRV_ES <- as.numeric(( (1/(2*(data[[ES_n]] -1))) + (1/(2*(data[[EC_n]] -1))) +
                 (1/(2*(data[[CS_n]] -1))) + (1/(2*(data[[CC_n]] -1))) ))
  

  
  # lnCVR
  CV_ES <- data[[ES_SD]]/data[[ES_mean]]
  CV_EC <- data[[EC_SD]]/data[[EC_mean]]
  CV_CS <- data[[CS_SD]]/data[[CS_mean]]
  CV_CC <- data[[CC_SD]]/data[[CC_mean]]
  

  
  # main effect Environment enrichment
  lnCVR_E <- as.numeric(0.5 * log((CV_ES*CV_EC) / (CV_CS*CV_CC)) + 
                          0.5 * ( (1/(2*(data[[ES_n]] -1))) - (1/(2*(data[[CS_n]] -1))) +
                                    (1/(2*(data[[EC_n]] -1))) - (1/(2*(data[[CC_n]] -1)))) )
  
  
  lnCVRV_E <-  lnRRV_E + lnVRV_E

  
  # main effect Stress
  lnCVR_S <- as.numeric(0.5 * log((CV_ES*CV_CS) / (CV_EC*CV_CC)) + 
                          0.5 * ( (1/(2*(data[[ES_n]] -1))) - (1/(2*(data[[EC_n]] -1))) +
                                    (1/(2*(data[[CS_n]] -1))) - (1/(2*(data[[CC_n]] -1)))) )
  
  lnCVRV_S <- lnCVRV_E
  
  # interaction
  
  lnCVR_ES <- as.numeric( (log(CV_ES) - log(CV_CS)) - (log(CV_EC) - log(CV_CC)) 
                         + ( (1/(2*(data[[ES_n]] -1))) - (1/(2*(data[[EC_n]] -1))) -
                               (1/(2*(data[[CS_n]] -1))) + (1/(2*(data[[CC_n]] -1)))) )

  
  lnCVRV_ES <- lnRRV_ES + lnVRV_ES
  
  # SMD
  SD_pool <- as.numeric(sqrt(((data[[ES_n]]-1)*data[[ES_SD]]^2 + 
                                (data[[EC_n]]-1)*data[[EC_SD]]^2 + 
                                (data[[CS_n]]-1)*data[[CS_SD]]^2 +
                                (data[[CC_n]]-1)*data[[CC_SD]]^2) / 
                               (data[[ES_n]] + data[[EC_n]] + data[[CS_n]] + data[[CC_n]] - 4)))
  
  
  # main effect Environment enrichment
  SMD_E <- as.numeric( ((data[[ES_mean]] + data[[EC_mean]]) - (data[[CS_mean]] + data[[CC_mean]]))/
                         (2*SD_pool))
  
  
  SMDV_E <- as.numeric(0.25*((1/data[[ES_n]] + 1/data[[EC_n]] + 1/data[[CS_n]] + 1/data[[CC_n]])
                                  + (SMD_E^2 / 
                                    2*(data[[ES_n]] + data[[EC_n]] + data[[CS_n]] + data[[CC_n]]))))
  

  
  # main effect Stress
  SMD_S <- as.numeric( ((data[[ES_mean]] + data[[CS_mean]]) - (data[[EC_mean]] + data[[CC_mean]])) /
                      (2*SD_pool))
  
  SMDV_S <- SMDV_E
  
  # interaction
  SMD_ES <- as.numeric( ((data[[ES_mean]] - data[[EC_mean]]) - (data[[CS_mean]] - data[[CC_mean]])) /
                          SD_pool)
  
  SMDV_ES <- as.numeric((1/data[[ES_n]] + 1/data[[EC_n]] + 1/data[[CS_n]] + 1/data[[CC_n]]
                              + (SMD_E^2 / 
                                (2*(data[[ES_n]] + data[[EC_n]] + data[[CS_n]] + data[[CC_n]])))))
  
  effect <- tibble(# lnRR
                   lnRR_E = lnRR_E,
                   lnRRV_E = lnRRV_E, 
                   lnRR_e = lnRR_e,
                   lnRRV_e = lnRRV_e,
                   lnRR_S = lnRR_S, 
                   lnRRV_S = lnRRV_S,
                   lnRR_s = lnRR_s, 
                   lnRRV_s = lnRRV_s, 
                   lnRR_ES =lnRR_ES, 
                   lnRRV_ES = lnRRV_ES,
                   # lnVR
                   lnVR_E = lnVR_E,
                   lnVRV_E = lnVRV_E, 
                   lnVR_S = lnVR_S, 
                   lnVRV_S = lnVRV_S, 
                   lnVR_ES = lnVR_ES, 
                   lnVRV_ES = lnVRV_ES,
                   # lnCVR
                   lnCVR_E = lnCVR_E,
                   lnCVRV_E = lnCVRV_E, 
                   lnCVR_S = lnCVR_S, 
                   lnCVRV_S = lnCVRV_S, 
                   lnCVR_ES = lnCVR_ES, 
                   lnCVRV_ES = lnCVRV_ES,
                   #SMD
                   SMD_E = SMD_E,
                   SMDV_E = SMDV_E, 
                   SMD_S = SMD_S, 
                   SMDV_S = SMDV_S, 
                   SMD_ES = SMD_ES, 
                   SMDV_ES = SMDV_ES
                   )
  effect
  
}


# comparing different conditions to CC

effect_set2 <- function(CC_n, CC_mean, CC_SD,
                       EC_n, EC_mean, EC_SD,
                       CS_n, CS_mean, CS_SD,
                       ES_n, ES_mean, ES_SD,
                       data){
  
  # lnRR
  # TODO - add some small sample adjustment (do this later)
  # EC vs CC
  lnRR_E2 <- as.numeric(log(data[[EC_mean]]) - log(data[[CC_mean]]))
  
  
  lnRRV_E2 <-  as.numeric((data[[EC_SD]]^2 / (data[[EC_mean]]^2*data[[EC_n]])) + 
                                   (data[[CC_SD]]^2 / data[[CC_mean]]^2*data[[CC_n]]))
  
  
  # CS vs CC
  lnRR_S2 <- as.numeric( log(data[[CS_mean]]) - log(data[[CC_mean]]))
  
  lnRRV_S2 <- as.numeric((data[[CS_SD]]^2 / (data[[CS_mean]]^2*data[[CS_n]])) + 
                          (data[[CC_SD]]^2 / data[[CC_mean]]^2*data[[CC_n]]))
  
  # ES vs CC
  lnRR_ES2 <- as.numeric( log(data[[ES_mean]]) - log(data[[CC_mean]]))
  
  lnRRV_ES2 <- as.numeric((data[[ES_SD]]^2 / (data[[ES_mean]]^2*data[[ES_n]])) + 
                           (data[[CC_SD]]^2 / data[[CC_mean]]^2*data[[CC_n]]))
  
  # ES vs CS (the effect of E in the presence of S)
  lnRR_E3 <- as.numeric( log(data[[ES_mean]]) - log(data[[CS_mean]]))
  
  lnRRV_E3 <- as.numeric((data[[ES_SD]]^2 / (data[[ES_mean]]^2*data[[ES_n]])) + 
                            (data[[CS_SD]]^2 / data[[CS_mean]]^2*data[[CS_n]]))
  
  # ES vs EC (the effect of S in the presence of E)
  lnRR_S3 <- as.numeric( log(data[[ES_mean]]) - log(data[[EC_mean]]))
  
  lnRRV_S3 <- as.numeric((data[[ES_SD]]^2 / (data[[ES_mean]]^2*data[[ES_n]])) + 
                           (data[[EC_SD]]^2 / data[[EC_mean]]^2*data[[EC_n]]))
  
  
  effect2 <- tibble(
    lnRR_E2 = lnRR_E2,
    lnRRV_E2 = lnRRV_E2, 
    lnRR_S2 = lnRR_S2, 
    lnRRV_S2 = lnRRV_S2, 
    lnRR_ES2 =lnRR_ES2, 
    lnRRV_ES2 = lnRRV_ES2,
    lnRR_E3 =lnRR_E3, 
    lnRRV_E3 = lnRRV_E3,
    lnRR_S3 = lnRR_S3,
    lnRRV_S3 = lnRRV_S3
  )
  effect2
  
}


#variance VCV

make_VCV_matrix <- function(data, V, cluster, obs, type=c("vcv", "cor"), rho=0.5){
  
  if (missing(data)) 
    stop("Must specify dataframe via 'data' argument.")
  if (missing(V)) 
    stop("Must specify name of the variance variable via 'V' argument.")
  if (missing(cluster)) 
    stop("Must specify name of the clustering variable via 'cluster' argument.")
  if (missing(obs)) 
    obs <- 1:length(V)   
  if (missing(type)) 
    type <- "vcv" 
  
  ## V = variance, cluster = animal group iD/study ID, ob = effect size id, type = default VCV settings
  
  new_matrix <- matrix(0,nrow = dim(data)[1],ncol = dim(data)[1]) #make empty matrix of the same size as data length
  rownames(new_matrix) <- data[ ,obs]
  colnames(new_matrix) <- data[ ,obs]
  # find start and end coordinates for the subsets
  shared_coord <- which(data[ ,cluster] %in% data[duplicated(data[ ,cluster]), cluster]==TRUE)
  # matrix of combinations of coordinates for each experiment with shared control
  combinations <- do.call("rbind", tapply(shared_coord, data[shared_coord,cluster], function(x) t(combn(x,2))))
  
  if(type == "vcv"){
    # calculate covariance values between  values at the positions in shared_list and place them on the matrix
    for (i in 1:dim(combinations)[1]){
      p1 <- combinations[i,1]
      p2 <- combinations[i,2]
      p1_p2_cov <- rho * sqrt(data[p1,V]) * sqrt(data[p2,V])
      new_matrix[p1,p2] <- p1_p2_cov
      new_matrix[p2,p1] <- p1_p2_cov
    }
    diag(new_matrix) <- data[ ,V]   #add the diagonal
  }
  
  if(type == "cor"){
    # calculate covariance values between  values at the positions in shared_list and place them on the matrix
    for (i in 1:dim(combinations)[1]){
      p1 <- combinations[i,1]
      p2 <- combinations[i,2]
      p1_p2_cov <- rho
      new_matrix[p1,p2] <- p1_p2_cov
      new_matrix[p2,p1] <- p1_p2_cov
    }
    diag(new_matrix) <- 1   #add the diagonal of 1
  }
  
  return(new_matrix)
}

estimates.CI <- function(model){
  db.mf <- data.frame(model$b,row.names = 1:nrow(model$b))
  db.mf <- cbind(db.mf,model$ci.lb,model$ci.ub,row.names(model$b))
  names(db.mf) <- c("mean","lower","upper","estimate")
  return(db.mf[,c("estimate","mean","lower","upper")])
}
# custom function for extracting mean and CI for emmeans (marginalized means)
estimates.CI2 <- function(res){
  db.mf <- data.frame(summary(res)[,2],row.names = 1:length( summary(res)[,2]))
  db.mf <- cbind(db.mf,summary(res)[,5],summary(res)[,6],paste0(names(res@levels),summary(res)[,1] ))
  names(db.mf) <- c("mean","lower","upper","estimate")
  return(db.mf[,c("estimate","mean","lower","upper")])
}
