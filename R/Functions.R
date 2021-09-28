# CUSTOM FUNCTIONS

# Effect size (lnRR and SMD) for 2 main effects and interaction effect 
effect_set <- function(CC_n, CC_mean, CC_SD,
                       EC_n, EC_mean, EC_SD,
                       CS_n, CS_mean, CS_SD,
                       ES_n, ES_mean, ES_SD,
                       percent){
  
  if(percent == "no"){
  
  # lnRR----
  # main effect Environmental enrichment----
  lnRR_E <- log(0.5*(ES_mean + EC_mean)) - 
                         log(0.5*(CS_mean+ CC_mean))
  
  # TODO this needs to be double checked
  lnRRV_E <-  (1/(ES_mean + EC_mean))^2*(ES_SD^2 / ES_n + EC_SD^2 / EC_n) + 
    (1/(CS_mean + CC_mean))^2*(CS_SD^2 / CS_n + CC_SD^2 / CC_n)
  
  # main effect Stress
  lnRR_S <- log(0.5*(ES_mean + CS_mean)) - 
                         log(0.5*(EC_mean+ CC_mean))
  
  lnRRV_S <- lnRRV_E
  
  # interaction----
  
  lnRR_ES <-   (log(ES_mean) - log(CS_mean)) - 
                            (log(EC_mean) - log(CC_mean))
  
  
  lnRRV_ES <- 
    (((ES_SD)^2 / ((ES_mean)^2*ES_n)) + 
     ((EC_SD)^2 / ((EC_mean)^2*EC_n)) + 
      ((CS_SD)^2 / ((CS_mean)^2*CS_n)) +
       ((CC_SD)^2 / ((CC_mean)^2*CC_n)))
  
  # SMD
  SD_pool <- sqrt(((ES_n-1)*ES_SD^2 + 
                                (EC_n-1)*EC_SD^2 + 
                                (CS_n-1)*CS_SD^2 +
                                (CC_n-1)*CC_SD^2) / 
                               (ES_n + EC_n + CS_n + CC_n - 4))
  
  
  # main effect Environment enrichment
  SMD_E <- ((ES_mean + EC_mean) - (CS_mean + CC_mean))/ (2*SD_pool)
  
  
  SMDV_E <- 0.25*((1/ES_n + 1/EC_n + 1/CS_n + 1/CC_n) + 
                    (SMD_E^2 /2*(ES_n + EC_n + CS_n + CC_n)))
  
  
  
  # main effect Stress
  SMD_S <- ((ES_mean + CS_mean) - (EC_mean + CC_mean)) / (2*SD_pool)
  
  SMDV_S <- SMDV_E
  
  # interaction
  SMD_ES <- ((ES_mean - EC_mean) - (CS_mean - CC_mean)) / SD_pool
  
  SMDV_ES <- 1/ES_n + 1/EC_n + 1/CS_n + 1/CC_n + (SMD_ES^2 / (2*(ES_n + EC_n + CS_n + CC_n)))
  
  effect <- tibble(
    # lnRR
    lnRR_E = lnRR_E,
    lnRRV_E = lnRRV_E, 
    lnRR_S = lnRR_S, 
    lnRRV_S = lnRRV_S,
    lnRR_ES =lnRR_ES, 
    lnRRV_ES = lnRRV_ES,
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
  
  else {
    
    asin_trans <- function(percent) { asin(sqrt(percent/100)) }
    
    
    # transforming SD 
    ES_SD <- sqrt((ES_SD/100)^2/(4*(ES_mean/100)*(1-(ES_mean/100))))
    EC_SD <- sqrt((EC_SD/100)^2/(4*(EC_mean/100)*(1-(EC_mean/100))))
    CS_SD <- sqrt((CS_SD/100)^2/(4*(CS_mean/100)*(1-(CS_mean/100))))
    CC_SD <- sqrt((CC_SD/100)^2/(4*(CC_mean/100)*(1-(CC_mean/100))))
    
    # transformaing mean
    ES_mean <- asin_trans(ES_mean)
    EC_mean <- asin_trans(EC_mean)
    CS_mean <- asin_trans(CS_mean)
    CC_mean <- asin_trans(CC_mean)

    lnRR_E <- log(0.5*(ES_mean + EC_mean)) - 
                           log(0.5*(CS_mean+ CC_mean))
    
    # TODO this needs to be double checked
    lnRRV_E <-  (1/(ES_mean + EC_mean))^2*(ES_SD^2 / ES_n + EC_SD^2 / EC_n) +  
                             (1/(CS_mean + CC_mean))^2*(CS_SD^2 /CS_n + CC_SD^2 / CC_n) 
    
    # main effect Stress
    lnRR_S <- log(0.5*(ES_mean + CS_mean)) - 
                           log(0.5*(EC_mean+ CC_mean))
    
    lnRRV_S <- lnRRV_E
    
    # interaction----
    
    lnRR_ES <-   (log(ES_mean) - log(CS_mean)) - 
                              (log(EC_mean) - log(CC_mean))
    
    
    lnRRV_ES <- (((ES_SD)^2 / ((ES_mean)^2*ES_n)) + 
                    ((EC_SD)^2 / ((EC_mean)^2*EC_n)) + 
                    ((CS_SD)^2 / ((CS_mean)^2*CS_n)) +
                    ((CC_SD)^2 / ((CC_mean)^2*CC_n)))
    
    # SMD
    SD_pool <- sqrt(((ES_n-1)*ES_SD^2 + 
                    (EC_n-1)*EC_SD^2 + 
                    (CS_n-1)*CS_SD^2 +
                    (CC_n-1)*CC_SD^2) / 
                    (ES_n + EC_n + CS_n + CC_n - 4))
    
    
    # main effect Environment enrichment
    SMD_E <- as.numeric( ((ES_mean + EC_mean) - (CS_mean + CC_mean))/ (2*SD_pool))
    
    
    SMDV_E <- as.numeric(0.25*((1/ES_n + 1/EC_n + 1/CS_n + 1/CC_n) + (SMD_E^2 /2*(ES_n + EC_n + CS_n + CC_n))))
    
    # main effect Stress
    SMD_S <- as.numeric( ((ES_mean + CS_mean) - (EC_mean + CC_mean)) / (2*SD_pool))
    
    SMDV_S <- SMDV_E
    
    # interaction
    SMD_ES <- as.numeric( ((ES_mean - EC_mean) - (CS_mean - CC_mean)) / SD_pool)
    
    SMDV_ES <- as.numeric((1/ES_n + 1/EC_n + 1/CS_n + 1/CC_n + (SMD_E^2 / (2*(ES_n + EC_n + CS_n + CC_n)))))
    
    effect <- tibble(
      # lnRR
      lnRR_E = lnRR_E,
      lnRRV_E = lnRRV_E, 
      lnRR_S = lnRR_S, 
      lnRRV_S = lnRRV_S,
      lnRR_ES =lnRR_ES, 
      lnRRV_ES = lnRRV_ES,
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
  
}


# pairwise comparisons lnRR (not for SMD)

effect_set2 <- function(CC_n, CC_mean, CC_SD,
                        EC_n, EC_mean, EC_SD,
                        CS_n, CS_mean, CS_SD,
                        ES_n, ES_mean, ES_SD,
                        percent){
  
  if(percent == "no"){
  
  # EE vs control
  lnRR_E2 <- log(EC_mean) - log(CC_mean)
  
  
  lnRRV_E2 <-  (EC_SD^2 / (EC_mean^2*EC_n)) + 
                            (CC_SD^2 / (CC_mean^2*CC_n))
  
  
  # Stress vs control
  lnRR_S2 <- log(CS_mean) - log(CC_mean)
  
  lnRRV_S2 <- (CS_SD^2 / (CS_mean^2*CS_n)) + 
                           (CC_SD^2 / (CC_mean^2*CC_n))
  
  # EE + stress vs control
  lnRR_ES2 <- log(ES_mean) - log(CC_mean)
  
  lnRRV_ES2 <- (ES_SD^2 / (ES_mean^2*ES_n)) + 
                            (CC_SD^2 / (CC_mean^2*CC_n))
  
  # EE + stress vs stress (the effect of E in the presence of S)
  lnRR_E3 <- log(ES_mean) - log(CS_mean)
  
  lnRRV_E3 <- (ES_SD^2 / (ES_mean^2*ES_n)) + 
                           (CS_SD^2 / (CS_mean^2*CS_n))
  
  # EE + stress vs EE (the effect of S in the presence of E)
  lnRR_S3 <- log(ES_mean) - log(EC_mean)
  
  lnRRV_S3 <- (ES_SD^2 / (ES_mean^2*ES_n)) + 
                           (EC_SD^2 / (EC_mean^2*EC_n))
  
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
  else {
    asin_trans <- function(percent) { asin(sqrt(percent/100)) }
    
    # transforming SD 
    ES_SD <- sqrt((ES_SD/100)^2/(4*(ES_mean/100)*(1-(ES_mean/100))))
    EC_SD <- sqrt((EC_SD/100)^2/(4*(EC_mean/100)*(1-(EC_mean/100))))
    CS_SD <- sqrt((CS_SD/100)^2/(4*(CS_mean/100)*(1-(CS_mean/100))))
    CC_SD <- sqrt((CC_SD/100)^2/(4*(CC_mean/100)*(1-(CC_mean/100))))
    
    # transformaing mean
    ES_mean <- asin_trans(ES_mean)
    EC_mean <- asin_trans(EC_mean)
    CS_mean <- asin_trans(CS_mean)
    CC_mean <- asin_trans(CC_mean)
    
    # EE vs control
    lnRR_E2 <- log(EC_mean) - log(CC_mean)
    
    
    lnRRV_E2 <- (EC_SD^2 / (EC_mean^2*EC_n)) + 
                              (CC_SD^2 / (CC_mean^2*CC_n))
    
    # Stress vs control
    lnRR_S2 <- log(CS_mean) - log(CC_mean)
    
    lnRRV_S2 <- (CS_SD^2 / (CS_mean^2*CS_n)) + 
                             (CC_SD^2 / (CC_mean^2*CC_n))
    
    # EE + stress vs control
    lnRR_ES2 <- log(ES_mean) - log(CC_mean)
    
    lnRRV_ES2 <- (ES_SD^2 / (ES_mean^2*ES_n)) + 
                              (CC_SD^2 / (CC_mean^2*CC_n))
    
    # EE + stress vs stress (the effect of E in the presence of S)
    lnRR_E3 <-log(ES_mean) - log(CS_mean)
    
    lnRRV_E3 <- (ES_SD^2 / (ES_mean^2*ES_n)) + 
                             (CS_SD^2 / (CS_mean^2*CS_n))
    
    # EE + stress vs EE (the effect of S in the presence of E)
    lnRR_S3 <- log(ES_mean) - log(EC_mean)
    
    lnRRV_S3 <- (ES_SD^2 / (ES_mean^2*ES_n)) + 
                             (EC_SD^2 / (EC_mean^2*EC_n))
    
    
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
  
}

# making take from metafor models
estimates.CI <- function(model){
  db.mf <- data.frame(model$b,row.names = 1:nrow(model$b))
  db.mf <- cbind(db.mf,model$ci.lb,model$ci.ub,row.names(model$b))
  names(db.mf) <- c("mean","lower","upper","estimate")
  return(db.mf[,c("estimate","mean","lower","upper")])
}

