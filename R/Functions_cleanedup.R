# CUSTOM FUNCTIONS
# lnRR and SMD 

effect_set <- function(CC_n, CC_mean, CC_SD,
                       EC_n, EC_mean, EC_SD,
                       CS_n, CS_mean, CS_SD,
                       ES_n, ES_mean, ES_SD,
                       data){
  
  # lnRR----
  # main effect Environmental enrichment----
  lnRR_E <- as.numeric(0.5 * log((data[[ES_mean]]*data[[EC_mean]]) / (data[[CS_mean]]*data[[CC_mean]])))
  
  
  lnRRV_E <-  as.numeric((1/(data[[ES_mean]] + data[[EC_mean]]))^2*(data[[ES_SD]]^2 / data[[ES_n]] + data[[EC_SD]]^2 / data[[EC_n]]) +  (1/(data[[CS_mean]] + data[[CC_mean]]))^2*(data[[CS_SD]]^2 / data[[CS_n]] + data[[CC_SD]]^2 / data[[CC_n]]))
  
  
  # main effect Stress
  lnRR_S <- as.numeric(log(0.5*(data[[ES_mean]] + data[[CS_mean]])) - 
                         log(0.5*(data[[EC_mean]]+ data[[CC_mean]])))
  
  lnRRV_S <- lnRRV_E
  
  # interaction----
  
  lnRR_ES <-   as.numeric((log(data[[ES_mean]]) - log(data[[CS_mean]])) - 
                            (log(data[[EC_mean]]) - log(data[[CC_mean]])))
  
  
  lnRRV_ES <- 
    as.numeric((((data[[ES_SD]])^2 / ((data[[ES_mean]])^2*data[[ES_n]])) + 
                  ((data[[EC_SD]])^2 / ((data[[EC_mean]])^2*data[[EC_n]])) + 
                  ((data[[CS_SD]])^2 / ((data[[CS_mean]])^2*data[[CS_n]])) +
                  ((data[[CC_SD]])^2 / ((data[[CC_mean]])^2*data[[CC_n]]))))
  
  

  # SMD
  SD_pool <- as.numeric(sqrt(((data[[ES_n]]-1)*data[[ES_SD]]^2 + 
                                (data[[EC_n]]-1)*data[[EC_SD]]^2 + 
                                (data[[CS_n]]-1)*data[[CS_SD]]^2 +
                                (data[[CC_n]]-1)*data[[CC_SD]]^2) / 
                               (data[[ES_n]] + data[[EC_n]] + data[[CS_n]] + data[[CC_n]] - 4)))
  
  
  # main effect Environment enrichment
  SMD_E <- as.numeric( ((data[[ES_mean]] + data[[EC_mean]]) - (data[[CS_mean]] + data[[CC_mean]]))/
                         (2*SD_pool))
  
  
  SMDV_E <- as.numeric(0.25*((1/data[[ES_n]] + 1/data[[EC_n]] + 1/data[[CS_n]] + 1/data[[CC_n]]
                              + SMD_E^2) / 
                               (2*(data[[ES_n]] + data[[EC_n]] + data[[CS_n]] + data[[CC_n]]))))
  
  
  
  # main effect Stress
  SMD_S <- as.numeric( ((data[[ES_mean]] + data[[CS_mean]]) - (data[[EC_mean]] + data[[CC_mean]])) /
                         (2*SD_pool))
  
  SMDV_S <- SMDV_E
  
  # interaction
  SMD_ES <- as.numeric( ((data[[ES_mean]] - data[[EC_mean]]) - (data[[CS_mean]] - data[[CC_mean]])) /
                          SD_pool)
  
  SMDV_ES <- as.numeric((1/data[[ES_n]] + 1/data[[EC_n]] + 1/data[[CS_n]] + 1/data[[CC_n]]
                         + SMD_E^2) / 
                          (2*(data[[ES_n]] + data[[EC_n]] + data[[CS_n]] + data[[CC_n]])))
  
  effect <- tibble(# lnRR
    lnRR_E = lnRR_E,
    lnRRV_E = lnRRV_E, 
    #lnRR_e = lnRR_e,
    #lnRRV_e = lnRRV_e,
    lnRR_S = lnRR_S, 
    lnRRV_S = lnRRV_S,
    #lnRR_s = lnRR_s, 
    #lnRRV_s = lnRRV_s, 
    lnRR_ES =lnRR_ES, 
    lnRRV_ES = lnRRV_ES,
    # lnVR
    #lnVR_E = lnVR_E,
    #lnVRV_E = lnVRV_E, 
    #lnVR_S = lnVR_S, 
    #lnVRV_S = lnVRV_S, 
    #lnVR_ES = lnVR_ES, 
    #lnVRV_ES = lnVRV_ES,
    # lnCVR
    #lnCVR_E = lnCVR_E,
    #lnCVRV_E = lnCVRV_E, 
    #lnCVR_S = lnCVR_S, 
    #lnCVRV_S = lnCVRV_S, 
    #lnCVR_ES = lnCVR_ES, 
    #lnCVRV_ES = lnCVRV_ES,
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


# pairwise comparisons lnRR----

effect_set2 <- function(CC_n, CC_mean, CC_SD,
                        EC_n, EC_mean, EC_SD,
                        CS_n, CS_mean, CS_SD,
                        ES_n, ES_mean, ES_SD,
                        data){
  
  # TODO - add some small sample adjustment (do this later)
  
  # EE vs control
  lnRR_E2 <- as.numeric(log(data[[EC_mean]]) - log(data[[CC_mean]]))
  
  
  lnRRV_E2 <-  as.numeric((data[[EC_SD]]^2 / (data[[EC_mean]]^2*data[[EC_n]])) + 
                            (data[[CC_SD]]^2 / data[[CC_mean]]^2*data[[CC_n]]))
  
  
  # Stress vs control
  lnRR_S2 <- as.numeric( log(data[[CS_mean]]) - log(data[[CC_mean]]))
  
  lnRRV_S2 <- as.numeric((data[[CS_SD]]^2 / (data[[CS_mean]]^2*data[[CS_n]])) + 
                           (data[[CC_SD]]^2 / data[[CC_mean]]^2*data[[CC_n]]))
  
  # EE + stress vs control
  lnRR_ES2 <- as.numeric( log(data[[ES_mean]]) - log(data[[CC_mean]]))
  
  lnRRV_ES2 <- as.numeric((data[[ES_SD]]^2 / (data[[ES_mean]]^2*data[[ES_n]])) + 
                            (data[[CC_SD]]^2 / data[[CC_mean]]^2*data[[CC_n]]))
  
  # EE + stress vs stress (the effect of E in the presence of S)
  lnRR_E3 <- as.numeric( log(data[[ES_mean]]) - log(data[[CS_mean]]))
  
  lnRRV_E3 <- as.numeric((data[[ES_SD]]^2 / (data[[ES_mean]]^2*data[[ES_n]])) + 
                           (data[[CS_SD]]^2 / data[[CS_mean]]^2*data[[CS_n]]))
  
  # EE + stress vs EE (the effect of S in the presence of E)
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
