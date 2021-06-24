# custom functions

# TODO this function needs to be updated - including 2nd order adjustments

# lnRR, SMD and lnCVR

effect_set <- function(CC_n, CC_mean, CC_SD,
                       EC_n, EC_mean, EC_SD,
                       CS_n, CS_mean, CS_SD,
                       ES_n, ES_mean, ES_SD,
                       data){
  
  # lnRR
  # main effect Environment enrichment
  lnRR_E <- as.numeric(0.5 * log((data[[ES_mean]]*data[[EC_mean]]) / (data[[CS_mean]]*data[[CC_mean]]))) #checked and seems correct
  
  # the error has 1/4 
  lnRRV_E <-  as.numeric(0.25 * (((data[[ES_SD]])^2 / ((data[[ES_mean]])^2*data[[ES_n]])) + 
                                   ((data[[EC_SD]])^2 / ((data[[EC_mean]])^2*data[[EC_n]])) + 
                                   ((data[[CS_SD]])^2 / ((data[[CS_mean]])^2*data[[CS_n]])) +
                                   ((data[[CC_SD]])^2 / ((data[[CC_mean]])^2*data[[CC_n]]))))
  
  #testing 
  #(0.25 * (((3.29)^2 / ((13.57)^2*10)) + 
             #((7.43)^2 / ((15.83)^2*10)) + 
             #((4.11)^2 / ((20.09)^2*10)) +
             #((4.46)^2 / ((10.5)^2*10))))
  
  #(0.25 * ((10.48 / 1841.449) + 
             #(55.2049 / 2505.889) + 
             #(16.8921/ 4036.081) +
             #(19.8916 / 1102.5)))
  #checked and seems correct: number can vary after four decimals
  
# main effect Stress
  lnRR_S <- as.numeric( 0.5 * log((data[[ES_mean]]*data[[CS_mean]]) / (data[[EC_mean]]*data[[CC_mean]])))
  
  #testing
  
#( 0.5 * log((13.57*20.09) / (15.83*10.5)))
#( 0.5 * log((272.6213) / (166.215)))
# seems correct
  
  lnRRV_S <- lnRRV_E
  
  # interaction
  lnRR_ES <-   as.numeric((log(data[[ES_mean]]) - log(data[[CS_mean]])) - 
                           (log(data[[EC_mean]]) - log(data[[CC_mean]])))
  
  #testing
  #((log(13.57) - log(20.09)) - 
      #(log(15.83) - log(10.5)))
  
  #((2.607861 - 3.000222) - 
      #(2.761907 - 2.351375))
#seems correct  

  lnRRV_ES <- 
    as.numeric((((data[[ES_SD]])^2 / ((data[[ES_mean]])^2*data[[ES_n]])) + 
                  ((data[[EC_SD]])^2 / ((data[[EC_mean]])^2*data[[EC_n]])) + 
                  ((data[[CS_SD]])^2 / ((data[[CS_mean]])^2*data[[CS_n]])) +
                  ((data[[CC_SD]])^2 / ((data[[CC_mean]])^2*data[[CC_n]]))))
  
 #testing
  #((((3.29)^2 / ((13.57)^2*10)) + 
   #   ((7.43)^2 / ((15.83)^2*10)) + 
    #  ((4.11)^2 / ((20.09)^2*10)) +
     # ((4.46)^2 / ((10.5)^2*10))))
  
 # ((10.8241 / 1841.449) + 
  #    (55.2049 / 2505.889) + 
   #   (16.8921 / 4036.081) +
    #  (19.8916/ 1102.5))
  #seems correct
  
  # lnVR
  # main effect Environment enrichment
  
  #TODO I think there may be an issue with the forumla for lnVR_E: I can't get the formular to be consistent when entering actual values. The formula also differs from lnVR_S 
  
  lnVR_E <- as.numeric(0.5 * log((data[[ES_SD]]*data[[EC_SD]]) / (data[[CS_SD]]*data[[CC_SD]])) + 
                         0.5 * ( (1/(2*(data[[ES_n]] -1))) + (1/(2*(data[[EC_n]] -1))) -
                                   (1/(2*(data[[CS_n]] -1))) - (1/(2*(data[[CC_n]] -1)))))
  
  #testing
  
  #(0.5 * log((3.21*7.43) / (4.11*4.46)) + 
   #0.5 * ( (1/(2*(10 -1))) + (1/(2*(10 -1))) -
    #        (1/(2*(10 -1))) - (1/(2*(10 -1)))))
  
  
  #(0.5 * log((23.8503) / (18.3306)) + 
   # 0.5 * ( (1/18) + (1/18) -
    #          (1/18) - (1/18)))
  #seems correct
  #TODO - why are the addition and subtraction of the df reversed of lnVR_E and lnVR_S? See forumla used for lnVR_S below
  
  
  #(0.5 * log(62.6961 / 33.1378) + 
  #   0.5 * ( (1/18) - (1/18) +
  #            (1/18) - (1/18)))
  
  lnVRV_E <- as.numeric(0.25 * ( (1/(2*(data[[ES_n]] -1))) + (1/(2*(data[[EC_n]] -1))) +
                                    (1/(2*(data[[CS_n]] -1))) + (1/(2*(data[[CC_n]] -1))) ))
  
  #testing
 # (0.25 * ( (1/(2*(10 -1))) + (1/(2*(10 -1))) +
  #                                 (1/(2*(10 -1))) + (1/(2*(10 -1))) ))
  
  
  #(0.25 * ( (1/18) + (1/18) +
   #           (1/18) + (1/18)))
  # seems correct
  
  
  # main effect Stress
  lnVR_S <- as.numeric(0.5 * log((data[[ES_SD]]*data[[CS_SD]]) / (data[[EC_SD]]*data[[CC_SD]])) + 
                         0.5 * ( (1/(2*(data[[ES_n]] -1))) - (1/(2*(data[[EC_n]] -1))) +
                                   (1/(2*(data[[CS_n]] -1))) - (1/(2*(data[[CC_n]] -1)))))
  
  #testing
  
  #(0.5 * log((3.09*20.29) / (7.43*4.46)) + 
   #   0.5 * ( (1/(2*(10 -1))) - (1/(2*(10 -1))) +
    #            (1/(2*(10 -1))) - (1/(2*(10 -1)))))
  
  #(0.5 * log(62.6961 / 33.1378) + 
   #   0.5 * ( (1/18) - (1/18) +
    #            (1/18) - (1/18)))
  # seems correct
  
  
  lnVRV_S <- lnVRV_E
  
  # interaction
  lnVR_ES <- as.numeric( (log(data[[ES_SD]]) - log(data[[CS_SD]])) - 
                           (log(data[[EC_SD]]) - log(data[[CC_SD]])) 
                         + ( (1/(2*(data[[ES_n]] -1))) - (1/(2*(data[[EC_n]] -1))) -
                               (1/(2*(data[[CS_n]] -1))) + (1/(2*(data[[CC_n]] -1)))) )
  
  #testing 
#( (log(3.29) - log(4.11)) - 
 #                          (log(7.43) - log(4.46)) 
  #                       + ( (1/(2*10 -1))) - (1/(2*(10 -1))) -
   #                            (1/(2*(10 -1))) + (1/(2*(10 -1)))) 
  
  
  #( (-0.222535) - 
   #   (0.510377) 
    #+ ( (1/18) - (1/18) -
     # (1/18) + (1/18)))
  
#seems correct
  
  lnVRV_ES <- as.numeric(( (1/(2*(data[[ES_n]] -1))) + (1/(2*(data[[EC_n]] -1))) +
                 (1/(2*(data[[CS_n]] -1))) + (1/(2*(data[[CC_n]] -1))) ))
  
  #testing
  
#(( (1/(2*(10 -1))) + (1/(2*(10 -1))) +
 #                            (1/(2*(10 -1))) + (1/(2*(10 -1))) ))
  
  #(( (1/(18) + (1/18) +
   #    (1/18) + (1/18))))
  
  #seems correct
  
  # lnCVR
  CV_ES <- data[[ES_SD]]/data[[ES_mean]]
  CV_EC <- data[[EC_SD]]/data[[EC_mean]]
  CV_CS <- data[[CS_SD]]/data[[CS_mean]]
  CV_CC <- data[[CC_SD]]/data[[CC_mean]]
  
  #testing
  CV_ES <- 3.29/13.57
  CV_EC <- 7.43/15.83
  CV_CS <- 4.11/20.09
  CV_CC <- 4.46/10.5
  
  # main effect Environment enrichment
 # (0.5 * log((0.2424466*0.469362) / (0.2045794*0.4247619)) + 
  #                       0.5 * ( (1/(2*(10 -1))) + (1/(2*(10 -1))) -
   #                                (1/(2*(10 -1))) - (1/(2*(10 -1)))) )
  
  
  #(0.5 * log(0.1137952 / 0.08689753) + 
   #   0.5 * ( (1/18) + (1/18) -
    #            (1/18) - (1/18) ))
  #seems correct
  
  lnCVRV_E <-  lnRRV_E + lnVRV_E

  
  # main effect Stress
  lnCVR_S <- as.numeric(0.5 * log((CV_ES*CV_CS) / (CV_EC*CV_CC)) + 
                          0.5 * ( (1/(2*(data[[ES_n]] -1))) - (1/(2*(data[[EC_n]] -1))) +
                                    (1/(2*(data[[CS_n]] -1))) - (1/(2*(data[[CC_n]] -1)))) )
  
  #testing
  
  #(0.5 * log((0.2424466*0.2045794) / (0.469362*0.4247619)) + 
  #    0.5 * ( (1/(2*(10 -1))) - (1/(2*(10 -1))) +
   #             (1/(2*(10 -1))) - (1/(2*(10-1)))) )
  
  #(0.5 * log(0.04959958 / 0.1993671) + 
   #   0.5 * ( (1/18) - (1/18) +
    #            (1/18) - (1/18) ))

  #Seems correct
  
  lnCVRV_S <- lnCVRV_E
  
  # interaction
  
  lnCVR_ES <- as.numeric( (log(CV_ES) - log(CV_CS)) - (log(CV_EC) - log(CV_CC)) 
                         + ( (1/(2*(data[[ES_n]] -1))) - (1/(2*(data[[EC_n]] -1))) -
                               (1/(2*(data[[CS_n]] -1))) + (1/(2*(data[[CC_n]] -1)))) )
  #testing
  
  #( (log(0.2424466) - log(0.2045794)) - (log(0.469362) - log(0.4247619)) 
   # + ( (1/(2*(10 -1))) - (1/(2*(10 -1))) -
    #      (1/(2*(10 -1))) + (1/(2*(10 -1)))) )
  
  #( (0.1698253 - 0.09984555) 
   # + ( (1/18) - (1/18) -
    #      (1/18) + (1/18) ))
  #seems correct
  

  lnCVRV_ES <- lnRRV_ES + lnVRV_ES
  
  # SMD
  SD_pool <- sqrt(as.numeric(sqrt(((data[[ES_n]]-1)*data[[ES_SD]]^2 + 
                                (data[[EC_n]]-1)*data[[EC_SD]]^2 + 
                                (data[[CS_n]]-1)*data[[CS_SD]]^2 +
                                (data[[CC_n]]-1)*data[[CC_SD]]^2) / 
                               (data[[ES_n]] + data[[EC_n]] + data[[CS_n]] + data[[CC_n]] - 4))))
  
  #testing
  
 # sqrt((sqrt(((10-1)*3.21^2 + 
  #                        (10-1)*7.43^2 + 
   #                       (10-1)*4.11^2 +
    #                      (10-1)*4.46^2) / 
     #                    (10 + 10 + 10 + 10 - 4))))
  
  #sqrt((sqrt(9*10.3041 + 
   #                       9*55.2049 + 
    #                      9*16.8921 +
      #                   36))
  #I can't get the the same numbers
  #TODO this looks like this is being sqrt twice - check if this is correct
  
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
                   lnRR_S = lnRR_S, 
                   lnRRV_S = lnRRV_S, 
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
