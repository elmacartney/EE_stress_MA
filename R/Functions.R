# CUSTOM FUNCTIONS

# Effect size (lnRR and SMD) for 2 main effects and interaction effect ----
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
  
  lnRRV_E <-  (1/(ES_mean + EC_mean))^2*(ES_SD^2 / ES_n + EC_SD^2 / EC_n) + 
    (1/(CS_mean + CC_mean))^2*(CS_SD^2 / CS_n + CC_SD^2 / CC_n)
  
  # main effect Stress----
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
                    (SMD_E^2 /(2*(ES_n + EC_n + CS_n + CC_n))))
  
  
  
  # main effect Stress
  SMD_S <- ((ES_mean + CS_mean) - (EC_mean + CC_mean)) / (2*SD_pool)
  
  SMDV_S <- 0.25*((1/ES_n + 1/EC_n + 1/CS_n + 1/CC_n) + 
                    (SMD_S^2 /(2*(ES_n + EC_n + CS_n + CC_n))))
  
  # interaction
  SMD_ES <- ((ES_mean - EC_mean) - (CS_mean - CC_mean)) / SD_pool
  
  SMDV_ES <- (1/ES_n + 1/EC_n + 1/CS_n + 1/CC_n) + (SMD_ES^2 / (2*(ES_n + EC_n + CS_n + CC_n)))
  
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
    SMD_E <- ((ES_mean + EC_mean) - (CS_mean + CC_mean))/ (2*SD_pool)
    
    
    SMDV_E <- 0.25*((1/ES_n + 1/EC_n + 1/CS_n + 1/CC_n) + (SMD_E^2 /(2*(ES_n + EC_n + CS_n + CC_n))))
    
    # main effect Stress
    SMD_S <- ((ES_mean + CS_mean) - (EC_mean + CC_mean)) / (2*SD_pool)
    
    SMDV_S <- 0.25*((1/ES_n + 1/EC_n + 1/CS_n + 1/CC_n) + (SMD_S^2 /(2*(ES_n + EC_n + CS_n + CC_n))))
    
    # interaction
    SMD_ES <- ((ES_mean - EC_mean) - (CS_mean - CC_mean)) / SD_pool
    
    SMDV_ES <- (1/ES_n + 1/EC_n + 1/CS_n + 1/CC_n) + (SMD_ES^2 / (2*(ES_n + EC_n + CS_n + CC_n)))
    
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


# Removing asin_trans for sensitivity analysis----

effect_setb <- function(CC_n, CC_mean, CC_SD,
                        EC_n, EC_mean, EC_SD,
                        CS_n, CS_mean, CS_SD,
                        ES_n, ES_mean, ES_SD)
  {
    # lnRR----
    # main effect Environmental enrichment----
    lnRR_Eb <- log(0.5*(ES_mean + EC_mean)) - 
      log(0.5*(CS_mean+ CC_mean))
    
    lnRRV_Eb <-  (1/(ES_mean + EC_mean))^2*(ES_SD^2 / ES_n + EC_SD^2 / EC_n) + 
      (1/(CS_mean + CC_mean))^2*(CS_SD^2 / CS_n + CC_SD^2 / CC_n)
    
    # main effect Stress----
    lnRR_Sb <- log(0.5*(ES_mean + CS_mean)) - 
      log(0.5*(EC_mean+ CC_mean))
    
    lnRRV_Sb <- lnRRV_Eb
    
    # interaction----
    
    lnRR_ESb <-   (log(ES_mean) - log(CS_mean)) - 
      (log(EC_mean) - log(CC_mean))
    
    
    lnRRV_ESb <- 
      (((ES_SD)^2 / ((ES_mean)^2*ES_n)) + 
         ((EC_SD)^2 / ((EC_mean)^2*EC_n)) + 
         ((CS_SD)^2 / ((CS_mean)^2*CS_n)) +
         ((CC_SD)^2 / ((CC_mean)^2*CC_n)))

    effectb <- tibble(
      # lnRR
      lnRR_Eb = lnRR_Eb,
      lnRRV_Eb = lnRRV_Eb, 
      lnRR_Sb = lnRR_Sb, 
      lnRRV_Sb = lnRRV_Sb,
      lnRR_ESb =lnRR_ESb, 
      lnRRV_ESb = lnRRV_ESb
    )
    effectb
}


# Pairwise comparisons lnRR (not for SMD) -----

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

# pairwise comparisons
### Step 1 ####
# contrast function
# takes: 1) data frame, 2) moderator of interest, and 3) VCV matrix

contrast_fun <- function(data, response, moderator, VCV){
  
  mod <- deparse(substitute(moderator))
  resp <- deparse(substitute(response))
  
  # get names of different levels of a categorical variable 
  names <- levels(as.factor(data[[mod]]))
  moderator <- as.factor(data[[mod]])
  
  # function for running a meta-regression
  run_rma <- function(name) {
    rma.mv(yi = data[[resp]], 
           V = VCV, 
           mods = ~ relevel(moderator, ref = name),
           random = list(~1|Study_ID, ~1|ES_ID,~1|Strain),
           test = "t",
           data = data,
           sparse = TRUE)
  }
  
  
  # running all the models to get all the contrasts
  contra <- map(names[-length(names)], run_rma)
}

### Step 2 ####
# function to get relevant estimates from normal and contast models
# takes: 1) normal model output 2) contrast model outputs, and 3) moderator of interest, 
get_estimate <- function(model, contra, moderator) {
  
  # making into the name
  mod <- deparse(substitute(moderator))
  
  # getting estimations 
  get_pred1 <- function (model, mod =  mod) {
    name <- name <- firstup(as.character(stringr::str_replace(row.names(model$beta), mod, "")))
    len <- length(name)
    newdata <- matrix(NA, ncol = len, nrow = len)
    for (i in 1:len) {
      pos <- which(model$X[, i] == 1)[[1]]
      newdata[, i] <- model$X[pos, ]
    }
    pred <- metafor::predict.rma(model, newmods = newdata)
    
    estimate <- pred$pred
    lowerCL <- pred$ci.lb
    upperCL <- pred$ci.ub 
    lowerPR <- pred$cr.lb
    upperPR <- pred$cr.ub 
    
    table <- tibble(name = factor(name, levels = name, labels = name), estimate = estimate,
                    lowerCL = lowerCL, upperCL = upperCL,
                    pval = model$pval,
                    lowerPR = lowerPR, upperPR = upperPR)
    table
  }
  # something has changed here
  # get estiamtions from normal model
  get_pred2 <- function (model) {
    name <- as.factor(str_replace(row.names(model$beta), 
                                  paste0("relevel", "\\(", "moderator, ref = name", "\\)"),
                                  ""))
    len <- length(name)
    newdata <- diag(len)
    pred1 <- data.frame(predict.rma(model, newmods = newdata[1,-1]))
    pred2 <- data.frame(predict.rma(model, intercept = FALSE, newmods = newdata[-1,-1]))
    pred <- rbind(pred1, pred2)
    
    estimate <- pred$pred
    lowerCL <- pred$ci.lb
    upperCL <- pred$ci.ub 
    lowerPR <- pred$cr.lb
    upperPR <- pred$cr.ub 
    
    table <- tibble(name = factor(name, levels = name, labels = name), 
                    estimate = estimate,
                    lowerCL = lowerCL, 
                    upperCL = upperCL,
                    pval = model$pval,
                    lowerPR = lowerPR, upperPR = upperPR)
    table
  }
  
  # getting contrasts name
  cont_gen <- function(name) {
    combination <- combn(name, 2)
    name_dat <- t(combination)
    names <- paste(name_dat[, 1], name_dat[, 2], sep = "-")
    return(names)
  }
  
  # put into a proper tale
  mr_results <- function(res1, res2) {
    restuls <-tibble(
      Levels = c(as.character(res1$name), cont_gen(res1$name)),
      Estimate = c(res1$estimate, res2$estimate),
      Lower_CI = c(res1$lowerCL, res2$lowerCL),
      Upper_CI = c(res1$upperCL, res2$upperCL),
      P_value = c(res1$pval, res2$pval),
      Lower_PI = c(res1$lowerPR, res2$lowerPR),
      Upper_PI = c(res1$upperPR, res2$upperPR),
    )
    restuls
  }
  
  res1 <- get_pred1(model, mod = mod)
  
  # applying the estimation function for each model output
  estiamtes <- map(contra, ~ get_pred2(.x))
  
  # a list of the numbers to take out unnecessary contrasts
  contra_list <- Map(seq, from=1, to =1:length(contra))
  
  # you need to flatten twice: first to make it a list and make it a vector
  # I guess we can make it a loop (or vectorise)
  res2 <- map2_dfr(estiamtes, contra_list, ~.x[-(.y), ])   
  
  # putting two results together
  
  results <-  mr_results(res1, res2)
}

