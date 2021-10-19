# example of contrast analyses

# contrast function

# getting the level names out
level_names <- levels(dat$Annuality)

# lnRR
# meta-regression: mutiple intercepts
mr_annuality_lnRR1 <- rma.mv(
  yi = lnRR,
  V = VCV_lnRR,
  mods = ~ Annuality - 1,
  test = "t",
  random = list(~1|Species, ~1|Study, ~1|Effect),
  data = dat
)

# getting marginal R2
r2_mr_annuality_lnRR1 <- r2_ml(mr_annuality_lnRR1)

# meta-regression: contrasts 

contrast_fun <- function(data, moderator, VCV){
  
  mod <- deparse(substitute(moderator))
  
  # get names of different levels of a categorical variable 
  names <- levels(as.factor(data[[mod]]))
  moderator <- as.factor(data[[mod]])
  
  # function for running a meta-regression
  run_rma <- function(name) {
    rma.mv(yi = lnRR_Ea, 
           V = VCV, 
           mods = ~ relevel(moderator, ref = name),
           random = list(~1|Study_ID, ~1|ES_ID,~1|Strain),
           test = "t",
           data = data,
           sparse = TRUE)
  }
  
  # running all teh models to get all the contrasts
  contra <- map(names[-length(names)], run_rma)
  contra
}

# function to get relevant estimates

get_estimate <- function(contra) {
  
  # getting estimations 
  # something has changed here
  get_pred <- function (model) {
    name <- as.factor(str_replace(row.names(model$beta), 
                                  paste0("relevel", "\\(", "moderator, ref = name", "\\)"),
                                  ""))
    len <- length(name)
    
    if(len != 1){
      newdata <- diag(len)
      pred <- predict.rma(model, intercept = FALSE, newmods = newdata[,-1])
    }
    else {
      pred <- predict.rma(model)
    }
    est <- pred$pred
    lowerCL <- pred$ci.lb
    upperCL <- pred$ci.ub 
    lowerPR <- pred$cr.lb
    upperPR <- pred$cr.ub 
    
    table <- tibble(name = factor(name, levels = name, labels = name), estimate = est,
                    lowerCL = lowerCL, upperCL = upperCL,
                    pval = model$pval,
                    lowerPR = lowerPR, upperPR = upperPR)
  }
  
  # applying the estimation function for each model output
  estiamtes <- map(contra, ~ get_pred(.x))
  
  # a list of the numbers to take out unnecessary contrasts
  contra_list <- Map(seq, from=1, to =1:(length(names)-1))
  
  # you need to flatten twice: first to make it a list and make it a vector
  # I guess we can make it a loop (or vectorise)
  resutls <- map2_dfr(estiamtes, contra_list, ~.x[-(.y), ])   
  results
}



# test
# ran what's required to get the below code working

dat1 <- filter(dat, Type_assay %in% c("Recognition", "Habituation", "Conditioning"))
VCV_E1 <- impute_covariance_matrix(vi = dat1$lnRRV_E, cluster = dat$Study_ID, r = 0.5)

mod_E2 <- rma.mv(yi = lnRR_Ea, V = VCV_E1, mod = ~Type_assay-1, random = list(~1|Study_ID,
                                                                              ~1|ES_ID,
                                                                              ~1|Strain),
                 test = "t",
                 data = dat1)

summary(mod_E2)
r2_ml(mod_E2) 


mod_E2b <- rma.mv(yi = lnRR_Ea, V = VCV_E1, mod = ~Type_assay, random = list(~1|Study_ID,
                                                                              ~1|ES_ID,
                                                                              ~1|Strain),
                  test = "t",
                  data = dat1)


summary(mod_E2b)
