taf.library(stockassessment)

set.seed(123)

## Last year in data
yr <- as.numeric(tail(rownames(fbartable(fit)), 1))
evalyr <- yr + 1

## Forecast settings ----
Ry <- 2008:max(fit$data$years) ## Recruitment since 2009
Ay <- max(fit$data$years) + (-2:0)  ## Biological vars years / landing fraction - last 3 years NOT SELECTIVITY
forecastArgs <- list(ave.years=Ay, rec.years=Ry, splitLD=TRUE, nosim=2001, savesim = TRUE)

## Last TAC advice ----
## TAC <- 2390000 # for 2018-2019
## TAC <- 1651000 # for 2020
## TAC <- 1733       # for 2021
## TAC <- 1206       # for 2022 (4287 t agreed TAC, combined with lemon sole)
## TAC <- 1313       # for 2023 (XXXX t agreed TAC, combined with lemon sole)
## TAC <- 1198       # for 2023 (Updaded in 2023 after finding an issue with the assessment and new reference points)
TAC_EU     <- 1481  #541 + 283 + 657
ICESadvice <- 1579

# Reference points ----
#Fmsy <- 0.154
#Flim <- 0.30
#Fpa <-  0.20
#Bpa <-  4745000
#Blim <- 3069000
#Btrigger <- 4745000
#Fmsyupper <- 0.21
#Fmsylower <- 0.108
#F05_AR <- 0.28 ## With Btrigger rule
#F05 <- 0.2 ## Without Btrigger rule
#Fpa <- min(F05, Fpa)
#Fmsyupper <- min(Fmsyupper, F05_AR)
#Fmsylower <- min(Fmsylower, F05_AR)

## IBPWitch 2021 reference points
#Fmsy <- 0.147
#Flim <- 0.32
#Fpa <-  0.28
#Bpa <-  4381
#Blim <- 3077
#Btrigger <- 4381
#Fmsyupper <-0.20
#Fmsylower <-0.105
#F05_AR <- 0.28 ## With Btrigger rule
#F05 <- 0.22 ## Without Btrigger rule

## Reference points after the fix
#Fmsy <- 0.161# 0.147
#Flim <- 0.309 # 0.32
#Fpa <-  0.241 # 0.28
#Bpa <-  4285 # 4381
#Blim <- 3362 # 3077
#Btrigger <- 4285 # 4381
#Fmsyupper <- 0.211 # 0.20
#Fmsylower <- 0.109 # 0.105
#F05_AR <- 0.241 # 0.28 ## With Btrigger rule
#F05 <- 0.208 # 0.22 ## Without Btrigger rule


## Reference points after the fix 1950-2021
#Fmsy <- 0.163# 0.147
#Flim <- 0.317 # 0.32
#Fpa <-  0.252 # 0.28
#Bpa <-  4409 # 4381
#Blim <- 3370 # 3077
#Btrigger <- 4409 # 4381
#Fmsyupper <- 0.217 # 0.20
#Fmsylower <- 0.112 # 0.105
#F05_AR <- 0.252 # 0.28 ## With Btrigger rule
#F05 <- 0.214 # 0.22 ## Without Btrigger rule

## Reference points after the fix 1950-2020
Fmsy <- 0.163# 0.147
Flim <- 0.33 # 0.32
Fpa <- F05_AR <- 0.245 # 0.28
Btrigger <- Bpa <-  4576 # 4381
Blim <- 3293 # 3077
Fmsyupper <- 0.222 # 0.20
Fmsylower <- 0.111 # 0.105
F05 <- 0.216 # 0.22 ## Without Btrigger rule

## Fs for the scenarios ----
## Get Fsq from the assessment summary table
s <- summary(fit)
snm <- colnames(s)
Fsq <- s[nrow(s), startsWith(snm, "Fbar")]

## Check if the biomass falls below MSY Btrigger while fishing at Fmsy
## Run short term forecast, fishing at Fmsy
args <- c(forecastArgs, list(args = I(list(fval = c(Fsq,Fsq,Fmsy,Fmsy))), nm = paste("F =", Fmsy)))
res <- do.call(run_one_scenario, args)

FmsyAR <- Fmsy
FmsyupperAR <- Fmsyupper
FmsylowerAR <- Fmsylower
BltBtrig <- FALSE

## Correct Fmsy (and upper and lower) with the advice rule, if B is below Btrigger
if (attr(res, "tab")[as.character(evalyr), "ssb:median"] < Btrigger) {
  BltBtrig <- TRUE
  Beval <- attr(res, "tab")[as.character(evalyr), "ssb:median"]
  FmsyAR <- Fmsy * min(Beval/Btrigger, 1)
  FmsyupperAR <- Fmsyupper * min(Beval/Btrigger, 1)
  FmsylowerAR <- Fmsylower * min(Beval/Btrigger, 1)
}

#args <- c(forecastArgs, list(args = I(list(fval = c(Fsq,Fsq,Fmsy,Fmsy))), nm = paste("F =", Fmsy)))
#res <- do.call(run_one_scenario, args)

# Find fishing mortalities 
tol <- 0.00005

# check if the intermediate year catch is higher than TAC_EU
F_int <- Fsq
if (TAC_EU < attr(res, "tab")[as.character(evalyr), "ssb:median"] ){
  F_tac <- optimise(op_catch, c(0, 1), target = TAC_EU, tol = tol)$minimum # find Fmort that leads to TAC_EU
  F_int <- F_tac}

# Find fishing mortality that leads to Blim, Bpa, Btrigger
F_Blim <- optimise(op, c(0, 1), target = Blim, tol = tol)$minimum
F_Btr <- F_Bpa <- optimise(op, c(0, 1), target = Bpa, tol = tol)$minimum
if (F_Btr != F_Bpa)
    F_Btr <- optimise(op, c(0, 1), target = Btrigger, tol = tol)$minimum

## Scenario definitions ---- w, x, y, z
            # values in the vectors are for: 
            # data year (WWWW), 
            # intermediate year (XXXX), 
            # advice year (YYYY), 
            # after advice year (ZZZZ)
if(BltBtrig){
  scen <- list(
    "MSY approach: FMSY x SSB (XXXX)/MSY Btrigger" = list(fval = c(Fsq,F_int,FmsyAR,FmsyAR)),
    "FMSY lower x SSB (XXXX)/MSY Btrigger" = list(fval = c(Fsq,F_int,FmsylowerAR,FmsylowerAR)),
    "F = 0"      = list(fval = c(Fsq,F_int,0.000001, 0.000001)),
    "Fpa"        = list( fval = c(Fsq,F_int,Fpa,Fpa)),
    "Flim"       = list(fval = c(Fsq,F_int,Flim,Flim)),
    "F = FXXXX"  = list(fval = c(Fsq,F_int,F_int,F_int)),
    "SSB (ZZZZ)  = Blim" = list(fval = c(Fsq,F_int,F_Blim,F_Blim)),
    "SSB (ZZZZ)  = Bpa = MSY Btrigger" = list(fval = c(Fsq,F_int,F_Bpa,F_Bpa)),
    "Rollover advice" = list(fval = c(Fsq,F_int,NA, NA), catchval = c(NA,NA,ICESadvice,ICESadvice)),
    "FMSY"       = list(fval = c(Fsq,F_int,Fmsy,Fmsy)),
    "FMSY lower" = list(fval = c(Fsq,F_int,Fmsylower,Fmsylower)))
} else {
  scen <- list(
    "MSY approach: FMSY" = list(fval = c(Fsq,F_int,Fmsy,Fmsy)),
    "F = 0"      = list(fval = c(Fsq,F_int,0.000001, 0.000001)),
    "Fpa"        = list( fval = c(Fsq,F_int,Fpa,Fpa)),
    "Flim"       = list(fval = c(Fsq,F_int,Flim,Flim)),
    "F = FXXXX"  = list(fval = c(Fsq,F_int,F_int,F_int)),
    "SSB (ZZZZ)  = Blim" = list(fval = c(Fsq,F_int,F_Blim,F_Blim)),
    "SSB (ZZZZ)  = Bpa = MSY Btrigger" = list(fval = c(Fsq,F_int,F_Bpa,F_Bpa)),
    "Rollover advice" = list(fval = c(Fsq,F_int,NA, NA), catchval = c(NA,NA,ICESadvice,ICESadvice)),
    "FMSY lower" = list(fval = c(Fsq,F_int,Fmsylower,Fmsylower)),
    "FMSY upper" = list(fval = c(Fsq,F_int,Fmsyupper,Fmsyupper)))
}

names(scen) <- sub("WWWW", yr, names(scen))
names(scen) <- sub("XXXX", yr + 1, names(scen))
names(scen) <- sub("YYYY", yr + 2, names(scen))
names(scen) <- sub("ZZZZ", yr + 3, names(scen))

FC <- setNames(mapply(run_one_scenario, scen, names(scen),
                      MoreArgs = forecastArgs,  SIMPLIFY = FALSE),
               names(scen))

## Make extra scenarios for a range of Fs
#scen_extra <- make_scenarios(s = seq(0.01, Flim, 0.01))
#FCextra <- setNames(mapply(run_one_scenario, scen_extra, names(scen_extra),
#                           MoreArgs = forecastArgs, SIMPLIFY = FALSE),
#               names(scen_extra))

save(FC, file="model/forecast.RData")
#save(FCextra, file = "run/forecast_extra.RData")

## Advice sheet tables -----
load("model/forecast.RData")
#load("run/forecast_extra.RData")
ft <- do.call(rbind.data.frame,lapply(FC, getForecastTable, catchyr = yr + 2, ssbyr = yr + 2, advice = ICESadvice, splitLD=TRUE))
#ftextra <- do.call(rbind.data.frame,lapply(FCextra, getForecastTable, catchyr = yr + 2, ssbyr = yr + 3, advice = TAC, splitLD=TRUE))

#write.table(ftextra, file = "res/wit.27.3a47d_extraFscen.csv", sep = ",", row.names = FALSE)
at <- makeAssumtpionTable(intermyr = ay, digits = 2, Ay = Ay, Ry = Ry)

save(ft, file = paste0("model/wit.27.3a47d_forecast_table_", yr + 1, ".RData"))
save(at, file = paste0("model/wit.27.3a47d_forecast_assumptions_table_", yr + 1, ".RData"))

