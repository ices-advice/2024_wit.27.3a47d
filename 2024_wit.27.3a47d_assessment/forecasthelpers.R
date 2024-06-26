## rounding function
rf <- if (require(icesAdvice, warn.conflicts=FALSE, quietly=TRUE)) {
  icesAdvice::icesRound
} else {
  function(x, ...) round(x, 2)
}

## Check if all equal
check_equal <- function(x) {
  if(mean(x) - x[1] != 0) warning("Not all values are the same")
  mean(x)
}

## Set assessment year
ay <- as.numeric(tail(rownames(stockassessment::fbartable(fit)), 1)) + 1
perc_change <- function(s,e) rf((e - s) / s * 100)

## Helper to make scenarios from a list of
make_scenarios <- function(s = seq(0.1, 0.6, by = 0.1)) {
  eval(parse(text =paste("list(", paste0('"F = ', s, '" = list(fval = c(Fsq,Fsq,', s,',', s,'))', collapse=",\n"), ")")))
}

## Squared difference of SSB at the end of the forecast to a target SSB
op <- function(Fm, target) {
  args <- c(forecastArgs, list(args = I(list(fval = c(Fsq,F_int,Fm,Fm))), nm = paste("F =", Fm)))
  res <- do.call(run_one_scenario, args)
  (attr(res, "tab")[4,7] - target)^2
}

## Squared difference of TAC at the end of the forecast to a target TAC
op_catch <- function(Fm, target) {
  args <- c(forecastArgs, list(args = I(list(fval = c(Fsq,Fm,Fmsy,Fmsy))), nm = paste("F =", Fm)))
  res <- do.call(run_one_scenario, args)
  (attr(res, "tab")[2,10] - target)^2
}

## Run one scenario
run_one_scenario <- function(args, nm="", ave.years, rec.years, splitLD = TRUE, nosim = 1001, savesim = FALSE) {
  set.seed(12345)
  ARGS <- args
  ARGS <- c(ARGS,
            list(fit = fit, ave.years=ave.years, rec.years=rec.years, label=nm, splitLD=splitLD, nosim = nosim, savesim = savesim))
  res <- do.call(forecast, ARGS)
  print(paste0("forecast : ", "'", nm, "'", " is complete"))
  res
}

getForecastTable <- function(fc, catchyr = 2019, ssbyr = 2021, advice, splitLD = FALSE) {
  Basis <- attr(fc, "label")
  tb <- attr(fc, "tab")
  tb <- tb[, grepl("median", colnames(tb))]

  ## Get relevant rows
  yrs <- as.numeric(rownames(tb))
  cr <- yrs == catchyr
  br <- yrs == ssbyr
  br_min1 <- yrs == (ssbyr - 1)
  br_plus1 <- yrs == (ssbyr + 1)
  ## Get relevant columns
  cc <- grepl("catch", colnames(tb))
  ftc <- grepl("fbar:", colnames(tb))
  fwc <- grepl("fbarL", colnames(tb))
  fuc <- grepl("fbarD", colnames(tb))
  bc <- grepl("ssb", colnames(tb))
  wc <- grepl("Land", colnames(tb))
  uc <- grepl("Disc", colnames(tb))

  ## Make data frame with the information needed
  res <- data.frame(Basis,
                    as.character(round(tb[cr, cc])),
                    as.character(round(tb[cr, wc])),
                    as.character(round(tb[cr, uc])),
                    as.character(rf(tb[cr, ftc])),
                    as.character(rf(tb[cr, fwc])),
                    as.character(rf(tb[cr, fuc])),
                    as.character(round(tb[br, bc])),
                    as.character(round(tb[br_plus1, bc])),
                    as.character(perc_change(tb[br_min1, bc], tb[br, bc])),
                    as.character(perc_change(advice, tb[cr, cc])),
                    stringsAsFactors = FALSE)
  names(res) <- c("Basis",
                  paste0("Total catch (", catchyr, ")"),
                  paste0("Projected landings (", catchyr, ")"),
                  paste0("Projected discards (", catchyr, ")"),
                  paste0("Ftotal ages 4-8 (", catchyr - 1, " & ", catchyr, ")"),
                  paste0("Flandings (", catchyr, ")"),
                  paste0("Fdiscards (", catchyr, ")"),
                  paste0("SSB (", ssbyr, ")"),
                  paste0("SSB (", ssbyr + 1, ")"),
                  "% SSB change",
                  "% Advice change"
  )
  if(splitLD) {
    res ##[, -c(6,7)]
  } else {
    res[, -c(3,4, 6,7)]
  }
}

makeAssumtpionTable <- function(intermyr, digits = 0, Ay, Ry) {
  ## ssblast <- round(s[nrow(s), startsWith(snm, "SSB")] / 1000, digits = digits)
  nms <- colnames(attr(FC[[1]], "tab"))
  recs <- sapply(FC, function(x) attr(x, "tab")[2:4, nms == "rec:median"])
  #rec <- check_equal(recs)
  rec <- exp(mean((attributes(FC[[5]])$fit$pl$logN[1, fit$data$years %in% Ry])))
  ssbs <- sapply(FC, function(x) attr(x, "tab")[1:4, nms == "ssb:median"])
  iy_row <- which(row.names(ssbs) == as.character(ay))
  ssblast <- check_equal(ssbs[iy_row, ])
  catches <- sapply(FC, function(x) attr(x, "tab")[1:4, nms == "catch:median"])
  catchlast <- check_equal(catches[iy_row, ]) ## tail(catchtable(fit), 1)[1]
  wcl <- check_equal(sapply(FC, function(x) attr(x, "tab")[iy_row, nms =="Land:median"]))
  ucl <- check_equal(sapply(FC, function(x) attr(x, "tab")[iy_row, nms =="Discard:median"]))
  rng <- function(x) paste0(range(x), collapse = "-")
  data.frame(
    Variable =c(paste0("F4-8(", intermyr, ")"),
                paste0("SSB(", intermyr, ")"),
                paste0("Rage1(", rng(intermyr:(intermyr + 2)), ")"),
                paste0("Total catch(", intermyr, ")"),
                paste0("Wanted catch(", intermyr, ")"),
                paste0("Unwanted catch(", intermyr, ")")
    ),
    Value =   c(rf(F_int),
                round(ssblast, digits = digits),
                round(rec, digits = digits),
                round(catchlast, digits = digits),
                round(wcl, digits = digits),
                round(ucl, digits = digits)
    ),
    Notes = c(
      paste0("Fsq = F4_8 (", intermyr - 1, ")."),
      "Tonnes; SSB at the middle of the year (i.e. spawning time) before the TAC year.",
      paste0("Resampled recruitment from the years ", rng(Ry), "(geometric mean shown); thousands."),
      "Tonnes; Short-term forecast.",
      paste0("Tonnes; Based on wanted catch fraction (average ", rng(Ay), ")."),
      paste0("Tonnes; Based on unwanted catch fraction (average ", rng(Ay), ").")
    )
  )
}
