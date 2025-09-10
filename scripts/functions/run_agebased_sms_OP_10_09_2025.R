###############################################################################
# Sandeel multi-area Operating Model — FUNCTION: run.agebased.sms.op()
# Save as: R/run_agebased_sms_OP_10_09_2025.R
###############################################################################

# (Local helper) Flexible parameter picker for scalar/vector/matrix inputs
.pick_param <- function(x, yr, space, nyear, nspace, name = "param") {
  if (is.null(x)) return(NULL)
  if (length(x) == 1) return(x)
  if (is.matrix(x) && all(dim(x) == c(nyear, nspace))) return(x[yr, space])
  if (length(x) == nyear) return(x[yr])
  if (length(x) == nspace) return(x[space])
  stop(sprintf("%s has incompatible shape.", name))
}

# ---- 7) run.agebased.sms.op() ----
#' Run the age-based operating model (OM) with age-0-only movement
run.agebased.sms.op <- function(df){
  
  nseason <- df$nseason
  nyear   <- length(df$years)
  year    <- df$years
  
  # Core inputs (ensure no NA)
  mat  <- df$mat;  mat[is.na(mat)]   <- 0
  M    <- df$M;    M[is.na(M)]       <- 0
  weca <- df$weca; weca[is.na(weca)] <- 0
  west <- df$west; west[is.na(west)] <- 0
  F0   <- df$F0;   F0[is.na(F0)]     <- 0
  
  nage   <- df$nage
  age    <- df$age
  nspace <- df$nspace
  maxage <- max(age)
  
  R0  <- df$R0
  SDR <- df$SDR
  
  # Equilibrium N0 (plus group)
  mage    <- max(age)
  agetmp  <- 0:(mage * 3)
  nagetmp <- length(agetmp)
  
  M0 <- rep(0, mage * 3 + 1)
  M0[1:nage] <- M[, 1, 1, 1] * nseason
  M0[nage:length(M0)] <- M[nage, 1, 1, 1] * nseason
  
  N0tmp <- rep(NA_real_, nagetmp)
  N0tmp[1:(nagetmp - 1)] <- R0 * exp(-agetmp[1:(nagetmp - 1)] * M0[1:(nagetmp - 1)])
  N0tmp[nagetmp]         <- R0 * exp(-M0[nagetmp] * agetmp[nagetmp]) / (1 - exp(-M0[nagetmp]))
  
  N0 <- rep(NA_real_, nage)
  N0[1:(nage - 1)] <- N0tmp[1:(nage - 1)]
  N0[nage]         <- sum(N0tmp[nage:nagetmp])
  
  # Containers ---------------------------------------------------------------
  year_1 <- c(year, max(year) + 1)
  
  SSB        <- matrix(NA_real_, nyear, nspace, dimnames = list(year = year, space = 1:nspace))
  SSB.all    <- array(NA_real_, dim = c(nyear, nspace, nseason),
                      dimnames = list(year = year, space = 1:nspace, season = 1:nseason))
  SSB.weight <- matrix(NA_real_, nyear, nspace, dimnames = list(year = year, space = 1:nspace))
  
  Catch        <- matrix(NA_real_, nyear, dimnames = list(year = year))
  Catch.age    <- matrix(NA_real_, nage, nyear, dimnames = list(age = age, year = year))
  CatchN       <- matrix(NA_real_, nyear, dimnames = list(year = year))
  CatchN.age   <- matrix(NA_real_, nage, nyear, dimnames = list(age = age, year = year))
  
  R.save       <- matrix(NA_real_, nyear, nspace, dimnames = list(year = year, space = 1:nspace))
  Fsel.save    <- array(NA_real_, dim = c(nage, nyear, nspace),
                        dimnames = list(age = age, year = year, space = 1:nspace))
  Fseason.save <- array(NA_real_, dim = c(nage, nyear, nspace, nseason),
                        dimnames = list(age = age, year = year, space = 1:nspace, season = 1:nseason))
  
  N.save.age      <- array(0,  dim = c(nage, nyear + 1, nspace, nseason),
                           dimnames = list(age = age, year = year_1, space = 1:nspace, season = 1:nseason))
  V.save          <- array(NA_real_, dim = c(nyear, nspace, nseason),
                           dimnames = list(year = year, space = 1:nspace, season = 1:nseason))
  E.save          <- array(NA_real_, dim = c(nyear, nspace, nseason),
                           dimnames = list(year = year, space = 1:nspace, season = 1:nseason))
  Catch.save.age  <- array(0,  dim = c(nage, nyear, nspace, nseason),
                           dimnames = list(age = age, year = year, space = 1:nspace, season = 1:nseason))
  CatchN.save.age <- array(0,  dim = c(nage, nyear, nspace, nseason),
                           dimnames = list(age = age, year = year, space = 1:nspace, season = 1:nseason))
  age_comps_OM    <- array(NA_real_, dim = c(nage, nyear, nspace, nseason),
                           dimnames = list(age = age, year = year, space = 1:nspace, season = 1:nseason))
  Z.save          <- array(NA_real_, dim = c(nage, nyear, nspace, nseason),
                           dimnames = list(age = age, year = year, space = 1:nspace, season = 1:nseason))
  
  # surveys
  Q <- df$Q
  survey <- array(NA_real_, dim = c(nage, nyear, df$nsurvey),
                  dimnames = list(age = age, year = year, survey = 1:df$nsurvey))
  survey.true <- array(NA_real_, dim = c(nage, nyear, nspace, df$nsurvey),
                       dimnames = list(age = age, year = year, space = 1:nspace, survey = 1:df$nsurvey))
  
  # catch age comps per space and overall
  age_comps_catch_space <- array(NA_real_, dim = c(maxage, nyear, nspace),
                                 dimnames = list(age = 1:maxage, year = year, space = 1:nspace))
  age_comps_catch <- array(NA_real_, dim = c(maxage, nyear),
                           dimnames = list(age = 1:maxage, year = year))
  
  # Initial distribution over space
  Ninit <- if (is.null(df$Ninit)) N0 else df$Ninit
  for (sp in 1:nspace) N.save.age[, 1, sp, 1] <- Ninit / nspace
  
  # Main loop ----------------------------------------------------------------
  for (yr in 1:nyear) {
    
    # (A) Age-0 routing BEFORE recruitment (handles any existing age-0 at season 1)
    if (!is.null(df$move_age0)) {
      a0_from <- N.save.age[1, yr, , 1]
      a0_to   <- df$move_age0 %*% a0_from
      N.save.age[1, yr, , 1] <- as.numeric(a0_to)
    }
    
    w_catch  <- df$weca[, yr, , , drop = FALSE]; w_catch[is.na(w_catch)] <- 0
    w_ssb    <- df$west[, yr, , , drop = FALSE]; w_ssb[is.na(w_ssb)]     <- 0
    sel      <- df$Fsel[, yr, , , drop = FALSE]; sel[is.na(sel)]         <- 0
    Myear    <- M[, yr, , , drop = FALSE]
    mat.year <- mat[, yr, , , drop = FALSE]
    
    for (season in 1:nseason) {
      for (space in 1:nspace) {
        
        # Spawning biomass (season 1 snapshot)
        if (season == 1) {
          SSB.weight[yr, space] <- sum(N.save.age[, yr, space, 1] *
                                         w_ssb[ , 1, space, season] *
                                         mat.year[, 1, space, season], na.rm = TRUE)
          SSB[yr, space] <- SSB.weight[yr, space]
        }
        
        # (B) RECRUITMENT at df$rseason
        if (season == df$rseason) {
          
          if (df$recruitment == 'Ricker') {
            alpha  <- .pick_param(df$alpha,  yr, space, nyear, nspace, "alpha")
            betaSR <- .pick_param(df$betaSR, yr, space, nyear, nspace, "betaSR")
            if (is.null(alpha) || is.null(betaSR))
              stop("Ricker needs df$alpha and df$betaSR (scalar, vector, or nyear×nspace).")
            R <- alpha * SSB[yr, space] * exp(-betaSR * SSB[yr, space])
          }
          
          if (df$recruitment == 'hockey') {
            alpha  <- if (is.matrix(df$alpha)  && all(dim(df$alpha)  == c(nyear, nspace))) df$alpha[yr, space]  else df$alpha
            betaSR <- if (is.matrix(df$betaSR) && all(dim(df$betaSR) == c(nyear, nspace))) df$betaSR[yr, space] else df$betaSR
            Rdet <- alpha + log(SSB[yr, space])
            if (SSB[yr, space] > betaSR) Rdet <- alpha + log(betaSR)
            Ry    <- rnorm(1, 0, df$SDR)
            R.err <- exp(-0.5 * df$b[yr] * df$SDR^2 + Ry)
            R <- exp(Rdet) * R.err
          }
          
          if (df$recruitment == 'estimated') {
            # Historical replay (pre-projection years)
            R <- df$Rin[yr] * df$rec.space[space]
            
            # Optional: in projection years (> last_year) use a model of logR vs SSB if present
            if (df$years[yr] > df$last_year) {
              mod <- if (!is.null(df$mod_by_year)) df$mod_by_year[[space]][[yr]] else df$mod[[space]]
              logR_ssb <- predict(mod, newdata = data.frame(SSB = SSB[yr, space]))
              R <- exp(as.numeric(logR_ssb[1])) * SSB[yr, space]
            }
          }
          
          N.save.age[1, yr, space, season] <- R
          R.save[yr, space] <- R
          
          # (C) Age-0 routing RIGHT AFTER recruitment
          if (!is.null(df$move_age0)) {
            n0_now <- N.save.age[1, yr, , season]
            n0_new <- as.numeric(df$move_age0 %*% n0_now)
            N.save.age[1, yr, , season] <- n0_new
          }
        }
        
        # (D) Mortality and catch for this step
        if (df$Fmodel == "est") {
          Fseason <- F0[, yr, space, season]                 # use estimated F directly
        } else {
          Fseason <- df$Fin[yr, season] * sel[, 1, space, season]  # Fin × selectivity shape
        }
        Mseason <- Myear[, 1, space, season]
        Z <- Mseason + Fseason
        if (Z[1] == 0) Z[1] <- Z[2]  # guard for age-0 edge cases
        
        Z.save[, yr, space, season]       <- Z
        Fseason.save[, yr, space, season] <- Fseason
        Fsel.save[, yr, space]            <- sel[, 1, space, season]
        
        if (season < nseason) {
          # Survivors to next season
          N.save.age[, yr, space, season + 1] <-
            N.save.age[, yr, space, season] * exp(-Z)
          
          # bookkeeping
          age_comps_OM[, yr, space, season] <-
            N.save.age[, yr, space, season] / sum(N.save.age[, yr, space, season])
          
          SSB.all[yr, space, season] <-
            sum(N.save.age[, yr, space, season] * mat.year[, 1, space, season], na.rm = TRUE)
          
          V.save[yr, space, season] <-
            sum(N.save.age[, yr, space, season] * sel[, 1, space, season] * w_catch[, 1, space, season])
          
          if (max(Fseason) > 0) {
            Zcatch <- Z; Zcatch[1] <- Zcatch[2]
            Catch.save.age[, yr, space, season] <-
              (Fseason / Zcatch) * (1 - exp(-Zcatch)) * N.save.age[, yr, space, season] * w_catch[, 1, space, season]
            CatchN.save.age[, yr, space, season] <-
              (Fseason / Zcatch) * (1 - exp(-Zcatch)) * N.save.age[, yr, space, season]
            E.save[yr, space, season] <- sum(Catch.save.age[, yr, space, season]) / V.save[yr, space, season]
          } else {
            E.save[yr, space, season] <- 0
          }
          
        } else {
          # End of year: ages advance to next year's season 1 (plus-group applied)
          if (df$nage >= 2) {
            idx_from <- 1:(df$nage - 1)
            idx_to   <- 2:(df$nage)
            N.save.age[idx_to, yr + 1, space, 1] <-
              N.save.age[idx_from, yr, space, nseason] *
              exp(-Z.save[idx_from, yr, space, nseason])
          }
          if (df$nage >= 2) {
            N.save.age[df$nage, yr + 1, space, 1] <-
              ( N.save.age[df$nage - 1, yr, space, nseason] * exp(-Z.save[df$nage - 1, yr, space, nseason]) ) +
              ( N.save.age[df$nage,     yr, space, nseason] * exp(-Z.save[df$nage,     yr, space, nseason]) )
          }
          
          # bookkeeping
          age_comps_OM[, yr, space, season] <-
            N.save.age[, yr, space, season] / sum(N.save.age[, yr, space, season])
          
          SSB.all[yr, space, season] <-
            sum(N.save.age[, yr, space, season] * mat.year[, 1, space, season], na.rm = TRUE)
          
          V.save[yr, space, season] <-
            sum(N.save.age[, yr, space, season] * sel[, 1, space, season] * w_catch[, 1, space, season])
          
          if (max(Fseason) > 0) {
            Zcatch <- Z; Zcatch[1] <- Zcatch[2]
            Catch.save.age[, yr, space, season] <-
              (Fseason / Zcatch) * (1 - exp(-Zcatch)) * N.save.age[, yr, space, season] * w_catch[, 1, space, season]
            CatchN.save.age[, yr, space, season] <-
              (Fseason / Zcatch) * (1 - exp(-Zcatch)) * N.save.age[, yr, space, season]
            E.save[yr, space, season] <- sum(Catch.save.age[, yr, space, season]) / V.save[yr, space, season]
          } else {
            E.save[yr, space, season] <- 0
          }
        }
        
      } # space
    }   # season
    
    # Aggregate catch for the year
    if (nseason > 1) {
      Catch.age[, yr]  <- apply(Catch.save.age[, yr, , ], 1, sum)
      Catch[yr]        <- sum(Catch.save.age[, yr, , ])
      CatchN.age[, yr] <- apply(CatchN.save.age[, yr, , ], 1, sum)
      CatchN[yr]       <- sum(CatchN.save.age[, yr, , ])
    } else {
      if (nspace == 1) {
        Catch.age[, yr]  <- Catch.save.age[, yr, , ]
        Catch[yr]        <- sum(Catch.save.age[, yr, , ])
        CatchN.age[, yr] <- CatchN.save.age[, yr, , ]
        CatchN[yr]       <- sum(CatchN.save.age[, yr, , ])
      } else {
        Catch.age[, yr]  <- rowSums(Catch.save.age[, yr, , ])
        Catch[yr]        <- sum(Catch.save.age[, yr, , ])
        CatchN.age[, yr] <- rowSums(CatchN.save.age[, yr, , ])
        CatchN[yr]       <- sum(CatchN.save.age[, yr, , ])
      }
    }
    
    # Surveys (per survey, per space) then aggregate across space
    for (surv in 1:df$nsurvey) {
      for (space in 1:nspace) {
        if (df$surveyEnd[surv] == 0) {
          # Point-in-time style survey at surveySeason
          survey.true[, yr, space, surv] <-
            N.save.age[, yr, space, df$surveySeason[surv]] *
            exp(-Z.save[, yr, space, df$surveySeason[surv]]) *
            Q[, surv] * exp(rnorm(nage, mean = 0, sd = df$surveySD))
        } else {
          # Interval-style survey: average over [surveyStart, surveyEnd] within the survey season
          Ntmp.s <- N.save.age[, yr, space, df$surveySeason[surv]] *
            exp(-Z.save[, yr, space, df$surveySeason[surv]] * df$surveyStart[surv])
          survey.true[, yr, space, surv] <-
            Ntmp.s * (1 - exp(-Z.save[, yr, space, df$surveySeason[surv]] *
                                (df$surveyEnd[surv] - df$surveyStart[surv]))) /
            (Z.save[, yr, space, df$surveySeason[surv]] *
               (df$surveyEnd[surv] - df$surveyStart[surv])) *
            Q[, surv] * exp(rnorm(nage, mean = 0, sd = df$surveySD))
        }
      }
      survey[, , surv] <- apply(survey.true[, , , surv, drop = FALSE], c(1, 2), sum)
    }
    survey[survey == 0] <- -1  # TMB sentinel
    
    # Catch age compositions (space & overall)
    for (space in 1:nspace) {
      if (nseason > 1) {
        Catch.tmp <- rowSums(CatchN.save.age[, yr, space, ])
      } else {
        Catch.tmp <- CatchN.save.age[, yr, space, ]
      }
      Catch.tot <- sum(CatchN.save.age[, yr, space, ])
      if (Catch.tot > 0) {
        age_comps_catch_space[1:(maxage - 1), yr, space] <- Catch.tmp[2:maxage] / Catch.tot
        age_comps_catch_space[maxage, yr, space]         <- sum(Catch.tmp[(maxage + 1):nage]) / Catch.tot
      } else {
        age_comps_catch_space[, yr, space] <- 0
      }
    }
    CatchN_tot_age <- apply(CatchN.save.age[, yr, , , drop = FALSE], 1, sum)
    CatchN_tot     <- sum(CatchN_tot_age)
    if (CatchN_tot > 0) {
      age_comps_catch[1:(maxage - 1), yr] <- CatchN_tot_age[2:maxage] / CatchN_tot
      age_comps_catch[maxage, yr]         <- sum(CatchN_tot_age[(maxage + 1):nage]) / CatchN_tot
    } else {
      age_comps_catch[, yr] <- 0
    }
    
  } # yr
  
  # Output roll-ups ----------------------------------------------------------
  if (isFALSE(df$move)) {
    Nsave    <- N.save.age[, , , nseason, drop = FALSE] # by space (no adult movement)
    SSB.save <- SSB
  } else {
    Nsave    <- apply(N.save.age[, , , 1, drop = FALSE], c(1, 2), sum) # summed across spaces
    SSB.save <- rowSums(SSB)
  }
  
  # Fbar over requested ages (inclusive), averaged over space & season
  Fbar <- rep(0, nyear)
  for (t in 1:nyear) {
    Fbar[t] <- sum(Fseason.save[(df$Fbarage[1] + 1):(df$Fbarage[2] + 1), t, , ]) /
      (df$Fbarage[2] - df$Fbarage[1] + 1)
  }
  
  list(
    N.save          = Nsave,
    SSB             = SSB,
    N.save.age      = N.save.age,
    R.save          = R.save,
    V.save          = V.save,
    E.save          = E.save,
    SSB.all         = SSB.all,
    Catch.save.age  = Catch.save.age,
    CatchN.save.age = CatchN.save.age,
    Catch           = Catch,
    Catch.age       = Catch.age,
    survey          = survey,
    Fbar            = Fbar,
    age_comps_OM    = age_comps_OM,
    age_catch       = age_comps_catch,
    Z               = Z.save,
    Fseason         = Fseason.save,
    Fsel            = Fsel.save
  )
}

# ---- NOTES: run.agebased.sms.op() ----
# TITLE: How the core dynamics work (with multipliers & recruitment)
# - Mortality/Catch multipliers:
#   * Z = M + Fseason; Fseason is either the estimated F0 (per age/season) or Fin × selectivity.
#   * Catch-at-age: Baranov's equation (F/Z) * (1 - exp(-Z)) * N * weight.
#   * Exploitation proxy E = Catch / Vulnerable biomass (V), with V = sum(N × sel × catch weight).
# - Recruitment modes:
#   * 'estimated': R = Rin[year] × rec.space[space] (hindcast). For years > last_year, optional
#     projection model logR ~ f(SSB) via df$mod or df$mod_by_year: R = exp(pred) × SSB.
#   * 'Ricker': R = α × SSB × exp(-β × SSB).
#   * 'hockey': logR = α + log(min(SSB, β)) + noise, noise ~ N(-0.5*b*SDR^2, SDR^2), with b a multiplier.
# - Movement:
#   * Adults: none (movemat all zeros).
#   * Larvae (age-0): routed with df$move_age0 both before and after recruitment assignment. Matrix is
#     rows = TO, columns = FROM; we post-multiply by the age-0 vector.
# - End-of-year aging:
#   * Survivors from ages 0..(A-2) move to 1..(A-1); plus group A accumulates survivors from (A-1) and A.
# - Outputs:
#   * SSB (year×space), Catch, CatchN, Fbar, Z, survey indices, age comps (OM & catch).
# - Guardrails:
#   * For age-0, if Z[1] == 0 we set it to Z[2] to avoid division by zero in catch calc.
#
# EXTRA NOTES — SURVEY TIMING MATH:
# - df$surveySeason[s]: the season (integer) in which survey s occurs.
# - If df$surveyEnd[s] == 0 → treat as a point-in-time survey at end of that season:
#       N_t(season) * exp(-Z) * Q * lognormalNoise.
# - If df$surveyEnd[s] > 0 → treat as an interval within the survey season from
#   fraction surveyStart to surveyEnd of that season. With constant Z over the interval,
#   we compute the average abundance over the window using the standard integral:
#       avg = N0 * (1 - exp(-Z * Δ)) / (Z * Δ), where N0 is the abundance at start of window
#       (already discounted by exp(-Z * surveyStart)). Multiply by Q and noise.
#
# EXTRA NOTES — Fbar INDEXING:
# - df$Fbarage is a length-2 vector of AGE VALUES in biological ages (often starting at 0 for age-0).
# - Arrays are 1-based in R, so we offset by +1 when slicing the Fseason array:
#       (df$Fbarage[1] + 1) : (df$Fbarage[2] + 1)
#   This matches ages to array rows correctly even if age[1] == 0.
# - We average F across those ages and across space/season for each year.
