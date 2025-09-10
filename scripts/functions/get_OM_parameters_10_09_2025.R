###############################################################################
# Sandeel multi-area Operating Model — FUNCTION: get_OM_parameters()
# Save as: R/get_OM_parameters_10_09_2025.R
###############################################################################

# ---- 6) get_OM_parameters() ----
#' Prepare parameters for an operating model using a fitted sms model
#' (Self-contained function)
get_OM_parameters <- function(df.tmb,
                              sas = NULL,
                              surveySD = 0.4,
                              recruitment = NULL,
                              nspace = 1,
                              #movemax = 0.3,
                              rec.space = 1,
                              move_age0 = NULL) {
  
  # --- basic sizes ---
  nyears  <- length(df.tmb$years)
  nage    <- df.tmb$nage
  nseason <- df.tmb$nseason
  age     <- df.tmb$age
  
  # rec.space: recruitment allocation across spaces (rescaled to sum 1)
  if (length(rec.space) == 1) rec.space <- rep(rec.space, nspace)
  if (length(rec.space) != nspace) stop("rec.space must be length nspace.")
  if (abs(sum(rec.space) - 1) > 1e-8) rec.space <- rec.space / sum(rec.space)
  
  # --- movement container (age, year, space, season) ---
  movemat <- array(0, dim = c(df.tmb$nage, df.tmb$nyears, nspace, df.tmb$nseason))
  move <- nspace > 1
  if (move) movemat[] <- 0  # adults fixed in place (no 1+ movement)
  
  # --- get fitted parms / recruitment time series ---
  if (is.null(sas)) stop("Please supply 'sas' (fitted smsR object).")
  parms.true <- getEstimatedParms(sas)
  
  if (is.null(recruitment)) {
    # Historical recruitment time series (total, not yet split by space)
    rec <- exp(parms.true$value[parms.true$parameter == "logRin"])
  } else {
    rec <- recruitment
  }
  
  # --- life-history + fishing, expanded to space ---
  F0   <- getF(df.tmb, sas)      # baseline/estimated F season-by-season
  Fsel <- getSel(df.tmb, sas)    # selectivity shapes
  
  F0_flat   <- array(F0$F0,                                 dim = c(nage, nyears, 1, nseason))
  mat_flat  <- array(as.numeric(df.tmb$Mat[,  1:nyears, ]), dim = c(nage, nyears, 1, nseason))
  weca_flat <- array(as.numeric(df.tmb$weca[,1:nyears, ]),  dim = c(nage, nyears, 1, nseason))
  west_flat <- array(as.numeric(df.tmb$west[,1:nyears, ]),  dim = c(nage, nyears, 1, nseason))
  M_flat    <- array(as.numeric(df.tmb$M[,   1:nyears, ]),  dim = c(nage, nyears, 1, nseason))
  Fsel_flat <- array(Fsel$Fsel,                             dim = c(nage, nyears, 1, nseason))
  
  # replicate across spaces
  for (i in 1:(nspace - 1)) {
    if (i == 1) {
      F0x <- F0_flat; matx <- mat_flat; wecax <- weca_flat; westx <- west_flat; Mx <- M_flat; Fselx <- Fsel_flat
    }
    F0x   <- abind::abind(F0x,   F0_flat,   along = 3)
    matx  <- abind::abind(matx,  mat_flat,  along = 3)
    wecax <- abind::abind(wecax, weca_flat, along = 3)
    westx <- abind::abind(westx, west_flat, along = 3)
    Mx    <- abind::abind(Mx,    M_flat,    along = 3)
    Fselx <- abind::abind(Fselx, Fsel_flat, along = 3)
  }
  if (nspace == 1) {
    F0x <- F0_flat; matx <- mat_flat; wecax <- weca_flat; westx <- west_flat; Mx <- M_flat; Fselx <- Fsel_flat
  }
  
  # survey catchability
  Q <- getCatchability(df.tmb, sas)
  Q <- array(Q$Q, dim = c(nage, df.tmb$nsurvey))
  Q[is.na(Q)] <- 0
  
  # assemble OM list
  df.OM <- list(
    years        = df.tmb$years,
    nseason      = nseason,
    nspace       = nspace,
    movemat      = movemat,  # all zeros → no 1+ movement
    age          = age,
    nage         = nage,
    F0           = F0x,
    M            = Mx,
    mat          = matx,
    weca         = wecax,
    west         = westx,
    Fsel         = Fselx,
    propF        = df.tmb$propF,
    propM        = df.tmb$propM,
    Fbarage      = df.tmb$Fbarage,
    betaSR       = df.tmb$betaSR,
    nsurvey      = df.tmb$nsurvey,
    surveyStart  = df.tmb$surveyStart,
    surveyEnd    = df.tmb$surveyEnd,
    surveySD     = surveySD,
    surveySeason = df.tmb$surveySeason,
    Q            = Q,
    recruitment  = "estimated",     # default mode here
    rec.space    = rec.space,       # area split of Rin
    rseason      = df.tmb$recseason,
    Fmodel       = "est",           # use estimated F0 per age/season
    Ninit        = c(0, exp(parms.true$value[parms.true$parameter == "logNinit"])),
    Rin          = rec,             # historical recruitment (total)
    move         = (nspace > 1),
    R0           = df.tmb$betaSR * exp(parms.true$value[parms.true$parameter == "logalpha"]),
    SDR          = exp(parms.true$value[parms.true$parameter == "logSDrec"]),
    b            = rep(0, nyears),  # bias-correction multiplier (used in 'hockey')
    last_year    = max(df.tmb$years)
  )
  
  if (!is.null(move_age0)) {
    stopifnot(is.matrix(move_age0), all(dim(move_age0) == c(nspace, nspace)))
  }
  df.OM$move_age0 <- move_age0
  
  return(df.OM)
}

# ---- NOTES: get_OM_parameters() ----
# TITLE: What this function builds and the “multipliers”
# - Replication multipliers:
#   * All 1-space arrays (F0, M, weights, maturity, selectivity) are replicated to nspace,
#     so each area starts identical unless you tweak per-space values later.
# - Movement multipliers:
#   * movemat is an age × year × space × season 4D array, here filled with zeros → no 1+ movement.
#   * movemax is a cap that would limit adult movement if used; we pass 0 → adults fixed.
#   * move_age0 (separate matrix) controls larval routing; applied inside the run function.
# - Recruitment multipliers:
#   * Rin[year] is the total recruits from smsR. Multiply by rec.space[space] to allocate by area.
#     rec.space is rescaled to sum 1 automatically if needed.
#   * R0 and SDR are derived from fitted parameters (alpha, SDrec); used for alternative SR models.
#   * The 'b' vector (defaults to 0) multiplies the usual lognormal bias-correction term in the
#     hockey-stick option: R.err = exp(-0.5 * b[yr] * SDR^2 + epsilon).
# - F multipliers:
#   * Fmodel == "est" → use F0 (estimated F by age/season). If using Fin (user-specified),
#     the run function computes Fseason = Fin × selectivity.
# - Survey multipliers:
#   * Q (catchability) multiplies abundance at the survey timing; lognormal noise ~ N(0, surveySD^2).

# ---- Function for checking move matrix sums to one, so that we don't just send fish anywhere ----

# --- helper (put near top of 01_main.R) ---
check_move_matrix <- function(route_mat, areas = NULL, normalize = FALSE, tol = 1e-8) {
  # Basic sanity checks
  if (!is.matrix(route_mat)) stop("route_mat must be a matrix.")
  if (any(!is.finite(route_mat))) stop("route_mat contains NA/Inf/NaN.")
  if (any(route_mat < 0) || any(route_mat > 1)) stop("All entries must be in [0,1].")
  
  # Optional: verify column names match areas for readability (not required)
  if (!is.null(areas) && !is.null(colnames(route_mat))) {
    if (!all(colnames(route_mat) == areas)) {
      message("Note: route_mat column names do not match 'areas'; using provided 'areas' for labeling.")
    }
  }
  
  # Columns = FROM; each column should sum to 1 to conserve numbers
  cs <- colSums(route_mat)
  if (!is.null(areas)) names(cs) <- areas
  bad <- which(abs(cs - 1) > tol)
  
  if (length(bad)) {
    message("Columns not summing to 1: ",
            paste0(names(cs)[bad], " (", round(cs[bad], 6), ")", collapse = ", "))
    if (normalize) {
      for (j in bad) if (cs[j] > 0) route_mat[, j] <- route_mat[, j] / cs[j]
      message(" -> Normalized offending columns to sum to 1.")
    } else {
      stop("Fix move_age0 so each column sums to 1 (or set normalize=TRUE to force).")
    }
  }
  route_mat
}

