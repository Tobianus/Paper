#' Ready parameters for an operating model using a fitted sms model
#'
#' @param df.tmb
#' @param sas
#' @param move
#' @param surveySD
#' @param recruitment
#'
#' @return
#' @export
#'
#' @examples
get_OM_parameters <- function(df.tmb = NULL, sas = NULL,
                              surveySD = 0.4,
                              recruitment = NULL,
                              nspace = 1,
                              moveinit = 1,
                              movemax = 0.3,
                              rec.space = 1,
                              moveslope = .7,
                              movefifty = 1
){
  
  
  # Do the movement parameters
  
  if(is.null(df.tmb)){
    nseason <- 2
    age <- 0:5
    nage <- length(age)
    
  }
  
  
  if(sum(rec.space) != 1){
    warning('sum of recruitment is not equal to one, rescaling')
    rec.space <- rec.space/sum(rec.space)
    
  }
  
  if(length(moveinit) ==1){
    moveinit <- rep(1/nspace, nspace)
  }
  # Maturity
  
  # Assign movement out of area
  if(length(movemax) == 1){
    movemax <- rep(movemax,df.tmb$nseason)
  }
  
  if(length(movemax) != nspace){
    stop('insert movement rates for each area')
  }
  
  
  movemat <- array(0, dim = c(df.tmb$nage,df.tmb$nyear, nspace, df.tmb$nseason)) # Chances of moving in to the other grid cell
  age <- df.tmb$age
  
  
  if(nspace == 1){
    move = FALSE
  }else{
    move = TRUE
  }
  
  if(move == TRUE){
    for(j in 1:nspace){
      for(i in 1:df.tmb$nseason){
        movemat[,,j,i] <- movemax[j]/(1+exp(-moveslope*(age-movefifty)))
        
      }
    }
    movemat[1,,,] <- 0 # Recruits and 1 year olds don't move
    
  }
  
  
  if(is.null(sas) == FALSE){
    parms.true <- getEstimatedParms(sas)
  }
  
  if(is.null(recruitment)){
    rec <- exp(parms.true$value[parms.true$parameter == 'logRin'])
  }
  
  # Turn life history parameters into spatial objects
  F0 <- getF(df.tmb, sas)
  Fsel <- getSel(df.tmb,sas)
  
  
  
  # Into matrix
  F0_flat <- array(F0$F0, dim = c(df.tmb$nage,df.tmb$nyears,1,df.tmb$nseason))
  mat_flat <- array(as.numeric(df.tmb$Mat[,1:df.tmb$nyears,]), dim = c(df.tmb$nage,df.tmb$nyears,1,df.tmb$nseason))
  weca_flat <- array(as.numeric(df.tmb$weca[,1:df.tmb$nyears,]), dim = c(df.tmb$nage,df.tmb$nyears,1,df.tmb$nseason))
  west_flat <- array(as.numeric(df.tmb$west[,1:df.tmb$nyears,]), dim = c(df.tmb$nage,df.tmb$nyears,1,df.tmb$nseason))
  M_flat <- array(as.numeric(df.tmb$M[,1:df.tmb$nyears,]), dim = c(df.tmb$nage,df.tmb$nyears,1,df.tmb$nseason))
  Fsel_flat <- array(Fsel$Fsel, dim = c(df.tmb$nage,df.tmb$nyears,1,df.tmb$nseason))
  
  # Abind to two spatial objects
  # This assumes the same M, weca, F, and mat in the number of areas
  # Loop through spatial areas
  for(i in 1:(nspace-1)) {
    
    if(i == 1) {
      # Expand relative.catch[i, ] to match F0_flat dimensions
      relative.catch_expanded <- array(relative.catch[i, ],
                                       dim = c(1, length(relative.catch[i, ]), 1, df.tmb$nseason))
      
      # Broadcast to match all dimensions of F0_flat
      relative.catch_expanded <- array(rep(relative.catch_expanded, df.tmb$nage),
                                       dim = dim(F0_flat))  # Ensures exact shape match
      
      # Perform the multiplication
      F0 <- F0_flat * nspace * relative.catch_expanded
      
      # Assign other arrays for the first area
      mat <- mat_flat
      weca <- weca_flat
      west <- west_flat
      M <- M_flat
      Fsel <- Fsel_flat
    }
    
    # Perform further calculations for other areas, if any
    F0 <- abind::abind(F0, F0_flat * nspace * relative.catch_expanded, along = 3)
    mat <- abind::abind(mat, mat_flat, along = 3)
    west <- abind::abind(west, west_flat, along = 3)
    weca <- abind::abind(weca, weca_flat, along = 3)
    M <- abind::abind(M, M_flat, along = 3)
    Fsel <- abind::abind(Fsel, Fsel_flat, along = 3)
  }
  
  
  Q <- getCatchability(df.tmb, sas)
  Q <- array(Q$Q, dim = c(df.tmb$nage, df.tmb$nsurvey), )
  Q[is.na(Q)] <- 0
  
  
  
  
  
  
  df.OM <- list(
    years = df.tmb$years,
    nseason = df.tmb$nseason,
    nspace = nspace,
    movemat = movemat,
    age = df.tmb$age,
    nage = length(df.tmb$age),
    F0 = F0,
    M = M,
    mat = mat,
    weca = weca,
    west = west,
    Fsel = Fsel,
    propF = df.tmb$propF,
    propM = df.tmb$propM,
    Fbarage = df.tmb$Fbarage,
    betaSR = df.tmb$betaSR,
    nsurvey = df.tmb$nsurvey,
    surveyStart = df.tmb$surveyStart,
    surveyEnd = df.tmb$surveyEnd,
    surveySD = surveySD,
    surveySeason = df.tmb$surveySeason,
    Q = Q,
    recruitment = 'estimated',
    rec.space = rec.space,
    rseason = df.tmb$recseason,
    Fmodel = 'est',
    Ninit = c(0,
              exp(parms.true$value[parms.true$parameter == 'logNinit'])),
    Rin = rec,
    move = move,
    R0 =  df.tmb$betaSR*exp(parms.true$value[parms.true$parameter == 'logalpha']),
    SDR = exp(parms.true$value[parms.true$parameter == 'logSDrec']),
    b = rep(0, df.tmb$nyears),
    last_year = max(df.tmb$years)
  )
  
  
  
  
  
  
  return(df.OM)
  
}
