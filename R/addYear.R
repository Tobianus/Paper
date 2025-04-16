addYear <- function(df.in,
                    new_years = 20,
                    method = 'last',
                    avg_years = 3,
                    F_future = rep(0, df.in$nspace),
                    b_new = 1){

  # Stuff to add to df.in
  # F0
  # M
  # mat
  # weca
  # west
  # Fsel
  # propF
  # propM
  # (R)
  # b

  nyear <- length(df.in$years)
  nage <- df.in$nage
  nseason <- df.in$nseason
  nspace <- df.in$nspace


  Fnew <- weca <- movemat <- M <- mat <- Fsel <- west <- array(0, dim = c(nage, new_years, nspace, nseason))


  if(method == 'last'){

  for(i in 1:new_years){
    for(j in 1:nspace){
    Fnew[,i,j,] <- df.in$Fsel[,nyear,j,]*F_future[j]

    }
    weca[,i,,] <- df.in$weca[,nyear,,]
    west[,i,,] <- df.in$west[,nyear,,]
    M[,i,,] <- df.in$M[,nyear,,]
    mat[,i,,] <- df.in$mat[,nyear,,]
    Fsel[,i,,] <- df.in$Fsel[,nyear,,]
    movemat[,i,,] <- df.in$movemat[,nyear,,]
  }
    
  df.in$F0 <- abind::abind(df.in$F0, Fnew, along = 2)
  df.in$weca <- abind::abind(df.in$weca, weca, along = 2)
  df.in$west <- abind::abind(df.in$west, west, along = 2)
  df.in$M <- abind::abind(df.in$M, M, along = 2)
  df.in$mat <- abind::abind(df.in$mat, mat, along = 2)
  df.in$Fsel <- abind::abind(df.in$Fsel, Fsel, along = 2)
  df.in$movemat <- abind::abind(df.in$movemat, movemat, along = 2)

  df.in$b <- c(df.in$b, rep(b_new, new_years))




  dmnames<- list(df.in$age,
                 c(df.in$years, (max(df.in$years)+1):(max(df.in$years)+new_years)),
                 1:df.in$nspace,
                 1:df.in$nseason)

  dimnames(df.in$F0) <- dmnames
  dimnames(df.in$weca) <- dmnames
  dimnames(df.in$west) <- dmnames
  dimnames(df.in$M) <- dmnames
  dimnames(df.in$mat) <- dmnames
  dimnames(df.in$Fsel) <- dmnames

    }

df.in$years <-  c(df.in$years, (max(df.in$years)+1):(max(df.in$years)+new_years))
df.in$new_years <- (max(df.in$years)+1):(max(df.in$years)+new_years)


  return(df.in)
}
