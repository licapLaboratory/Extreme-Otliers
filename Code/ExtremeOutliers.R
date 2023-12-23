#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--- GENERALIZED EXTREME VALUE OF GUMBEL (TYPE I) DISTRIBUTION
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

GEVGumbel <- function(df, windowRange, T_Percent, showInfo=FALSE){

  # Parameters:
  #     df         : Data frame containing the time series with one or more columns
  #     windowRange: Initial windowing or points' range to build the Gumbel's distribution  
  #     T_Percent  : Threshold of the estimation by the cumulative distribution function of Gumbel
  #     showInfo   : Show variables' values of Gumbel distribution
  # 
  # Object return with info:
  #     extreme vector; mean; coeff. variation; parameters of scale, localization, and tail; pdf and cdf of Gumbel's distribution; trigger; indexes and values of estimated.

  setClass(Class="gevGumbel",representation(vetExt="vector",x_mean="numeric",s_CoefVar="numeric",sig="numeric",mi="numeric",csi="numeric",gz_Density="vector",Gz_Cumulat="vector",T_Percent="numeric",xT_FreqFactor="numeric",val="vector",idx="vector"))
  
  df <- data.frame(unlist(rbind(df)))
  colnames(df) <- "S1"
  
  vetExt <- df[windowRange,1]
  
  x_mean     <- mean(vetExt)
  s_CoefVar  <- sqrt( sum((vetExt - x_mean)^2) / (length(vetExt)-1) )
  EulerMasch <- 0.5772156649
  
  sig <- sqrt( (6 * (s_CoefVar^2)) / (pi^2) )
  mi  <- x_mean - EulerMasch * sig
  csi <- 0
  
  gz_Density <- sort((1 / sig) * exp( -((vetExt - mi) / sig) ) * exp( -exp( -((vetExt - mi) / sig) ) ))
  Gz_Cumulat <- sort(exp( -exp( -((vetExt - mi) / sig) ) ))
  
  xT_FreqFactor <- -log( log( (1 / T_Percent) ) ) * sig + mi
  
  idx <- which(vetExt >= xT_FreqFactor)
  val <- vetExt[which(vetExt >= xT_FreqFactor)]
  
  if(showInfo){
    cat("\nExtreme data vector      :","[",length(vetExt),"]",head(vetExt),"...")
    cat("\nMean extreme data        :",x_mean)
    cat("\nCoefficient of variation :",s_CoefVar)
    cat("\nScale parameter          :",sig)
    cat("\nLocation parameter       :",mi)
    cat("\nTail parameter           :",csi) 
    cat("\ng(z) GEV P.D.F. values   :","[",length(gz_Density),"]",head(gz_Density),"...")
    cat("\nG(z) GEV C.D.F. values   :","[",length(Gz_Cumulat),"]",head(Gz_Cumulat),"...")
    cat("\nGEV C.D.F. threshold     :",T_Percent)
    cat("\nFreq. factor (trigger)   :",xT_FreqFactor)
    cat("\nExtreme indexes          :","[",length(idx),"]",idx)
    cat("\nExtreme values           :","[",length(val),"]",val)
  }
  return( new(Class="gevGumbel",vetExt=vetExt,x_mean=x_mean,s_CoefVar=s_CoefVar,sig=sig,mi=mi,csi=csi,gz_Density=gz_Density,Gz_Cumulat=Gz_Cumulat,T_Percent=T_Percent,xT_FreqFactor=xT_FreqFactor,val=val,idx=idx) )
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
