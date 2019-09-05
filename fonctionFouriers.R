# Ensemble de fonctions stat utilisées
#
fourier3.cal <- function(x,per,frac=0.5) {
# ========================================================================
# Calage d'une série de Fourier à 1 harmonique sur le vecteur x de période per
# renvoie les coefficients de sin et cos
# x    : le vecteur de longueur libre commençant au pdt 1 de la période
# per  : la période 
# frac : la fraction mini de la période nécessaire au calcul
# ========================================================================
# V0 le 2009 - A.Poirel
# V1 le 27/12/2011 - Modification pour accepter les valeurs manquantes dans
#                    la limite de 1/2 de la periode et de 4 valeurs min
# ========================================================================
# 
 n=length(x)
 nper=floor(n/per)
 # si on a assez de données pour le calcul
 if ( sum(!is.na(x)) > max(6,frac*nper*per) ){ 
   f3=rep(NA,3)
   # si on a très peu de cycles, il faut interpoler les valeurs
   if ( (nper<=2) & sum(is.na(x))>0 ) { 
     xest=approx(c(1:n),x,c(1:n))$y
     x[is.na(x)]=xest[is.na(x)]
   }
   f3[1]=mean(x,na.rm=TRUE)
   f3[3]=2*mean(x*sin(seq(1:n)*2*pi/per),na.rm=TRUE)
   f3[2]=2*mean(x*cos(seq(1:n)*2*pi/per),na.rm=TRUE)
 } else { f3=c(mean(x,na.rm=TRUE),0,0) }
return(f3)
}

#

fourier5.cal <- function(x,per,frac=0.5)  {
# ========================================================================
# Calage d'une série de Fourier à 2 harmoniques sur le vecteur x de période per
# renvoie les coefficients de sin et cos
# ========================================================================
# V0 le 2009 - A.Poirel
# V1 le 27/12/2011 - Modification pour accepter les valeurs manquantes dans
#                    la limite de 1/2 de la periode
# ========================================================================
# 
 n=length(x)
 nper=floor(n/per)
 if (sum(!is.na(x))>max(10,frac*nper*per)){
   f5=rep(NA,5)
   # si on a très peu de cycles, il faut interpoler les valeurs
   if (sum(is.na(x))>0) {
     xest=approx(c(1:n),x,c(1:n))$y
     x[is.na(x)]=xest[is.na(x)]
   }
  f5[1]=mean(x,na.rm=TRUE)
  f5[3]=2*mean(x*sin(seq(1:n)*2*pi/per),na.rm=TRUE)
  f5[5]=2*mean(x*sin(seq(1:n)*4*pi/per),na.rm=TRUE)
  f5[2]=2*mean(x*cos(seq(1:n)*2*pi/per),na.rm=TRUE)
  f5[4]=2*mean(x*cos(seq(1:n)*4*pi/per),na.rm=TRUE)
 } else {f5=c(mean(x,na.rm=TRUE),0,0,0,0)}
 return(f5)
} 

fourier3.calphase <- function(x,per,frac=0.5)  {
# ========================================================================
# Calage d'une série de Fourier à 1 harmonique sur le vecteur x de période per
# renvoie les coefficients de  cos(alpha*t+phase)
# ========================================================================
# V0 le 2009 - A.Poirel
# ========================================================================
#                   
 f3=rep(NA,3)
 fbase=fourier3.cal(x,per,frac=frac)
 if (!is.na(fbase[1]) & (sum(abs(fbase[2:3]))>0) ) {
    f3[1]=fbase[1]
    f3[2]=(fbase[2]^2+fbase[3]^2)^0.5
    if ((fbase[2]<0) & (fbase[3]>0)) f3[3]= pi + atan(-fbase[3]/fbase[2])
    if ((fbase[2]>0) & (fbase[3]>0)) f3[3]= atan(-fbase[3]/fbase[2])
    if ((fbase[2]>0) & (fbase[3]<0)) f3[3]= -atan(fbase[3]/fbase[2])
    if ((fbase[2]<0) & (fbase[3]<0)) f3[3]= pi+atan(-fbase[3]/fbase[2])
    f3[3]=f3[3]*per/(2*pi)
   if (f3[3]<0 ) f3[3]=f3[3]+per
 } else f3=fbase
return(f3)
}



fourier5.calphase <- function(x,per,frac=0.5)  {
# ========================================================================
# Calage d'une série de Fourier à 2 harmoniques sur le vecteur x de période per
# renvoie les coefficients de  cos(alpha*t+phase)
# ========================================================================
# V0 le 2009 - A.Poirel
# ========================================================================                   
 f5=rep(NA,5)
 fbase=fourier5.cal(x,per,frac=frac)
 if (!is.na(fbase[1]) & (sum(abs(fbase[2:5]))>0)  ) {
   f5[1]=fbase[1]
   f5[2]=(fbase[2]^2+fbase[3]^2)^0.5
   if ((fbase[2]<0) & (fbase[3]>0)) f5[3]= pi + atan(-fbase[3]/fbase[2])
   if ((fbase[2]>0) & (fbase[3]>0)) f5[3]= atan(-fbase[3]/fbase[2])
   if ((fbase[2]>0) & (fbase[3]<0)) f5[3]= -atan(fbase[3]/fbase[2])
   if ((fbase[2]<0) & (fbase[3]<0)) f5[3]= pi+atan(-fbase[3]/fbase[2])
   f5[3]=f5[3]*per/(2*pi)
   if (f5[3]<0) {f5[3]=f5[3]+per}

   f5[4]=(fbase[4]^2+fbase[5]^2)^0.5
   if ((fbase[4]<0) & (fbase[5]< 0)) f5[5]=  pi+ atan(-fbase[5]/fbase[4])
   if ((fbase[4]>0) & (fbase[5]< 0)) f5[5]= atan(-fbase[5]/fbase[4])
   if ((fbase[4]>0) & (fbase[5]> 0)) f5[5]= -atan(fbase[5]/fbase[4])
   if ((fbase[4]<0) & (fbase[5]> 0)) f5[5]= pi+ atan(-fbase[5]/fbase[4])
   f5[5]=f5[5]*per/(4*pi)
   if (f5[5]<0) {f5[5]=f5[5]+per}
 } else f5=fbase

 return(f5)
} 


fourier3.sim <- function(long,per,ff3) {
# ========================================================================
# Renvoie une série de Fourier à 1 harmonique de période per sur longueur long=n*per
# à partir d'une série sin et cos
# ========================================================================
# V0 le 2009 - A.Poirel
# ========================================================================              
fsim=rep(ff3[1],long)+
     ff3[2]*cos(seq(1:long)*2*pi/per)+
     ff3[3]*sin(seq(1:long)*2*pi/per)
fsim[is.nan(fsim)]=NA
return( fsim)
} 


fourier5.sim <- function(long,per,ff5)  {
# ========================================================================
# Renvoie une série de Fourier à 2 harmoniques de période per sur longueur long=n*per
# à partir d'une série sin et cos
# renvoie les coefficients de  cos(alpha*t+phase)
# ========================================================================
# V0 le 2009 - A.Poirel
# ========================================================================                   
fsim=rep(ff5[1],long)+
     ff5[2]*cos(seq(1:long)*2*pi/per)+
     ff5[3]*sin(seq(1:long)*2*pi/per)+
     ff5[4]*cos(seq(1:long)*4*pi/per)+
     ff5[5]*sin(seq(1:long)*4*pi/per)
fsim[is.nan(fsim)]=NA
return( fsim)
} 


fourier3.simphase <- function(long,per,ff3) {
# ========================================================================
# Renvoie une série de Fourier à 1 harmonique de période per sur longueur long=n*per
# renvoie les coefficients de cos(alpha*t+phase)
# ========================================================================
# V0 le 2009 - A.Poirel
# ========================================================================                     
fsim=rep(ff3[1],long)+ ff3[2]*cos((seq(1:long) + ff3[3])*2*pi/per)
fsim[is.nan(fsim)]=NA
return( fsim)
} 


fourier5.simphase <- function(long,per,ff5) {
# ========================================================================
# Renvoie une série de Fourier à 2 harmoniques de période per sur longueur long=n*per
# renvoie les coefficients de cos(alpha*t+phase)
# ========================================================================
# V0 le 2009 - A.Poirel
# ========================================================================                     
fsim=rep(ff5[1],long)+
     ff5[2]*cos((seq(1:long) + ff5[3])*2*pi/per)+
     ff5[4]*cos((seq(1:long) + ff5[5])*4*pi/per)
fsim[is.nan(fsim)]=NA
return( fsim)
} 
