################################################################################
# #  Misc. Functions
################################################################################
# Scaler function
min_max = function(x,a=0,b=1){
  X_std = (x- min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))
  X_scaled = X_std * (b - a) + a
  return(X_scaled)
}
