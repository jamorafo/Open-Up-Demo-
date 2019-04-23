#produit de proba
h = function(x1, x2){
	X=cbind(x1, x2)

	m=dim(X)
	n.obs=m[1]
	n.var=m[2]
	classes=c()
	prob=c()
	#frequencies and probability for each class for each variable

	max=c()
	min=c()
	for (i in 1:n.var){
		n.class=min(length(levels(factor(X[,i]))) , nclass.Sturges(X[,i]))
		factorx = factor(cut(X[,i], breaks=n.class))
		#Tabulate and turn into data.frame
		x.out = as.data.frame(table(factorx))
		#Add cumFreq and proportions
		x.out = transform(x.out, relative = prop.table(Freq))
		pos.vir=regexpr(",", x.out[,1])
		
		len=nchar(as.character.factor(x.out[,1]))

		if(i==1){
			prob1=x.out[,3]
			min1= as.numeric(substr(x.out[,1], 2, pos.vir-1))
			max1=as.numeric(substr(x.out[,1], pos.vir+1, len-1))
		}

		if(i==2){
			prob2=x.out[,3]
			min2= as.numeric(substr(x.out[,1], 2, pos.vir-1))
			max2=as.numeric(substr(x.out[,1], pos.vir+1, len-1))
		}
	}

	p=prob1%*%t(prob2)
	return(p)
}


#####Probabilité jointe
f = function(x1, x2){

	X=cbind(x1, x2)

	n.obs=nrow(X)
	n.var=ncol(X)
	classes=c()
	prob=c()
	#frequencies and probability for each class for each variable
	max=c()
	min=c()
	for (i in 1:n.var){
		n.class=min(length(levels(factor(X[,i]))) , nclass.Sturges(X[,i]))
		factorx = factor(cut(X[,i], breaks=n.class))
		#Tabulate and turn into data.frame
		x.out = as.data.frame(table(factorx))
		#Add cumFreq and proportions
		x.out = transform(x.out, relative = prop.table(Freq))
		pos.vir=regexpr(",", x.out[,1])

		len=nchar(as.character.factor(x.out[,1]))
		if(i==1){
			max1=as.numeric(substr(x.out[,1], pos.vir+1, len-1))
			min1=as.numeric(substr(x.out[,1], 2, pos.vir-1))
		}
		if(i==2){
			max2=as.numeric(substr(x.out[,1], pos.vir+1, len-1))
			min2=as.numeric(substr(x.out[,1], 2, pos.vir-1))
		}

	}
	
	P.joint=matrix(rep(0, length(max1)*length(max2)), nrow=length(max1))
	total=0
	#occurence class cx et cy
	for (k in 1:length(max1)){
		for (l in 1:length(max2)){
			#nombre d obs dont la var X[,i] est dans la classe k et la var X[,j] est dans la classe l
			freq=length(which (X[,1]<=max1[k] & X[,1]>=min1[k] & X[,2]<=max2[l] & X[,2]>=min2[l]))
			P.joint[k,l]=freq/n.obs 
		}

	}
return(P.joint)
}

#Exemple de fonction G
G = function(u){
	return(u*log(u))
}

G1 = function(u){
	return((u-1)^2)
}

G2 = function(u){
	return(u*(1-1/sqrt(u)))
}

G3 = function(u){
	return((1-u)^2/(u+0.01))
}

G4 = function(u){
	return(u*(u^(2/3)-1))
}
################################################################################
# #  General Information Matrix
################################################################################

######################################Fonction générique quantifiant la métrique choisie#########################
#####f est une matrice (n,m)
#h et une matrice (n,m)
# G est une fonction est une fonction univariable

GI= function(f, h, G) {
  
  S = G(f/h)*h
  S[is.nan(S)] = 0
  G = sum(S)
  return(G)
}

################################################################################
# #  Projection in the parallel plane
################################################################################
parallel.plane = function(stitching,data){
  data = as.matrix(data)
  a1 = data[,-ncol(data)]
  a2 = data[,-1]
  # Kronecker product #(1-x[i])*a1+x[i]*a2
  if (is.null(dim(a1))) {
    a1 = t(a1)
    a2 = t(a2)
  }
  kronecker(a1,1-t(stitching)) + kronecker(a2,t(stitching))
}



################################################################################
# #  Misc. Functions
################################################################################
# Scaler function
min_max = function(x,a=0,b=1){
  X_std = (x- min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))
  X_scaled = X_std * (b - a) + a
  return(X_scaled)
}

min_max_back = function(x_scaled,a=0,b=1,min.x,max.x){
  x = ((x_scaled-a)*(max.x-min.x)/(b-a)) + min.x
  return(x)
}

# # Descriptives functions
NaN_consecutive = function(x,count=NA){
  x.na =  is.na(x)*1
  if (!is.na(count==0)){
    fil1 = x==0
    fil1[is.na(fil1)] = FALSE
    x.na =  fil1*1
  }
  rl = rle(x.na)
  x1 = sequence(rle(x.na)$lengths)
  x2 = x.na*x1
  l1 = x2[-1]
  l2 = x2[-length(x2)]
  fil= l2+l1==l2
  max.c = l2[fil]
  if(x2[length(x2)]>0)
    max.c = c(max.c,x2[length(x2)])
  max.c[max.c!=0]
}
descr = function(y) c(min(y),median(y),max(y),mean(y),sd(y),sum(y))

################################################################################
# #  Plot Functions
################################################################################

# Data pre-processing treatment

preprocess_data = function(data){
  data = data[,colnames(x.train)]
  # Standardization (based on the min and the max of the training data)
  x.train.min = sapply(x.train,min)
  x.train.max = sapply(x.train,max)
  data.std = sapply(1:ncol(data),function(i) (data[,i]-x.train.min[i])/(x.train.max[i]-x.train.min[i]) )
  if (is.null(dim(data.std))) data.std = data.frame(t(data.std))
  colnames(data.std) = colnames(data)
  # #  Ordering
  data.sorted   = data.std[,sorted.variables]
  return(data.sorted)
}

# Painting BOZ
# # limBOZ: draw the limits of the BOZ. The limits can be moved according to the e.s parameter.
limBOZ = function(data,lower.limit,upper.limit,e.l=0){
  matplot (aa, cbind(lower.limit-e.l,upper.limit+e.l), lty=1, xlab = "", ylab = "",
           lwd=1,type="l", col="darkviolet", axes=FALSE, add=TRUE)
}

# # BOZ function draws the BOZ.
BOZ = function(data,lower.limit,upper.limit,limits=TRUE){
  aa=seq(1,(ncol(data)-0.05),0.05)
  polygon(c(aa, aa[length(aa):1]), c(lower.limit,upper.limit[length(aa):1]), 
          col=adjustcolor("aquamarine2", alpha.f=0.8), border=NA)
  if (limits) limBOZ(data,lower.limit,upper.limit,e.l=0)
  #abline(v = 1:ncol(data),col="grey70",lty=1.5)
  for (i in 1L:ncol(data))
    lines(c(i, i), c(0, 1), col = "grey70",lwd=0.8)
}

# # EmptyZones function draws the empty zones.
EmptyZones = function(data,empty.zones.limits,e.s=0){
  aa=seq(1,(ncol(data)-0.05),0.05)
  for (ii in 1:length(empty.zones.limits)){
    limits =  empty.zones.limits[[ii]]
    if(!is.null(limits)){
      for(jj in 1:length(limits$lower.limit)){
        lines(rep(aa[ii],2),c(limits$lower.limit[jj]+e.s,limits$upper.limit[jj]-e.s), # e: tolerance
              col="white",lwd=2.5)
      }
    }
  }
}

# draw an observation on the plot
draw_obs = function(obs,lty=2,lwd=2.5,col="firebrick1"){
  redline = (obs>=1.2)*1.2-0.2*(obs<=-0.2)+(obs>-0.2)*(obs<1.2)*obs
  id = aa%in%unique(round(aa))
  if (!id[length(id)]) id[length(id)] = TRUE
  redline = redline[id]
  matplot(redline, type="l",lty=lty, xlab = "", ylab = "", axes = FALSE, col=col, lwd=lwd,add=T)
}




#######

OpenUp = function(obs,e.l=0,e.s=0.1){
  op = par(mar=c(8,0,0,0))
  aa=seq(1,(ncol(x.new.sorted)-0.05),0.05)
  matplot(NA,type="l",lty=1, xlab = "", ylab = "", axes = FALSE, col="white",
          lwd=1,ylim=c(-0.2,1.2),xlim=range(aa))
  xlabels = paste("x_{",1:ncol(x.new.sorted),"}",sep="")
  xlabels = TeX(xlabels)
  
  #draw_obs(obs,lty = 1,col = "grey70",lwd=2)
  BOZ(x.new.sorted,lower.limit,upper.limit,limits = FALSE)
  limBOZ(x.new.sorted,lower.limit,upper.limit,e.l=e.l)
  EmptyZones(x.new,empty.zones.limits,e.s=e.s)
  cross.bounds.matrix = ((lower.limit-e.l)>obs)|((upper.limit+e.l)<obs)
  where_cross = aa[which(cross.bounds.matrix)]
  floor_cross = floor(where_cross)
  ceiling_cross = ceiling(where_cross)
  kpi_id = union(floor_cross,ceiling_cross)
  if(sum(cross.bounds.matrix)==0) {draw_obs(obs,lty = 1,col = "black",lwd=2)
  axis(side = 1,at=1:ncol(x.new.sorted),labels=xlabels,font=2,cex.axis=2,padj=3)}
  if(sum(cross.bounds.matrix)>0) {
    draw_obs(obs,lty = 1,col = "red",lwd=1)
    axis(1, at = kpi_id, labels = xlabels[kpi_id], font=2,cex.axis=2,col="red",padj=3, lwd=2)
  }
  on.exit(par(op))
}

################################################################################
# #  Anomaly generation
################################################################################
# intruder is the index of the variable in the plot to be affected for the noise.
anomaly_example = function(intruder){
  normal_id = sample(id_good,1)
  normal_obs = x.training[normal_id,]
  normal_obs.sorted = preprocess_data(normal_obs)
  # ruido
  eta = rbeta(length(intruder),shape1=2,shape2=5)
  normal_obs.sorted[intruder] = sample(c(1+e.l+eta ,-(e.l+eta)),length(intruder))
  normal_obs.parallel = parallel.plane(stitching,normal_obs.sorted)
  OpenUp(normal_obs.parallel)
}

# 
acc_table = function(data,s,e.s,e.l){
if (e.l<0.02) e.l <- 0.02
if (e.s<0.02) e.s <- 0.02
filter <- data$s==round(s,2) & round(data$e.s,2)==round(e.s,2) & round(data$e.l,2)==round(e.l,2)
out<- output.best[filter,c("accuracy","false.alarms","fault.detection")]*100
out<- format(round(out,2), nsmall = 2)
colnames(out) <- c("Accuracy (%)", "False alarms (%)","Failure to detect (%)")
return(out)
}


