# Adstock functions

#=========================================================
# http://drunks-and-lampposts.com/2012/01/08/recovering-the-parameters-in-a-marketing-mix-model/
adstock_calc_1<-function(media_var,dec,dim=100){
	l <- length(media_var)
	adstock<-rep(0,l)
	for(i in 2:l){
		adstock[i]<-(1-exp(-media_var[i]/dim)+dec*adstock[i-1])
	}
	adstock
}
	 
adstock_calc_2<-function(media_var,dec,dim=100){
	l<-length(media_var)
	adstock<-rep(0,l)
	for(i in 2:l){
		adstock[i]<-1-exp(-(media_var[i]+dec*media_var[i-1])/dim)
	}
	adstock
}
#=========================================================

# Simple
adstock_calc_3<-function(media_var,dec){
	l<-length(media_var)
	adstock<-rep(0,l)
	for(i in 2:l){
		adstock[i] <- (1-dec)*media_var[i] + dec*adstock[i-1]
	}
	adstock
}
