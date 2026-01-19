#####################
# Remove the placeholder values for very small and very large Hb values and NA's
hb.freq = hb.freq[abs(hb.freq$Hb) < 1000 & !is.na(hb.freq$Hb),]


## ----function-definitions---------------------------------------------------------------------------------------------------------------------------------------------------------------
plotColBySex = function(basics,col,ylab,xvar='year') {
  sex.col = data.frame(sex=c('Female','Male'),col=c('red','blue'))
  rownames(sex.col)=sex.col$sex
  
  par(mar=c(2,4,0,4))
  plot.cols=c(col)
  plot(basics[[xvar]],basics[[plot.cols[1]]],type='n',ylab=ylab)
  for (i in 1:nrow(sex.col)) {
    data1 = basics[basics$Sex==sex.col$sex[i],]
    for (col in plot.cols) {
      lines(data1[[xvar]],data1[[col]],col=sex.col$col[i],lwd=2)
      
      for (suf in c('low','hi')) {
        ci.var = paste0(col,'.',suf)
        if (ci.var %in% names(basics)) {
          lines(data1[[xvar]],data1[[ci.var]],lty='dotted',lwd=1,col=sex.col$col[i])
        }
      }
      
      frml=paste0(col,'~',xvar)
      m=lm(formula(frml),data=data1)
      sm=summary(m)
      abline(a=sm$coeff[1,1],b=sm$coeff[2,1],col=sex.col$col[i],lty='dashed',lwd=2)
      text(x=min(data1[[xvar]]),y=min(data1[[col]])+0.01,
           labels=paste0('b=',round(100*sm$coeff[2,1],5),'%,p=',round(sm$coeff[2,4],3)),adj=c(0))
    }
  }
}

# Some of these function definitions may be obsolete, should check and remove the unnecessary ones.
plotHbDist = function(data0,sex,cutoff,zoom=TRUE) {
  sex0=data0[data0$year>=2005&data0$Sex==sex & 
              !is.na(data0$Hb)&data0$BloodDonationTypeKey %in% c('Whole Blood (K)','No Donation (E)'),c('Hb')]  
  freq = sex0 %>%
    group_by(Hb) %>%
    summarise(n=n()) %>%
    mutate(prop=n/sum(n))

  limits = NULL
  if (zoom)
    limits = c(cutoff-5,cutoff+5)
  plot(prop~Hb,data=freq,type='l',lwd=2,xlim=limits)

  mean0=mean(sex0$Hb)
  sd0=sd(sex0$Hb)
  
  text(x=cutoff,y=0.03,labels=paste0('mean=',mean0,', sd=',sd0))
  
  ndist=dnorm(unique(freq$Hb),mean=mean0,sd=sd0)
  lines(unique(freq$Hb),ndist)
  abline(v=cutoff,col='blue')
  
  return(list(mean0=mean0,sd0=sd0))
}

estimateTrend = function(final) {
  final.longer = matToLonger(final)
  final.longer$interceptFemale=1
  final.longer$interceptFemale[final.longer$Sex=='Male']=0
  final.longer$interceptMale=1-final.longer$interceptFemale
  m=lm(value~0+interceptFemale+interceptMale+year:Sex,data=final.longer)
  print(summary(m))
  return(m)
}

matToLonger = function(mat) {
  df1=as.data.frame(pivot_longer(cbind(Sex=rownames(mat)[1:2],mat[1:2,]),colnames(mat),names_to='year'))
  df1$year=as.integer(df1$year)
  return(df1)
}

# This function is in active use, but it doesn't seem to refer the functions above
plotYearSex = function(m,relative=FALSE) {
  cofs = summary(m)$coeff
  conames=rownames(cofs)
  codf=data.frame(name=conames,estimate=cofs[,1])
  wh=grep('(Male|Female)',conames)
  codf$Sex[wh]=sub('.*(Male|Female).*','\\1',conames[wh])
  wh=grep('[0-9][0-9][0-9][0-9]',conames)
  codf$year[wh]=sub('[^0-9]+([0-9]+).*','\\1',conames[wh])
  codf=cbind(codf,confint(m))
  
  codf=codf[!is.na(codf$year),]
  pah=pivot_wider(codf,names_from='year',values_from='estimate',id_cols=c('Sex'))
  rn = as.vector(pah[,1])
  pah=as.data.frame(pah[,-1])
  rownames(pah)=rn$Sex
  
  means=apply(pah,1,mean)
  if (!relative)
    means=0*means
  
  pah=pah-means
  
  pah2=pivot_wider(codf,names_from='year',values_from='2.5 %',id_cols=c('Sex'))
  pah2=as.data.frame(pah2[,-1])
  pah2=pah2-means
  pah3=pivot_wider(codf,names_from='year',values_from='97.5 %',id_cols=c('Sex'))
  pah3=as.data.frame(pah3[,-1])
  pah3=pah3-means
  
  pah.all=rbind(pah,pah2,pah3)
  
  matplot(t(pah.all),type='l',lty=c(rep('solid',2),rep('dashed',4)),col=rep(c('red','blue'),2),axes=FALSE)
  axis(2)
  axis(side=1,at=1:ncol(pah.all),labels=colnames(pah.all))
  return(pah.all)
}


## ----distribution-analysis--------------------------------------------------------------------------------------------------------------------------------------------------------------
# This is the function that correct the kink in a distriution (data0)
# The function can be either called with data0 != NULL; data0 should then have the structure of simple, donation0 or donation.r
#   and can be a subset of these.
# *or* with freq != NULL, with the structure of hb.freq
# Nb! This function operates on a single distribution
rectifyDistribution = function (data0,sex=NULL,cutoff=NULL,plot=TRUE,freq=NULL) {
  if (is.null(cutoff)) {
    if (is.null(sex)) {
      if (is.null(data0)) {
        sex = unique(freq$Sex)[1]
      } else
        sex = unique(data0$Sex)[1]
    } 
    cutoff = if (sex=='Female') param$cutoff.female else param$cutoff.male
  }

  if (!is.null(data0)) {
    # sex0=data0[data0$Sex==sex&!is.na(data0$Hb)&data0$BloodDonationTypeKey %in% c('Whole Blood (K)','No Donation (E)'),c('Hb')]  
    freq = data0 %>%
      group_by(Hb) %>%
      summarise(n=n()) %>%
      mutate(prop=n/sum(n))
  } else {
    freq = freq %>%
      group_by(Hb) %>%
      summarise(n=sum(n),.groups='drop')
  }
  
  freq = freq[!is.na(freq$Hb),]
  
  if (!'prop' %in% colnames(freq)) {
    sum.n = sum(freq$n) # -coalesce(freq$nas,0))
    freq$prop = freq$n / sum.n
  }
  
  freq.mean = freq %>%
    filter(!is.na(Hb)) %>% # These are the summary rows for NA; leave them out at this point
    summarise(n2=sum(n),mean0=sum(Hb*(n))/n2,.groups='drop')
  
  freq.stats = freq %>%
    filter(!is.na(Hb)) %>% # These are the summary rows for NA; leave them out at this point
    mutate(dev=Hb-freq.mean$mean0) %>% # ,prop=n/n2) %>%
    group_by() %>%
    summarise(mean=min(freq.mean$mean0),n0=min(n),var=sum(prop*dev^2),sd=sqrt(var),
              skewness=sum(prop*(dev/sd)^3),kurtosis=sum(prop*(dev/sd)^4),.groups='drop')
  mean0= freq.stats$mean
  sd0 = freq.stats$sd

  Hb.values = sort(unique(freq$Hb))
  if (FALSE) {
    dx <- 1. #depends on units, but should now all be converted to g/L
  } else  {
    if (param$hb.decimals == 1){
      dx <- 0.1 #depends on units, but should now all be converted to g/L
    } else{
      dx = 1.
    }
  }
  ndist=dnorm(Hb.values,mean=mean0,sd=sd0)*dx
  
  # Plotting the unchanged distribution
  if (plot) {
    plot(prop~Hb,data=freq,type='l',lwd=2)
    lines(Hb.values,ndist)
  
    # A plot showing the difference between the distribution defined by data0 and the
    # normal distribution estimated from that data
    plot(freq$prop-ndist~Hb.values)
    abline(v=cutoff,col='blue')
    abline(h=c(-1,1)*0.0005,col='red')
  }

  # This is the part that 
  cnt = 0
  freq2 = freq
  # cover the case that there is no data at the cutoff value (should not occur with reasonable data volumes)
  # k = which(freq$Hb==cutoff)
  k = max(which(freq$Hb<=cutoff))
  if (length(k) == 0 || k == -Inf) 
    k = min(which(freq$Hb>=cutoff))
  while (TRUE) {
    diff = (freq2$prop-ndist)[1:(k-1)]
    wh = which(diff < -0.0005)
    if (length(wh) == 0)
      break
    wh0 = max(wh)
    ds0 = diff[wh0]
    
    diff.plus = (freq2$prop-ndist)[k:nrow(freq2)]
    wh = which(diff.plus > 0.0005)
    if (length(wh) == 0)
      break
    wh1 = min(wh)
    ds1 = diff.plus[wh1]
    
    to.adjust = min(-ds0,ds1)
    
    freq2$prop[wh0] = freq2$prop[wh0] + to.adjust
    freq2$prop[wh1+(k-1)] = freq2$prop[wh1+(k-1)] - to.adjust
    
    cnt = cnt + 1
    if (cnt > 1000) {
      print('max iterations exceeded (back-stop)')
      break
    }
  }

  mean1 = freq2 %>% 
      mutate(mom=Hb*prop) %>%
      summarise(mean=sum(mom))
  mean1=as.numeric(mean1)
  sd1 =  freq2 %>% 
      mutate(mom=(Hb-as.numeric(mean1))^2*prop) %>%
      summarise(sdx=sum(mom)) 
  sd1=sqrt(as.numeric(sd1))
  
  # The theoretical deferred proportion is computed here
  deferred.prop = pnorm(cutoff-0.5,mean1,sd1)

  if (plot) {
    plot(prop~Hb,data=freq2,type='l',lwd=3,xlim=NULL)
    lines(prop~Hb,data=freq2,col='green', lwd=2)
    ndist2=dnorm(Hb.values,mean=mean1,sd=sd1)*dx
    lines(Hb.values,ndist2,col='red',lty='dotted',lwd=2)
    abline(v=cutoff,col='blue',lty='dashed')
    rect(mean1-5,0,mean1+5,0.001,col='pink',lwd=2)
    abline(v=mean1,lty='dotted',lwd=3)
    
    plot(freq2$prop-ndist2~Hb.values)
    abline(v=cutoff,col='blue',lty='dashed')
    abline(h=c(-1,1)*0.0005,col='red',lty='dashed')
  }

  return(list(dist=freq2,params=list(mean=mean1,sd=sd1,deferred.prop=deferred.prop)))
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Example of rectifying the distribution
freq <- hb.freq[hb.freq$Sex=='Female', ]
rectifyDistribution(NULL,'Female',param$cutoff.female,freq=freq)
# dist.male = rectifyDistribution(simple,'Male',cutoff)
# dist.female = rectifyDistribution(simple,'Female',cutoff)


## ----getStats-function------------------------------------------------------------------------------------------------------------------------------------------------------------------
# This function produces statistics of value.col (Hb or age) based on groups defined by group.col (can be year or age) and sex
# If age.freq is provided, the proportions are included in the results (relevant when value.col == 'Hb')
getStats = function(freq,value.col='Hb',group.col='year',n.filter=100,age.freq=NULL,age.breaks=c(-Inf,24,40,Inf),age.labels=c('young','mid','old')) {
  freq = data.frame(freq)
  
  if (is.null(n.filter) || is.na(n.filter))
    n.filter = 0
  
  wh = min(which(colnames(freq)==value.col))
  colnames(freq)[wh]='value'
  wh.group = min(which(colnames(freq)==group.col))
  colnames(freq)[wh.group]='group'

  new.basics.mean = freq %>%
    # filter(is.null(data.set) || data.set==data.set) %>%
    filter(!is.na(value)) %>% # These are the summary rows for NA; leave them out at this point
    group_by(group,Sex) %>%
    summarise(n2=sum(n-nas),deferred.prop=sum(deferred/n2),mean0=sum(value*(n-nas))/n2,.groups='drop')
  
  new.basics = freq %>%
    filter(!is.na(value)) %>% # These are the summary rows for NA; leave them out at this point
    inner_join(new.basics.mean,join_by(group,Sex)) %>%
    mutate(dev=value-mean0,prop=n/n2) %>%
    group_by(group,Sex) %>%
    summarise(mean=min(mean0),n0=min(n),n=min(n2),deferred.prop=min(deferred.prop),var=sum(prop*dev^2),sd=sqrt(var),
              skewness=sum(prop*(dev/sd)^3),kurtosis=sum(prop*(dev/sd)^4),.groups='drop') %>%
    mutate(sd.low=sqrt((n-1)*var/qchisq(c(.975), n-1))) %>%
    mutate(sd.hi =sqrt((n-1)*var/qchisq(c(.0275), n-1))) %>%
    mutate(mean.low=mean-qnorm(0.025)*sd/sqrt(n)) %>%
    mutate(mean.hi =mean-qnorm(0.975)*sd/sqrt(n)) %>%
    filter(n>n.filter) %>%
    data.frame()
  
  wh.group = min(which(colnames(new.basics)=='group'))
  colnames(new.basics)[wh.group]=group.col
  
  if (value.col=='Hb') {
    freq.limits = freq %>% group_by(group,Sex) %>% summarise(limit=min(limit),.groups='drop') %>% mutate(year=group)
    new.basics = new.basics %>%
      inner_join(freq.limits,join_by(year,Sex)) %>%
      mutate(theoretical.deferred.prop = pnorm(limit,mean,sd)) %>%
      dplyr::select(-limit)
  }
  
  if (!is.null(age.freq)) {
    year.totals = age.freq %>%
      group_by(year,Sex) %>%
      summarise(n2=sum(n),age=mean(age),.groups='drop')
    
    age2 = age.freq %>%
      mutate(group = cut(age,age.breaks,labels=age.labels)) %>%
      group_by(year,Sex,group) %>%
      summarise(n=sum(n),.groups='drop') %>%
      inner_join(year.totals,join_by(year,Sex)) %>%
      mutate(prop=n/n2) %>% # ,name=paste(year,Sex)) %>%
      pivot_wider(id_cols=c('year','Sex'),names_from='group',values_from='prop')


    new.basics = new.basics %>%
      inner_join(age2,join_by(year,Sex)) %>%
      inner_join(year.totals[,c('year','Sex','age')],join_by(year,Sex))
  }

  
  return(new.basics)
}


## ----county-specific-basics-------------------------------------------------------------------------------------------------------------------------------------------------------------
getCountryBasics = function(hb.freq,age.freq,cf=1) {
  # hb.freq = data$hb.freq; age.freq=data$age.freq
  freq0=hb.freq[hb.freq$data.set=='donation0',]
  age0 = age.freq[age.freq$data.set=='donation0',]
  rv=by(freq0,freq0[,c('Sex','year')],function(x) unlist(rectifyDistribution(data0=NULL,freq=x,plot=FALSE)$params))
  res=array2DF(rv)
  res$year=as.integer(res$year)
  
  # Basic statistics for the unrectified hb distributions
  basics.all = getStats(freq0,age.freq = age0)
  
  # Basic statistics joined with results from the rectified distributions
  basics = inner_join(basics.all,res,
                      join_by(year,Sex),suffix=c('','.rectified'))
  
  if (cf==1) 
    return(basics)
  colnames(basics)
  
  convert.cols = grep('^mean|sd',colnames(basics))
  basics[,convert.cols] = basics[,convert.cols]*cf
  
  convert.cols.sq = grep('^var',colnames(basics))
  basics[,convert.cols.sq] = basics[,convert.cols.sq]*cf^2
  
  return(basics)
}


## ----basic-data-processing--------------------------------------------------------------------------------------------------------------------------------------------------------------
freq0=hb.freq[hb.freq$data.set=='donation0',]

# Rectify the male distribution from all years as an example
rectifyDistribution(data0=NULL,freq=freq0[freq0$Sex=='Male',])

rv=by(freq0,freq0[,c('Sex','year')],function(x) unlist(rectifyDistribution(data0=NULL,freq=x,plot=FALSE)$params))
res=array2DF(rv)
res$year=as.integer(res$year)

# Basic statistics for the unrectified hb distributions
basics.all = getStats(hb.freq,age.freq = age.freq)

# Basic statistics joined with results from the rectified distributions
basics = inner_join(basics.all,res,
                    join_by(year,Sex),suffix=c('','.rectified'))


## ----plotting---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# means and such
plotColBySex(basics,'mean','mean (actual data)')
plotColBySex(basics,'mean.rectified','theoretical mean (rectified)')
plotColBySex(basics,'sd','sd (actual data)')
plotColBySex(basics,'sd.rectified','theoretical sd (rectified)')

# deferral rates
plotColBySex(basics,'deferred.prop','actual percentage deferred')
plotColBySex(basics,'theoretical.deferred.prop','theoretical proportion deferred (unrectified distribution)')
plotColBySex(basics,'deferred.prop.rectified','theoretical proportion deferred (rectified distribution)')
# plotColBySex(basics,'dperc.lm','adjusted using estimated mean differences')


## ----new-lm-approach--------------------------------------------------------------------------------------------------------------------------------------------------------------------
newLMapproach = function(basics) {
  # This block relies on basics, so need to make sure it can be computed with the summary data
  # Plus must add countries overall
  # This all seems to be based on basics
  par(mar=c(2,4,0,0))
  data=basics[basics$Sex=='Female',]
  data.female=data
  mean.female=mean(data$deferred.prop)
  plot(deferred.prop~mean,data=data,ylim=c(0,0.15),xlim=c(130,160),col='red')
  m=lm(deferred.prop~mean,data=data)
  m1=lm(deferred.prop~mean+sd,data=data)
  sm=summary(m)
  sm1=summary(m1)
  sm1.female=sm1
  
  m2.female=NULL
  if ('young' %in% colnames(data)) {
    m2.female=lm(deferred.prop~mean+sd+young+old,data=data)
  }

    abline(a=sm$coeff[1,1],b=sm$coeff[2,1],col='red',lty='dashed',lwd=2)
  text(x=param$cutoff.male,y=0.03,labels=paste0('b=',round(sm1$coeff[2,1],3),'\np=',round(sm1$coeff[2,4],3),'\nR',intToUtf8(178),'=',round(sm1$r.squared,3)),adj=c(0))
  data=basics[basics$Sex=='Male',]
  m=lm(deferred.prop~mean,data=data)
  m1=lm(deferred.prop~mean+sd,data=data)
  sm=summary(m)
  sm1=summary(m1)
  abline(a=sm$coeff[1,1],b=sm$coeff[2,1],col='blue',lty='dashed',lwd=2)
  points(data$mean,data$deferred.prop,col='blue')
  text(x=155,y=0.06,labels=paste0('b=',round(sm1$coeff[2,1],3),'\np=',round(sm1$coeff[2,4],3),'\nR',intToUtf8(178),'=',round(sm1$r.squared,3)),adj=c(0))
  plot(data$year,sm1.female$residuals,col='red',ylim=c(-0.05,0.05),type='l',ylab='residuals',lwd=3)
  lines(data$year,sm1$residuals,col='blue',lwd=3)
  mean.male=mean(data$deferred.prop)
  lines(data$year,data.female$deferred.prop-mean.female,col='red',lty='dotted',lwd=2)
  lines(data$year,data$deferred.prop-mean.male,col='blue',lty='dotted',lwd=2)
  
  year=data$year
  residual=data.female$deferred.prop-mean.female
  m=lm(residual~year)
  summary(m)
  
  table(data$Sex)
  m1=lm(deferred.prop~mean+sd+age,data=data)
  summary(m1)
  
  m1=lm(deferred.prop~mean+sd+age,data=data.female)
  summary(m1)

  m2.male=NULL
  if ('young' %in% colnames(data)) {
    m2.male=lm(deferred.prop~mean+sd+young+old,data=data)
  }
  
  return(list(m.female=m2.female,m.male=m2.male))
}


## ----experimental-results---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Do the young and old age proportions explain deferral rates:
# They do, but only for men 
rv=newLMapproach(basics)
# summary(rv$m.female)
# summary(rv$m.male)

plotColBySex(basics,'old','proportion of 40+ yrs of age')


## ----statistics-by-age------------------------------------------------------------------------------------------------------------------------------------------------------------------
agedist = getStats(age.freq %>% filter(data.set=='all'),value.col='mean.hb',group.col='age',n.filter=100)
plotColBySex(agedist,'mean',xvar = 'age',ylab='')

plotColBySex(agedist,'n',xvar = 'age',ylab='')
plotColBySex(agedist,'mean',xvar = 'age',ylab='')
plotColBySex(agedist,'sd',xvar = 'age',ylab='')

plotColBySex(basics,'age',xvar='year',ylab='age by year')

#####################
# survival plotting

#####
# Below, some plots are drawn for survival data
colours=list()
colours[['top 10%']]='blue3'
colours[['top 10-25%']]='lightblue'
colours[['bottom 10-25%']]='pink'
colours[['bottom 10%']]='red3'
colours[['(25,40]']]='green3'
colours[['(40,100]']]='gray3'
colours[['O-']]='blue3'

getIntervals = function(breaks) {
	brs=strsplit(breaks,',')[[1]]
	mid.bits=sapply(2:(length(brs)-2),function(x) paste0('(',brs[x],',',brs[x+1],']'))
	lower=paste0('<',brs[2])
	upper=paste0('>',brs[length(brs)-1])
	return(c(lower,mid.bits[-2],upper))
}

pdf('results/figures.pdf')
by(res.models,res.models[,c('sex','var')],function(y) {
	plot(x=NULL,type='n',xlim=c(1,max(y$ord)),ylim=c(0.70,1.40),main=paste(y$sex[1],y$var[1]),
		xlab='number of donation',ylab='relative likelihood of donation')
	abline(h=1,lwd=2,lty='dotted')
	breaks=y$breaks[1]
	brk.labels=if (breaks!='-') getIntervals(breaks) else ''
	levels=unique(y$level)
	legend(x='bottomright',legend=paste(levels,brk.labels),fill=sapply(levels,function(x) colours[[x]]))
	by(y,y[,c('var','level')],function(x) {
			gr=x$level[1]
			col=colours[[gr]]
			ordint=x$ord
			lines(ordint,x$exp.coef,lwd=2,col=col)
			lines(ordint,x$lower..95,lwd=2,lty='dotted',col=col)
			lines(ordint,x$upper..95,lwd=2,lty='dotted',col=col)
		})
	})
dev.off()

pdf('results/curves.pdf')
dummy=by(res.curves,res.curves[,c('sex','ord')],function(df) {
bsAssign('df')
		wh=min(which(df$surv<0.99))
		len0=length(df$surv)
		df=df[wh:len0,]
		# df$surv=df$surv[wh:len0]
		df$time=df$time[wh:len0]-wh
		df$sqrt.x=sqrt(df$time)

		# This is promising, but seems not to work after all
		# m.ss=nls(surv~SSweibull(time,yf,y0,log_alpha,poweri),data=df)
		m.ss=nls(surv~SSasymp(sqrt.x,yf,y0,log_alpha),data=df)
		sm=summary(m.ss)

		plot(surv~time,data=df,type='l',lwd=2,xlim=c(0,2*365),ylim=c(0,1),
			main=paste(df$sex[1],df$ord.group[1]))
		pred.ss=predict(m.ss,data.frame(sqrt.x=df$sqrt.x))
		lines(df$time,pred.ss,col='red2',lwd=2.5)

		tv=qt(0.025,df=sm$df[2])
		return(data.frame(sex=df$sex[1],ord=df$ord[1],var=rownames(sm$coeff),sm$coeff,lower=sm$coeff[,1]-tv*sm$coeff[,2],upper=sm$coeff[,1]+tv*sm$coeff[,2]))
	})
dev.off()

ce=do.call(rbind,dummy)

# nb! should use the similar implementation in the previous project to enable easier and more parametric
# plotting with countries etc.
plotParameters = function(ce,xvar='log_alpha',yvar='yf',col.var='sex',pp.cols=list(Male='blue3',Female='red3'),shade.var='ord') {
	lims = ce %>% 
		group_by(var) %>%
		summarise(min=min(Estimate),max=max(Estimate)) %>%
		data.frame()
	rownames(lims)=lims$var
	lims$var=NULL

	# plot(x=NULL,xlim=c(-3,-1),ylim=c(0,0.33),xlab='exponent',ylab='asymptote')
	plot(x=NULL,xlim=t(lims[xvar,]),ylim=t(lims[yvar,]),xlab='exponent',ylab='asymptote')
	for (gr in names(pp.cols)) {
		xdata=ce %>%
			filter(as.character(!!!syms(col.var))==gr,var==xvar) %>%
			arrange(!!!syms(shade.var))
		ydata=ce %>%
			filter(as.character(!!!syms(col.var))==gr,var==yvar) %>%
			arrange(!!!syms(shade.var))

		shades=colorRampPalette(c(pp.cols[[gr]],"white"))(5+nrow(xdata))

		col0=col=shades[xdata$ord]
		points(xdata$Estimate,ydata$Estimate,col=col0,lwd=3,pch=10)
		lines(xdata$Estimate,ydata$Estimate,col=col0,lwd=1,lty='dashed')

		# This is done to make the error bars at least somewhat visible
		# col0='black'
		arrows(xdata$Estimate,ydata$lower,xdata$Estimate,ydata$Estimate,length=0.05,angle=90,code=3,col=col0)
		arrows(xdata$Estimate,ydata$Estimate,xdata$Estimate,ydata$upper,length=0.05,angle=90,code=3,col=col0)

		arrows(xdata$lower,ydata$Estimate,xdata$Estimate,ydata$Estimate,length=0.05,angle=90,code=3,col=col0)
		arrows(xdata$Estimate,ydata$Estimate,xdata$upper,ydata$Estimate,length=0.05,angle=90,code=3,col=col0)
	}
}

pdf('results/parameters.pdf')
plotParameters(ce)
dev.off()
