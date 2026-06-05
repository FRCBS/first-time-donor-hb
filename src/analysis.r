source('src/analysis-functions.r')

vars2=c('ord.group.full','bloodgr','age.group.t','hb.surplus','sex')
vl.comb=left_join(data.frame(var=vars2),unique((res.models %>% mutate(level=coalesce(level,'-')))[,c('var','level')]),join_by(var))
singles = vl.comb %>% 
	group_by(var) %>%
	summarise(n=n()) %>%
	filter(n==1) %>%
	dplyr::select(var)
vl.comb$level[vl.comb$var %in% unlist(singles)]=NA
vl.comb$legend.position=NA
vl.comb$legend.position[1]='topleft'
vl.comb$x.max=NA
vl.comb[vl.comb$var=='ord.group.full','x.max']=40

hr.plot.extras.fun = function() {
	abline(h=1,lty='dashed')
	# abline(v=15.5,lty='dashed')
}

pdf('results\\survival-joint-with-levels.pdf',width=12,height=6)
file.pattern='results/survival-joint-¤var-¤sex-¤level.pdf'
lapply(rownames(vl.comb),function(x) {
		# x=vars.levels[[x]]
		x=unclass(vl.comb[x,])
		df=res.models %>% filter(var==x$var)

		level=''
		if ('level' %in% names(x) && !is.na(x$level)) {
			df=df %>% filter(level==x$level)
			level=x$level
		}

		legend.position=''
		if ('legend.position' %in% names(x) && !is.na(x$legend.position)) {
			legend.position=x$legend.position
		}

		if (x$var!='sex') {
			par(mfrow=c(1,2))
		} else {
			# nb! must fetch new data and remove this hard-coding
			df$sex='Female'
		}

		if (!is.na(x$x.max)) 
			df=df %>% filter(ord<=x$x.max)

		ycols=c('exp.coef','lower..95','upper..95')
		ylim=c(min(df[,ycols]),max(df[,ycols]))
		by(df,df[,'sex'],function(y) {
			sex0=y[1,'sex']
			x$sex=sex0

			main=paste(x$var,sex0,level)

			local.plot=FALSE
			if (!is.null(file.pattern) && file.pattern!='') {
				filename=gsub('[](%,]','-',subFromList(file.pattern,x))
				filename=gsub(' ','-',filename)

				local.plot=TRUE

				pdf(filename,width=7,heigh=5)
				# par(mar=c(0.1,5.5,0.5,0.6)) # no space at the top; bottom,left,top,right bottom 2.2->0
				par(mar=c(4.1,4.1,0.2,0.1)) # modifired measures with left and bottom margins for labels and some at top for y-axis labels
				par(cex=1.25,cex.axis=1.25,cex.lab=1.25)
				main=''
			}
			
			plotByGroups(y,group.cols=c(NA,'country'),xcol='ord',ycols=ycols,y.lim=ylim,
				colours=colours,ltys=ltys,main=main,trends='',legend.position=legend.position,
				extras.fun=hr.plot.extras.fun,x.max=x$x.max)

			if (local.plot)
				dev.off()
		})
	})

dev.off()

# nb! using sqrt(t) implies steeper initial decline than in the standard exponential model
t=0:500
y=exp(-0.002*t)
y2=exp(-0.002*sqrt(500)*sqrt(t))
plot(y~t,type='l',col='blue3',lwd=3)
lines(t,y2,lwd=3)

res.curves=res.curves %>% arrange(country,ord,sex,time)
dummy=by(res.curves,res.curves[,c('country','ord','sex')],function(df) {
		wh=min(which(df$surv<0.99))
		len0=length(df$surv)
		df=df[wh:len0,]
		df$time=df$time[wh:len0]-wh
		df$sqrt.x=sqrt(df$time)

		# This is promising based on description, but seems not to work after all
		# m.ss=nls(surv~SSweibull(time,yf,y0,log_alpha,poweri),data=df)
		m.ss=NULL
		try(m.ss<-nls(surv~SSasymp(sqrt.x,yf,y0,log_alpha),data=df))

		if (is.null(m.ss)) {
			return(NULL)
		}
		sm=summary(m.ss)

		# plot(surv~time,data=df,type='l',lwd=2,xlim=c(0,2*365),ylim=c(0,1),
		#	main=paste(df$sex[1],df$ord[1]))
		pred.ss=predict(m.ss,data.frame(sqrt.x=df$sqrt.x))
		# lines(df$time,pred.ss,col='red2',lwd=2.5)

		tv=qt(0.025,df=sm$df[2])
		return(list(parameters=data.frame(country=df$country[1],sex=df$sex[1],ord=df$ord[1],var=rownames(sm$coeff),sm$coeff,lower=sm$coeff[,1]-tv*sm$coeff[,2],upper=sm$coeff[,1]+tv*sm$coeff[,2]),
			fitted.data=data.frame(country=df$country[1],sex=df$sex[1],ord=df$ord[1],time=df$time,surv=df$surv,fitted=pred.ss)))
	})
ce=do.call(rbind,lapply(dummy,function(x) x$parameters))
fitted=do.call(rbind,lapply(dummy,function(x) x$fitted.data))

pdf('results\\survival-joint-curves.pdf')
by(fitted,fitted$ord,function(x) {
	# plot here
	plot(x=NULL,xlim=c(0,2*365),ylim=c(0,1),main=x$ord[1],ylab='survival',xlab='time')
	by(x,x[,c('sex','country')],function(y) {
		country0=y$country[1]
		sex0=y$sex[1]
		col=colours[[country0]]
		# pch=pchs[[sex0]]

		lines(fitted~time,data=y,col=col,lwd=2,lty='dotted')
		lines(surv~time,data=y,col=col,lwd=2,lty=if (sex0=='Female') 'solid' else '8282')

		# wh=seq(1,y$time[nrow(y],by=50)
		# points(y$surv[wh],)
	})
})
dev.off()

pdf('results\\survival-joint-curves.pdf')
by(fitted,fitted$ord,function(x) {
	# plot here
	plot(x=NULL,xlim=c(0,2*365),ylim=c(0,1),main=x$ord[1],ylab='survival',xlab='time')
	by(x,x[,c('sex','country')],function(y) {
		country0=y$country[1]
		sex0=y$sex[1]
		col=colours[[country0]]
		# pch=pchs[[sex0]]

		lines(fitted~time,data=y,col=col,lwd=2,lty='dotted')
		lines(surv~time,data=y,col=col,lwd=2,lty=if (sex0=='Female') 'solid' else '8282')

		# wh=seq(1,y$time[nrow(y],by=50)
		# points(y$surv[wh],)
	})
})
dev.off()

# In single files, separately for males and females
# pdf('results\\survival-joint-curves.pdf')
fitted.116=fitted %>% filter(ord %in% c(1,16))
by(fitted.116,fitted.116$ord,function(x) {
	# plot here
	ord0=x$ord[1]

	by(x,x[,c('sex')],function(y) {
		sex0=y$sex[1]
		pdf(paste0('results/survival-curves-',ord0,'-',sex0,'.pdf'),width=7,height=5.5)
		par(mar=c(4.1,4.1,0.2,0.1)) # modifired measures with left and bottom margins for labels and some at top for y-axis labels
		par(cex=1.25,cex.axis=1.25,cex.lab=1.25)
		plot(x=NULL,xlim=c(0,2*365),ylim=c(0,1),main='',ylab='survival',xlab='time') # main=x$ord[1]
		by(y,y$country,function(z) {
			country0=z$country[1]
			sex0=z$sex[1]
			col=colours[[country0]]
			# pch=pchs[[sex0]]

			lines(fitted~time,data=z,col=col,lwd=2,lty='dotted')
			lines(surv~time,data=z,col=col,lwd=2) # ,lty=if (sex0=='Female') 'solid' else '8282')
		})
		dev.off()
	})
})
# dev.off()

# Both sexes in the same plot
pdf('results/survival-parameters.pdf',width=7,height=6)
par(mar=c(4.1,4.1,0.2,0.1)) # modifired measures with left and bottom margins for labels and some at top for y-axis labels
par(cex=1.25,cex.axis=1.25,cex.lab=1.25)
plotParameters2(ce,xvar='log_alpha',yvar='yf',group.by=c('country','sex'),col.var='country',pp.cols=colours,shade.var='ord')
dev.off()

lims = ce %>% 
	group_by(var) %>%
	summarise(min=min(Estimate),max=max(Estimate)) %>%
	data.frame()
rownames(lims)=lims$var
lims$var=NULL
lims

source('src/analysis-functions.r')
# Sexes in different plots
by(ce,ce$sex,function(ce0) {
bsAssign('ce0')
	sex0=ce0$sex[1]
	pdf(paste0('results/survival-parameters-',sex0,'.pdf'),width=7,height=6)
	par(mar=c(4.1,4.1,0.2,0.1)) # modifired measures with left and bottom margins for labels and some at top for y-axis labels
	par(cex=1.25,cex.axis=1.25,cex.lab=1.25)
	plotParameters2(ce0,xvar='log_alpha',yvar='yf',group.by=c('country','sex'),col.var='country',pp.cols=colours,shade.var='ord',ylim=t(lims['yf',]))
	dev.off()
})

sapply(unique(res.models$country),plotSurvivalCurvesByCountry)

cairo_pdf('results/survival-sample-age.group.t-fi-female.pdf',width=7,heigh=5)
par(mar=c(4.1,4.1,0.2,0.1)) # modifired measures with left and bottom margins for labels and some at top for y-axis labels
par(cex=1.10,cex.axis=1.10,cex.lab=1.10)
plotSurvivalCurvesByCountry('fi',plot.what='figures',filters=list(sex='Female',var='age.group.t'),pdf.internal=FALSE,draw.confint=FALSE)
dev.off()