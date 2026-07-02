distHeatmap = function(var0,country0,filename='results/dist-heatmap-¤var-¤country.pdf') {
	dist.hm=dist.year
	dist.hm$cn=sapply(dist.hm$country,function(x) cn.names[[x]])
	dist.hm$prop[dist.hm$sex=='Female']=-1*dist.hm$prop[dist.hm$sex=='Female']
	dist.hm=dist.hm %>% 
		filter(data.set=='donation0') %>%
		dplyr::select(cn,level,sex,year,var,prop) %>%
		filter(var==var0) %>% # parameter
		pivot_wider(names_from='level',values_from='prop')
	dist.hm = dist.hm %>% filter(cn==country0) # parameter
	wh=grep('-?[0-9]+',colnames(dist.hm),value=FALSE)
	level.cols=colnames(dist.hm)[wh]
	level.cols=sort(as.integer(level.cols))
	dist.hm=dist.hm[,c(colnames(dist.hm)[1:(min(wh)-1)],as.character(level.cols))]
	dist.hm=dist.hm %>% dplyr::select(-one_of(c('var')))
	level.cols=as.character(level.cols)
	dist.hm[,level.cols]=round(dist.hm[,level.cols],2)
	dist.hm %>% data.frame()
bsAssign('dist.hm')
	sbs=do.call(cbind,by(dist.hm,dist.hm$sex,function(x) x)) 
	use.columns=grep('([0-9]$)|Female.year|Male.sex',colnames(sbs),value=TRUE)
	dist.hm=sbs %>% 
		dplyr::select(one_of(use.columns)) %>%
		rename(year=Female.year,sep.col=Male.sex)
	dist.hm$sep.col=NA

	wh.num=grep('[0-9]$',colnames(dist.hm))
	abs.max=max(dist.hm[,wh.num],na.rm=TRUE) # max(abs(min(ctb$crtn)),abs(max(ctb$crtn)))

	header=paste0('header',sub('[^0-9\\-]+(-?[0-9]+$)?','\\1',colnames(dist.hm)))
	header[1]='year'
	if (length(header)>50) {
		wh.10=grep('0$',header)
		header[-wh.10]=''
	}
	dist.hm=rbind(header,dist.hm)

	inx=expand.grid(row=1:nrow(dist.hm),col=1:ncol(dist.hm))
	vls=lapply(1:nrow(inx),function(x) data.frame(y=inx[x,'row'],x=inx[x,'col'],value=as.character(dist.hm[inx[x,'row'],inx[x,'col']])))
	vls.df=do.call(rbind,vls)
	vls.df$col=NA

	col.minus=colorRampPalette(colors=c('red','white'))(50)
	col.plus=colorRampPalette(colors=c('white','blue'))(50)
	col_vector=c(col.minus,rep('white',3),col.plus[-1])
	length(col_vector)

	# The expansion by 1/1000 is needed to get a colour for the maximum value as well
bsAssign('col_vector')
bsAssign('vls.df')
	value.seq=seq(from=-1.001*abs.max,to=1.001*abs.max,len=length(col_vector))
	wh=which(grepl('^-?[0-9](\\.[0-9]+)?$',vls.df$value))
	cols=left_join(data.frame(value=as.numeric(vls.df$value[wh])),data.frame(inx=1:length(value.seq),tb=value.seq),join_by(closest(x$value<y$tb))) %>% 
		dplyr::select(inx) %>%
		unlist()

	vls.df$col[wh]=cols
	vls.df$col[-wh]=length(col.minus) # 1#length(col_vector)
	vls.df$value[wh]=NA # dont' print the labels here
	table(vls.df$value,useNA='ifany')

	str(vls.df)
	vls.df$value=sub('^header','',vls.df$value)

	col.widths=rep(1,ncol(dist.hm)) # c(5,3,rep(1,ncol(dist.hm)-2))
	s0=sum(col.widths)
	col.widths[1]=s0/20
	col.widths[colnames(dist.hm)=='sep.col']=s0/50

	if (!is.null(filename)) {
		filename=sub('¤var',var0,sub('¤country',country0,filename))
		pdf(filename,height=nrow(dist.hm)*5/25.4,width=7)
	}

	par(mar=c(0.1,1,0.0,0.0)) # bottom,left,top,right bottom 2.2->0
	par(mai=c(0,0,0,0))

	plot(NULL,xlim=c(0,sum(col.widths)),ylim=rev(c(1-0.5,nrow(dist.hm)+0.5)),axes=FALSE,xaxs = "i",yaxs = "i")
	plot.et.data(vls.df,col.widths,hadj=0.5)

	if (!is.null(filename))
		dev.off()

	return(dist.hm)
}

str(dist.year)
dist.year %>%
	group_by(country,var) %>%
	summarise(min.level=min(level,na.rm=TRUE),max.level=max(level,na.rm=TRUE),.groups='drop') %>%
	# rowwise() %>%
	filter(min.level!=max.level) %>%
	apply(1,function(x) {bsAssign('x'); str(x); print(x); distHeatmap(x[['var']],cn.names[[x[['country']]]])})

str(x)
x[['var']]
pah=distHeatmap('age','Netherlands',NULL)
traceback(max=3)