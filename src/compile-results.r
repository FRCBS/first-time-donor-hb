source('src/analysis-functions.r')

include.captions=TRUE
captions=list()

# age month hour / levels and margins
html.table.ml='<table><tr>
<td><b>Female</b></td>
<td><b>Male</b></td> </tr><tr>

<td><img width=500 src="../results/hb-levels-age-Female.png"></td>
<td><img width=500 src="../results/hb-levels-age-Male.png"></td> </tr><tr>
<td><img width=500 src="../results/hb-margins-age-Female.png"></td>
<td><img width=500 src="../results/hb-margins-age-Male.png"></td> </tr><tr>

<td><img width=500 src="../results/hb-levels-month-Female.png"></td>
<td><img width=500 src="../results/hb-levels-month-Male.png"></td> </tr><tr>
<td><img width=500 src="../results/hb-margins-month-Female.png"></td>
<td><img width=500 src="../results/hb-margins-month-Male.png"></td> </tr><tr>

<td><img width=500 src="../results/hb-levels-hour-Female.png"></td>
<td><img width=500 src="../results/hb-levels-hour-Male.png"></td> </tr><tr>
<td><img width=500 src="../results/hb-margins-hour-Female.png"></td>
<td><img width=500 src="../results/hb-margins-hour-Male.png"></td> </tr>

</table>'

captions$figure.ml="<b>Figure ML</b> Levels and estimated deviations by age and sex 
(females on left, males on right). See legend for colours in top-left panel."

html.file=sub('¤table¤',paste(html.table.ml,if(include.captions) captions$figure.ml else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-ml levels margins.html'))

### heatmaps

# age month hour / levels and margins
html.table.ml='<table><tr>
<td><img width=500 src="../results/heatmap-Australia"></td> </tr><tr>
<td>(a) Australia</td> </tr><tr>
<td><img width=500 src="../results/heatmap-Finland"></td> </tr><tr>
<td>(b) Finland</td> </tr><tr>
<td><img width=500 src="../results/heatmap-France"></td> </tr><tr>
<td>(c) France</td> </tr><tr>
<td><img width=500 src="../results/heatmap-Navarre"></td> </tr><tr>
<td>(d) Navarre</td> </tr><tr>
<td><img width=500 src="../results/heatmap-Netherlands"></td> </tr>
<td>(e) Netherlands</td> </tr><tr>
</table>'

captions$figure.h="<b>Figure H</b> Heatmaps for the countries, panels&nbsp;(a) through&nbsp;(e). Red tones 
indicate negative and blue tones positive corrections that are added to the mean hemoglobin values to 
achieve corrected hemoglobin values. All units in g/L. The heatmaps show that overall, largest corrections 
are due to age and changes in the age distribution over the years. The rectification of distributions produces 
noticable but rather constant corrections (where applicable)."

html.file=sub('¤table¤',paste(html.table.ml,if(include.captions) captions$figure.h else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-h heatmaps.html'))

#### trends

# age month hour / levels and margins
html.table.t='<table><tr>
<td><img width=1800 src="../results/trends-corrected.pdf"></td> </tr>
</table>'

captions$figure.t="<b>Figure T</b> Mean (solid lines) and corrected (dashed lines) hemoglobin levels. <br>Fitted trend lines have been added where there is a statistically significant trend."

html.file=sub('¤table¤',paste(html.table.t,if(include.captions) captions$figure.t else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-t trends.html'))

# example rectification
html.table.r='<table><tr>
<td><img width=900 src="../results/rectify-distribution-sample.pdf"></td> </tr>
</table>'

captions$figure.r="<b>Figure R</b> Example of hemoglobin distribution: Finland, year 2013, females. 
The black line shows the original distribution with an artefact caused by remeasurement after an 
initial measurement below the cutoff value (125 g/L, green vertical line). 
The post-rectification distribution is drawn in red. The mean values of the original and rectified 
distributions are illustrated with black and red, respectively, vertical dashed lines. 
The normal distribution following the mean and standard deviation of the rectified distribution is 
illustrated with red, dotted line."

html.file=sub('¤table¤',paste(html.table.r,if(include.captions) captions$figure.r else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-r rectification.html'),page.width=10)

#### Table 1
table1=annual.hb %>%
	filter(data.set=='donation0') %>%
	group_by(country,year) %>%
	summarise(n=sum(n.donor)/1000,.groups='drop') %>%
	left_join(data.frame(use.years,hit=''),join_by(country,between(year,y$year.min,y$year.max))) %>%
	mutate(n=paste0(n,hit),n=sub('(.+)NA','(\\1)',n)) %>%
	dplyr::select(country,year,n) %>%
	pivot_wider(names_from='country',values_from='n') %>%
	arrange(year) %>%
	data.frame()
colnames(table1)[-1]=sapply(colnames(table1)[-1],function(x) cn.names[[x]])
table1[,1]=as.character(table1[,1])

html.table1=paste(capture.output(print(xtable(table1,align=c('l',rep('r',ncol(table1)-0))),type='html',include.rownames=FALSE)),collapse='\n')
html.table1=gsub('&amp;','&',html.table1)
caption='<b>Table 1</b> Number of new donors per country and year. Years with their number in parentheses were not used in the analysis.'
html.file=sub('¤table¤',paste0(caption,'\n',html.table1),html.template)
cat(html.file,file=paste0(param$shared.dir,'table-1.html'))

##### survival

# relative curves

html.table.s='<table><tr>
<td><img width=500 src="../results/survival-joint-ord.group.full-Female-NA.png"></td>
<td><img width=500 src="../results/survival-joint-ord.group.full-Male-NA.png"></td> </tr><tr>

<td>(a)</td>
<td>(b)</td> </tr><tr>

<td><img width=500 src="../results/survival-joint-bloodgr-Female-NA.png"></td>
<td><img width=500 src="../results/survival-joint-bloodgr-Male-NA.png"></td> </tr><tr>

<td>(c)</td>
<td>(d)</td> </tr><tr>

<td><img width=500 src="../results/survival-joint-age.group.t-Female--15-20-.png"></td>
<td><img width=500 src="../results/survival-joint-age.group.t-Male--15-20-.png"></td> </tr><tr>

<td>(e)</td>
<td>(f)</td> </tr><tr>

<td><img width=500 src="../results/survival-joint-hb.surplus-Female-bottom-10-.png"></td>
<td><img width=500 src="../results/survival-joint-hb.surplus-Male-bottom-10-.png"></td> </tr><tr>

<td>(g)</td>
<td>(h)</td> </tr><tr>

<td><img width=500 src="../results/survival-joint-sex-Female-NA.png"></td>
<td><img width=500 src="../results/survival-sample-age.group.t-fi-female.png"></td> </tr>

<td>(i)</td>
<td>(j)</td> </tr><tr>

<td><img width=500 src="../results/survival-cn0-Female.png"></td>
<td><img width=500 src="../results/survival-cn0-Male.png"></td> </tr>

<td>(k)</td>
<td>(l)</td> </tr><tr>


</table>'

# <td><img width=500 src="../results/survival-joint-hb.surplus-Female-bottom-10-.png"></td>
# <td><img width=500 src="../results/survival-joint-hb.surplus-Male-bottom-10-.png"></td> </tr><tr>
# <td>(i)</td>
# <td>(j)</td> </tr><tr>

captions$figure.s="<b>Figure S</b> Relative retention by various various groups: (a, b)&nbsp;Relative likelikelihood
of next donation after the second etc. donation compared with after the first donation for females and males.
(c,d)&nbsp;Relative likehood of retention for females and males, respectively, for O negative blood group compared with all other blood groups as reference,
(e,f)&nbsp;Relative likehood of retention for females and males, respectively, in age of at most 20 years at donation, 
compared with the reference age group of 41 to 45 years. 
(g,h)&nbsp;Similarly for bottom 10% of hemoglobin surplus (excess to threshold) with the mid-50% group as reference.
(i)&nbsp;Similarly for males, with females as reference group, 
(j)&nbsp;example (Finnish females) of relative likelihoods of retention for age groups (at donation), with 41 to 45 as reference,
(k,l)&nbsp;relative likelihood of retention for females and males, for different blood establishments (Australia as reference)"

html.file=sub('¤table¤',paste(html.table.s,if(include.captions) captions$figure.s else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-s relative survival.html'))

# curves and parameters

html.table.c='<table><tr>
<td><img width=500 src="../results/survival-curves-1-Female.png"></td>
<td><img width=500 src="../results/survival-curves-1-Male.png"></td> </tr><tr>

<td><img width=500 src="../results/survival-curves-16-Female.png"></td>
<td><img width=500 src="../results/survival-curves-16-Male.png"></td> </tr><tr>

<td><img width=500 src="../results/survival-parameters-Female.png"></td>
<td><img width=500 src="../results/survival-parameters-Male.png"></td> </tr>

</table>'

captions$figure.c="<b>Figure C</b> Survival as a function of time for (a)&nbsp;1 and 
(b)&nbsp;16 previous donations. Data for females on the left and males on the right. Top row: retention after
first donation. Second row: retention after 16 or more donations. Legend for colours in bottom row. While there are significant differences 
between blood establishments in retention after first donation, the differences tend to vanish with 
increasing number of donations. This phenomenon can also be seen from the parameter estimates at the bottom 
row, where the trajectories converge towards the bottom-right corner for all blood establishments."

html.file=sub('¤table¤',paste(html.table.c,if(include.captions) captions$figure.c else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-c curves.html'))

# table 1 (for survival)

# nb! 
getCountriesStats = function(var) {
	stats.list=lapply(names(countries),function(x) {
		countries[[x]][[var]] %>%
			# rowwise() %>%
			# filter(grepl('cutoff',name)) %>%
			mutate(country=x,var=var) %>%
			data.frame()
	})
	stats=do.call(rbind,stats.list)

	wh=which(grepl('age',colnames(stats)))
	if (length(wh) > 0) 
		colnames(stats)[wh]='age' # hack
	return(stats)
}

stats.age=getCountriesStats('stats.age')
stats.age.t=getCountriesStats('stats.age.t')
stats.ord=getCountriesStats('stats.ord')

str(stats.age)

# number of donors by sex
st.donor= stats.ord %>%
	group_by(country,sex,var) %>%
	filter(ord==1) %>%
	summarise(value=max(n),.groups='drop') %>%
	mutate(var='Donors (in 1,000)',value=as.character(value/1000)) %>%
	pivot_wider(names_from='country',values_from='value')

# number of donations
st.donations=stats.ord %>%
	group_by(country,sex,var) %>%
	# filter(ord==1) %>%
	summarise(value=sum(n),.groups='drop') %>%
	mutate(var='Donations (in 1,000)',value=as.character(value/1000)) %>%
	pivot_wider(names_from='country',values_from='value')

# mean age at donation
st.age.t=stats.age.t %>%
	group_by(country,sex,var) %>%
	filter(!is.na(age)) %>%
	summarise(value=sum(n*age)/sum(n),.groups='drop') %>%
	mutate(var='Mean age at donation',value=round(value,2)) %>%
	pivot_wider(names_from='country',values_from='value')

st.age0=stats.age %>%
	group_by(country,sex,var) %>%
	filter(!is.na(age)) %>%
	summarise(value=sum(n*age)/sum(n),.groups='drop') %>%
	mutate(var='Mean age at first donation',value=round(value,2)) %>%
	pivot_wider(names_from='country',values_from='value')

st.all=rbind(st.donor,st.donations,st.age.t) %>%
	mutate(ord=row_number()) %>%
	arrange(sex,ord) %>%
	rowwise() %>%
	mutate(sex=if (ord > 2) '' else sex) %>%
	dplyr::select(-ord)
colnames(st.all)[-(1:2)]=sapply(colnames(st.all)[-(1:2)],function(x) cn.names[[x]])
colnames(st.all)[2]='Quantity'
colnames(st.all)=firstUp(colnames(st.all))
st.all


html.table1.s=paste(capture.output(print(xtable(st.all,align=c('l','l',rep('r',ncol(st.all)-1))),type='html',include.rownames=FALSE)),collapse='\n')
html.table1.s=gsub('&amp;','&',html.table1.s)
caption='<b>Table 1S</b> Descriptive statistics of study sample'
html.file.1s=sub('¤table¤',paste0(caption,'\n',html.table1.s),html.template)
cat(html.file.1s,file=paste0(param$shared.dir,'table-1 survival.html'))
