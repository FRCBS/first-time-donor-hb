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

<td colspan="2">(a) By age</td>  </tr><tr>

<td><img width=500 src="../results/hb-levels-month-Female.png"></td>
<td><img width=500 src="../results/hb-levels-month-Male.png"></td> </tr><tr>
<td><img width=500 src="../results/hb-margins-month-Female.png"></td>
<td><img width=500 src="../results/hb-margins-month-Male.png"></td> </tr><tr>

<td colspan="2">(b) By month of donation</td> </tr><tr>

<td><img width=500 src="../results/hb-levels-hour-Female.png"></td>
<td><img width=500 src="../results/hb-levels-hour-Male.png"></td> </tr><tr>
<td><img width=500 src="../results/hb-margins-hour-Female.png"></td>
<td><img width=500 src="../results/hb-margins-hour-Male.png"></td> </tr>

<td colspan="2">(c) By hour of donation</td>  </tr><tr>

</table>'

captions$figure.ml="<b>Figure 2</b> Levels and estimated deviations by (a) age, (b) month and (c) hour of donation, and sex 
(females on left, males on right). See legend for colours in top-left panel."

html.file=sub('¤table¤',paste(html.table.ml,if(include.captions) captions$figure.ml else '',sep='\n'),html.template)
# source('src/analysis-functions.r')
convertOutput(html.file,file=paste0(param$shared.dir,'figure-hb-2 levels margins.html'))

### heatmaps

# age month hour / levels and margins
html.table.h='<table><tr>
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

# New version, 2-wide
html.table.h='<table><tr>
<td><img width=500 src="../results/heatmap-Australia"></td>
<td><img width=500 src="../results/heatmap-Finland"></td> </tr><tr>

<td>(a)&nbsp;Australia</td>
<td>(b)&nbsp;Finland</td> </tr><tr>

<td><img width=500 src="../results/heatmap-Netherlands"></td>
<td><img width=500 src="../results/heatmap-Navarre"></td> </tr><tr>

<td>(c)&nbsp;Netherlands</td>
<td>(d)&nbsp;Navarre</td> </tr><tr>

<td><img width=500 src="../results/heatmap-France.png"></td>
<td></td> </tr><tr>

<td>(e)&nbsp;France</td>
<td></td> </tr><tr>
</table>'

captions$figure.h="<b>Figure 3</b> Heatmaps for the blood establishments, panels&nbsp;(a) through&nbsp;(e). Red tones 
indicate negative and blue tones positive corrections that are added to the mean hemoglobin values to 
achieve corrected hemoglobin values. All units in g/L. The heatmaps show that overall, largest corrections 
are due to age and changes in the age distribution over the years. The rectification of distributions produces 
noticable but rather constant corrections (where applicable)."

html.file=sub('¤table¤',paste(html.table.h,if(include.captions) captions$figure.h else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-hb-3 heatmaps.html'))

# D-figures

html.table.d='<table><tr>
<td><img width=1800 src="../results/dist-heatmap-age-Australia"></td> </tr><tr>
<td>(a) Australia</td> </tr><tr>
<td><img width=1800 src="../results/dist-heatmap-age-Finland"></td> </tr><tr>
<td>(b) Finland</td> </tr><tr>
<td><img width=1800 src="../results/dist-heatmap-age-France"></td> </tr><tr>
<td>(c) France</td> </tr><tr>
<td><img width=1800 src="../results/dist-heatmap-age-Navarre"></td> </tr><tr>
<td>(d) Navarre</td> </tr><tr>
<td><img width=1800 src="../results/dist-heatmap-age-Netherlands"></td> </tr>
<td>(e) Netherlands</td> </tr><tr>
</table>'

captions$figure.d="<b>Figure D</b> Heatmaps of the distribution of age by sex and year for the blood establishments, panels&nbsp;(a) through&nbsp;(e). 
Red tones is for females and blue tones for males. Darker tones imply high number of donations."

html.file=sub('¤table¤',paste(html.table.d,if(include.captions) captions$figure.d else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-d heatmaps-by-age.html'))

html.table.d2='<table><tr>
<td><img width=1800 src="../results/dist-heatmap-hour-Finland"></td> </tr><tr>
<td>(a) Finland</td> </tr><tr>
<td><img width=1800 src="../results/dist-heatmap-hour-Navarre"></td> </tr><tr>
<td>(b) Navarre</td> </tr><tr>
<td><img width=1800 src="../results/dist-heatmap-hour-Netherlands"></td> </tr>
<td>(c) Netherlands</td> </tr><tr>
</table>'

captions$figure.d2="<b>Figure D2</b> Heatmaps of the distribution of hour of donation by sex and year for the blood establishments, panels&nbsp;(a) through&nbsp;(e). 
Red tones is for females and blue tones for males. Darker tones imply high number of donations."

html.file=sub('¤table¤',paste(html.table.d2,if(include.captions) captions$figure.d2 else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-d2 heatmaps-by-hour.html'))

#### trends

# age month hour / levels and margins
html.table.t='<table><tr>
<td><img width=1800 src="../results/trends-corrected.pdf"></td> </tr>
</table>'

captions$figure.t="<b>Figure 4</b> Mean (solid lines) and corrected (dashed lines) hemoglobin levels. <br>Fitted trend lines (solid and straight) have been added where there is a statistically significant trend in the corrected data."

html.file=sub('¤table¤',paste(html.table.t,if(include.captions) captions$figure.t else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-hb-4 trends.html'))

# example rectification
html.table.r='<table><tr>
<td><img width=900 src="../results/rectify-distribution-sample.pdf"></td> </tr>
</table>'

captions$figure.r="<b>Figure 1</b> Example of hemoglobin distribution: Finland, year 2013, females. 
The black line shows the original distribution with an artefact caused by remeasurement after an 
initial measurement below the cutoff value (125 g/L, green vertical line). 
The post-rectification distribution is drawn in red. The mean values of the original and rectified 
distributions are illustrated with black and red, respectively, vertical dashed lines. 
The normal distribution following the mean and standard deviation of the rectified distribution is 
illustrated with red, dotted line."

html.file=sub('¤table¤',paste(html.table.r,if(include.captions) captions$figure.r else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-hb-1 rectification.html'),page.width=10)

#### Table 1
table1=annual.hb %>%
	filter(data.set=='donation0') %>%
	group_by(country,year) %>%
	summarise(n=sum(n2)/1000,.groups='drop') %>%
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
# cat(html.file,file=paste0(param$shared.dir,'table-1.html'))
# 2026-08-22 This is not used anymore

##### survival

# relative curves

html.table.s='<table><tr>
<td><img width=500 src="../results/survival-joint-ord.group.full-Female-NA.png"></td>
<td><img width=500 src="../results/survival-joint-ord.group.full-Male-NA.png"></td> </tr><tr>

<td>(a)&nbsp;Relative to first donation, females</td>
<td>(b)&nbsp;Relative to first donation, males</td> </tr><tr>

<td><img width=500 src="../results/survival-joint-bloodgr-Female-NA.png"></td>
<td><img width=500 src="../results/survival-joint-bloodgr-Male-NA.png"></td> </tr><tr>

<td>(c)&nbsp;O- donor relative to other, females</td>
<td>(d)&nbsp;O- donor relative to other, males</td> </tr><tr>

<td><img width=500 src="../results/survival-joint-age.group.t-Female--15-20-.png"></td>
<td><img width=500 src="../results/survival-joint-age.group.t-Male--15-20-.png"></td> </tr><tr>

<td>(e)&nbsp;Donors up to 20 vs. 41 to 45 years of age, females</td>
<td>(f)&nbsp;Donors up to 20 vs. 41 to 45 years of age, males</td> </tr><tr>

<td><img width=500 src="../results/survival-joint-hb.surplus-Female-bottom-10-.png"></td>
<td><img width=500 src="../results/survival-joint-hb.surplus-Male-bottom-10-.png"></td> </tr><tr>

<td>(g)&nbsp;Bottom decile of hemoglobin, females</td>
<td>(h)&nbsp;Bottom decile of hemoglobin, males</td> </tr><tr>

<td><img width=500 src="../results/survival-joint-sex-Female-NA.png"></td>
<td><img width=500 src="../results/survival-sample-age.group.t-fi-female.png"></td> </tr>

<td>(i)Males with females as reference group</td>
<td>(j)Relative to donors of 41 to 45 years of age</td> </tr><tr>

<td><img width=500 src="../results/survival-cn0-Female.png"></td>
<td><img width=500 src="../results/survival-cn0-Male.png"></td> </tr>

<td>(k)Relative to Australia, females</td>
<td>(l)Relative to Australia, males</td> </tr><tr>

</table>'

# <td><img width=500 src="../results/survival-joint-hb.surplus-Female-bottom-10-.png"></td>
# <td><img width=500 src="../results/survival-joint-hb.surplus-Male-bottom-10-.png"></td> </tr><tr>
# <td>(i)</td>
# <td>(j)</td> </tr><tr>

captions$figure.s="<b>Figure S</b>  by various various groups: (a, b)&nbsp;Relative likelikelihood
of next donation after the second etc. donation compared with after the first donation for females and males.
(c,d)&nbsp;Hazard/return ratio for females and males, respectively, for O negative blood group compared with all other blood groups as reference,
(e,f)&nbsp;Hazard/return for females and males, respectively, in age of at most 20 years at donation, 
compared with the reference age group of 41 to 45 years. 
(g,h)&nbsp;Similarly for bottom decile of hemoglobin surplus (excess to threshold) with the mid-50% fractile as reference.
(i)&nbsp;Similarly for males, with females as reference group, 
(j)&nbsp;example (Finnish females) of hazard/return ratios for age groups (at donation), with 41 to 45 as reference,
(k,l)&nbsp;hazard/return ratio for females and males, for different blood establishments (Australia as reference)"

html.file=sub('¤table¤',paste(html.table.s,if(include.captions) captions$figure.s else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-s relative survival.html'))

#### Figures S1 and S2
##### survival

# relative curves

html.table.s1='<table><tr>
<td><img width=500 src="../results/survival-joint-ord.group.full-Female-NA.png"></td>
<td><img width=500 src="../results/survival-joint-ord.group.full-Male-NA.png"></td> </tr><tr>

<td colspan="2">(a) Relative to first donation: females on left, males on right </td>  </tr><tr>

<td><img width=500 src="../results/survival-joint-bloodgr-Female-NA.png"></td>
<td><img width=500 src="../results/survival-joint-bloodgr-Male-NA.png"></td> </tr><tr>

<td colspan="2">(b) O- donors relative to other (non-O-) donors: females on left, males on right </td>  </tr><tr>

<td><img width=500 src="../results/survival-joint-age.group.t-Female--15-20-.png"></td>
<td><img width=500 src="../results/survival-joint-age.group.t-Male--15-20-.png"></td> </tr><tr>

<td colspan="2">(c) Donors up to 20 years of age relative to donors from 41 to 45 years of age: females on left, males on right </td>  </tr><tr>

<td><img width=500 src="../results/survival-joint-hb.surplus-Female-bottom-10-.png"></td>
<td><img width=500 src="../results/survival-joint-hb.surplus-Male-bottom-10-.png"></td> </tr><tr>

<td colspan="2">(d) Lowest 10% of hemoglobin compared with mid 50%: females on left, males on right </td>  </tr><tr>

</table>'

html.table.s2='<table><tr>
<td><img width=500 src="../results/survival-joint-sex-Female-NA.png"></td>
<td><img width=500 src="../results/survival-sample-age.group.t-fi-female.png"></td> </tr>

<td>(a) Males with females as reference group</td>
<td>(b) Relative to donors of 41 to 45 years of age</td> </tr><tr>

<td><img width=500 src="../results/survival-cn0-Female.png"></td>
<td><img width=500 src="../results/survival-cn0-Male.png"></td> </tr>

<td colspan="2">(c) Relative to Australia: females on left, males on right </td>  </tr><tr>

</table>'

# <td><img width=500 src="../results/survival-joint-hb.surplus-Female-bottom-10-.png"></td>
# <td><img width=500 src="../results/survival-joint-hb.surplus-Male-bottom-10-.png"></td> </tr><tr>
# <td>(i)</td>
# <td>(j)</td> </tr><tr>

captions$figure.s1="<b>Figure 1</b> Hazard/return ratios by various groupings as a function of the number of donation with 
confidence intervals (dashed). A value above the horizontal dashed line marking hazard/return ratio = 1 implies 
that the group is more likely to return than the reference group: as an example, a hazard/return ration 1.5 implies that the group is 50% more likely to return than the reference group. 
See panel legends for group variables and reference groups."

# (a)&nbsp;Hazard/return ratio after the second etc. donation compared with after the first donation for females and males.
# (b)&nbsp;Hazard/return ratio for females and males, respectively, for O negative blood group compared with all other blood groups as reference,
# (c)&nbsp;Hazard/return ratio for females and males, respectively, in age of at most 20 years at donation, 
# compared with the reference age group of 41 to 45 years. 
# (d)&nbsp;Similarly for bottom decile of hemoglobin surplus (excess to threshold) with the mid-50% fractile as reference.
# "

# captions$figure.s2="<b>Figure S2</b> (a)&nbsp;Similarly for males, with females as reference group, 
# (b)&nbsp;example (Finnish females) of hazard/return ratios for age groups (at donation), with 41 to 45 as reference,
# (c)&nbsp;hazard/return ratio for females and males, for different blood establishments (Australia as reference)"

captions$figure.s2="<b>Figure 2</b> Hazard/return ratios by various groupings as a function of the number of donation with 
confidence intervals (dashed). A value above the horizontal dashed line marking hazard/ratio ratio = 1 implies 
that the group is more likely to return than the reference group. See panel legends for more details."


html.file=sub('¤table¤',paste(html.table.s1,if(include.captions) captions$figure.s1 else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-surv-1 relative survival basic.html'))

html.file=sub('¤table¤',paste(html.table.s2,if(include.captions) captions$figure.s2 else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-surv-2 relative survival special.html'))

############### S1/S2

# curves and parameters

html.table.c='<table><tr>
<td><img width=500 src="../results/survival-curves-1-Female.png"></td>
<td><img width=500 src="../results/survival-curves-1-Male.png"></td> </tr><tr>

<td colspan="2">(a)&nbsp;Retention after the first donation, females (left) and males (right)</td>  </tr><tr>

<td><img width=500 src="../results/survival-curves-16-Female.png"></td>
<td><img width=500 src="../results/survival-curves-16-Male.png"></td> </tr><tr>

<td colspan="2">(b)&nbsp;Retention after 16 or more donations, females (left) and males (right)</td>  </tr><tr>

<td><img width=500 src="../results/survival-parameters-Female.png"></td>
<td><img width=500 src="../results/survival-parameters-Male.png"></td> </tr>

<td colspan="2">(c)&nbsp;Parameters estimated from the asymptotic regression model, females (left) and males (right)</td>  </tr><tr>

</table>'

frml.latex='$S(t)=a+(R_0–a)\\cdot \\exp\\{–\\exp(lrc)\\cdot \\sqrt{t}\\}$'
frml.html='S(t)=a+(R<sub>0</sub>–a)·exp{–exp(lrc)·sqrt(t)}'
captions$figure.c="<b>Figure 3</b> Survival as a function of time for (a)&nbsp;1 and 
(b)&nbsp;16 previous donations. Data for females on the left and males on the right. Top row: retention after
first donation. Second row: retention after 16 or more donations. Legend for colours in bottom row. While there are significant differences 
between blood establishments in retention after first donation, the differences tend to vanish with 
increasing number of donations. This phenomenon can also be seen from the parameter estimates at the bottom 
row, where the trajectories converge towards the bottom-right corner for all blood establishments. 
(c) Parameters estimated from the model ¤frml for each country, sex and number of previous donations. 
Lighter tones correspond to larger number of donations (<i>ord</i>). While the parameter estimates differ significantly 
at the early stages of donor careers, they converge towards the right-bottom corner at later stages."

html.file=sub('¤table¤',paste(html.table.c,if(include.captions) sub('¤frml',frml.latex,captions$figure.c,fixed=TRUE) else '',sep='\n'),html.template,fixed=TRUE)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-surv-3 curves.html'))
captions$figure.c=sub('¤frml',frml.html,captions$figure.c,fixed=TRUE)

# table 1 (for survival)
getCountriesStats = function(var,from.variable='countries.surv') {
	data.source=get(from.variable)
	stats.list=lapply(names(countries.surv),function(x) {
		data.source[[x]][[var]] %>%
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

st.hb=stats.age.t %>%
	group_by(country,sex,var) %>%
	filter(!is.na(age)) %>%
	summarise(value=sum(n*mean.hb)/sum(n),.groups='drop') %>%
	inner_join(conversions.df,join_by(country)) %>%
	mutate(var='Mean hemoglobin',value=round(value*rate,2)) %>%
	dplyr::select(-rate) %>%
	pivot_wider(names_from='country',values_from='value')

st.age0=stats.age %>%
	group_by(country,sex,var) %>%
	filter(!is.na(age)) %>%
	summarise(value=sum(n*age)/sum(n),.groups='drop') %>%
	mutate(var='Mean age at first donation',value=round(value,2)) %>%
	pivot_wider(names_from='country',values_from='value')

st.all=rbind(st.donor,st.donations,st.age.t,st.hb) %>%
	mutate(ord=row_number()) %>%
	arrange(sex,ord) %>%
	rowwise() %>%
	mutate(sex=if (ord > 2) '' else sex) %>%
	dplyr::select(-ord)
colnames(st.all)[-(1:2)]=sapply(colnames(st.all)[-(1:2)],function(x) cn.names[[x]])
colnames(st.all)[2]='Quantity'
colnames(st.all)=firstUp(colnames(st.all))
st.all

# Table 1 (for hb)
stats.annual.hb=getCountriesStats('annual.hb','countries') 
wh.age=which(colnames(stats.annual.hb)=='age')
colnames(stats.annual.hb)[wh.age]=c('mean.age','sd.age')
colnames(stats.annual.hb)=tolower(colnames(stats.annual.hb))
stats.annual.hb = stats.annual.hb %>% filter(data.set=='donation0')

stats.annual.age=getCountriesStats('annual.age','countries') %>% filter(data.set=='donation0')
colnames(stats.annual.age)=tolower(colnames(stats.annual.age))

st.donor.hb= stats.annual.hb %>%
	group_by(country,sex,var) %>%
	# filter(ord==1) %>%
	summarise(value=sum(n),.groups='drop') %>%
	mutate(var='Donors (in 1,000)',value=as.character(value/1000)) %>%
	pivot_wider(names_from='country',values_from='value')

st.age.hb=stats.annual.age %>%
	group_by(country,sex,var) %>%
	filter(!is.na(age)) %>%
	summarise(value=sum(n*age)/sum(n),.groups='drop') %>%
	mutate(var='Mean age at donation',value=round(value,2)) %>%
	pivot_wider(names_from='country',values_from='value')

st.hb.hb=stats.annual.hb %>%
	group_by(country,sex,var) %>%
	filter(abs(hb)<10000) %>%
	summarise(value=sum(n*hb)/sum(n),.groups='drop') %>%
	inner_join(conversions.df,join_by(country)) %>%
	mutate(var='Mean hemoglobin',value=round(value*rate,2)) %>%
	dplyr::select(-rate) %>%
	pivot_wider(names_from='country',values_from='value')

st.all.hb=rbind(st.donor.hb,st.age.hb,st.hb) %>%
	mutate(ord=row_number()) %>%
	arrange(sex,ord) %>%
	rowwise() %>%
	mutate(sex=if (ord > 2) '' else sex) %>%
	dplyr::select(-ord)
colnames(st.all.hb)[-(1:2)]=sapply(colnames(st.all.hb)[-(1:2)],function(x) cn.names[[x]])
colnames(st.all.hb)[2]='Quantity'
colnames(st.all.hb)=firstUp(colnames(st.all.hb))

html.table1.s=paste(capture.output(print(xtable(st.all,align=c('l','l',rep('r',ncol(st.all)-1))),type='html',include.rownames=FALSE)),collapse='\n')
html.table1.s=gsub('&amp;','&',html.table1.s)
caption='<b>Table 1</b> Descriptive statistics of study sample'
html.file.1s=sub('¤table¤',paste0(caption,'\n',html.table1.s),html.template)
cat(html.file.1s,file=paste0(param$shared.dir,'table-1 survival.html'))

html.table1.s=paste(capture.output(print(xtable(st.all.hb,align=c('l','l',rep('r',ncol(st.all.hb)-1))),type='html',include.rownames=FALSE)),collapse='\n')
html.table1.s=gsub('&amp;','&',html.table1.s)
caption='<b>Table 1</b> Descriptive statistics of study sample'
html.file.1s=sub('¤table¤',paste0(caption,'\n',html.table1.s),html.template)
cat(html.file.1s,file=paste0(param$shared.dir,'table-1 hb.html'))

# Numbers for abstracts
# hb
stats.annual.hb %>% summarise(sum(n))


stats.ord %>%
	# group_by(country,sex,var) %>%
	filter(ord==1) %>%
	summarise(value=max(n),.groups='drop')

stats.ord %>%
# 	group_by(country,sex,var) %>%
	summarise(value=sum(n),.groups='drop')

lapply(captions,function(x))
html.file.captions=sub('¤table¤',,html.template)
cat(html.file.1s,file=paste0(param$shared.dir,'table-1 survival.html'))
