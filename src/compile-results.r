source('src/analysis-functions.r')

include.captions=TRUE
captions=list()

# age month hour / levels and margins
html.table.ml='<table><tr>
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

captions$figure.ml="<b>Figure ML</b> Levels and estimated deviations by age and sex"

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

captions$figure.h="<b>Figure H</b> Heatmaps for the countries"

html.file=sub('¤table¤',paste(html.table.ml,if(include.captions) captions$figure.h else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-h heatmaps.html'))

#### trends

# age month hour / levels and margins
html.table.t='<table><tr>
<td><img width=1800 src="../results/trends-corrected.pdf"></td> </tr>
</table>'

captions$figure.t="<b>Figure T</b> Mean and corrected hemoglobin levels. <br>Fitted trend lines have been added where there is a statistically significant trend."

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

<td><img width=500 src="../results/survival-joint-bloodgr-Female-NA.png"></td>
<td><img width=500 src="../results/survival-joint-bloodgr-Male-NA.png"></td> </tr><tr>

<td><img width=500 src="../results/survival-joint-age.group.t-Female--15-20-.png"></td>
<td><img width=500 src="../results/survival-joint-age.group.t-Male--15-20-.png"></td> </tr><tr>

<td><img width=500 src="../results/survival-joint-hb.surplus-Female-bottom-10-.png"></td>
<td><img width=500 src="../results/survival-joint-hb.surplus-Male-bottom-10-.png"></td> </tr><tr>

<td><img width=500 src="../results/survival-joint-hb.surplus-Female-bottom-10-.png"></td>
<td><img width=500 src="../results/survival-joint-hb.surplus-Male-bottom-10-.png"></td> </tr><tr>

<td><img width=500 src="../results/survival-joint-sex-Female-NA.png"></td>
<td><img width=500 src="../results/survival-sample-age.group.t-fi-female.png"></td> </tr>

</table>'

captions$figure.s="<b>Figure S</b> Relative retention by various various groups: (a) ..."

html.file=sub('¤table¤',paste(html.table.s,if(include.captions) captions$figure.s else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-s relative survival.html'))

# curves

html.table.c='<table><tr>
<td><img width=500 src="../results/survival-curves-1-Female.png"></td>
<td><img width=500 src="../results/survival-curves-1-Male.png"></td> </tr><tr>

<td><img width=500 src="../results/survival-curves-16-Female.png"></td>
<td><img width=500 src="../results/survival-curves-16-Male.png"></td> </tr><tr>

<td><img width=500 src="../results/survival-parameters.png"></td>

</table>'

captions$figure.c="<b>Figure C</b> Survival as a function of time for (a)&nbsp;1 and (b)&nbsp;16 previous donations. "

html.file=sub('¤table¤',paste(html.table.c,if(include.captions) captions$figure.c else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-c curves.html'))
