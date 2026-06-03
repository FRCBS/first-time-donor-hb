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
convertOutput(html.file,file=paste0(param$shared.dir,'figure-ml model specifications.html'))

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
html.table.ml='<table><tr>
<td><img width=1800 src="../results/trends-corrected"></td> </tr>
</table>'

captions$figure.t="<b>Figure T</b> Mean and corrected hemoglobin levels. <br>Fitted trend lines have been added where there is a statistically significant trend."

html.file=sub('¤table¤',paste(html.table.ml,if(include.captions) captions$figure.t else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-t trends.html'))
