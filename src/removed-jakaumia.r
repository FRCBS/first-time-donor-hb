```{r}
# Jakaumia
dist = donation0 %>%
  filter(!is.na(Hb)) %>%
  mutate(Hb=pmin(pmax(Hb,100),200)) %>%
  group_by(year,Sex,Hb) %>%
  summarise(n=n(),.groups='drop')

dist$Hb=as.integer(dist$Hb)
dist$year=as.integer(dist$year)
dist.male = as.data.frame(dist[dist$Sex=='Male',!colnames(dist) %in% c('Sex')])
tab.male = as.data.frame(pivot_wider(dist.male,names_from='Hb',values_from='n',values_fill=0,names_sort=TRUE))
rownames(tab.male)=tab.male$year
tab.male=tab.male[,-1]
tab.male
row.sums = apply(tab.male,1,sum,na.rm=TRUE)
prop.male = tab.male / row.sums
# prop.male[is.na(prop.male)] = 0
cuml.male = apply(prop.male,1,cumsum)
dim(cuml.male)
cuml.male[,1]

tab.male[1:5,1:5]
i=0
dim(cuml.male)
for (i in 0:4) {
  matplot(cuml.male[1:40,5*i+(1:5)],type='l',ylim=c(0,0.1),main=paste(5*i+(1:5),collapse=' '))
}
lines(1:ncol(cuml.male),cuml.male['135',])

colnames(cuml.male)
cuml.male['135',]



matplot(cuml.male[1:40,5*i+(1:5)],type='l',ylim=c(0,0.2))
```


# 2025-04-23

```{r basics-processing}

# Mitä tietoja oikeastaan tarvittaisiin: jakauma itsessään(~), mean1, sd1, error (suhde normaalijakaumaan), korjauksen summa
# Jos ei plottaa, tämän saa kätevästi apply:lla (rivinumerot jos ei muu auta, kaivellaan sitten funktiossa esiin)

# pitäisikö käyttää myös donation.r erikseen? 
# Voisi laskea kokeeksi siihenkin

# simple$deferred[simple$Sex=='Female'&simple$Hb<125]=1
# simple$deferred[simple$Sex=='Male'&simple$Hb<135]=1

new.basics = getStats(hb.freq,'Hb',age.freq=age.freq %>% filter(data.set=='donation0'))

basics = donation0 %>% # simple
  filter(!is.na(Hb)) %>%
  group_by(year,Sex) %>%
  summarise(n=n(),mean0=mean(Hb,na.rm=TRUE),sd0=sd(Hb,na.rm=TRUE),deferred.prop=sum(deferred)/n(),
            limit=min(limit),age=mean(age),.groups='drop')

basics.all = simple %>% # simple
  filter(!is.na(Hb)) %>%
  group_by(year,Sex) %>%
  summarise(n=n(),mean.year=mean(Hb,na.rm=TRUE),sd0=sd(Hb,na.rm=TRUE),deferred.prop=sum(deferred)/n(),limit=min(limit),.groups='drop')

true.basics = simple %>%
  filter(!is.na(Hb)) %>%
  group_by(Sex) %>%
  summarise(n=n(),mean.all=mean(Hb,na.rm=TRUE),sd0=sd(Hb,na.rm=TRUE),deferred.prop=sum(deferred)/n(),limit=min(limit),.groups='drop')
true.basics$z=with(true.basics,(limit-mean.all)/sd0)
true.basics=data.frame(true.basics)
rownames(true.basics)=true.basics$Sex
true.basics$comp=true.basics$z[c(2,1)]*true.basics$sd0+true.basics$mean.all

true.basics$comp

basics = basics %>%
  inner_join(true.basics[,c('Sex','mean.all')],join_by(Sex)) %>%
  inner_join(basics.all[,c('Sex','mean.year','year')],join_by(Sex,year))

basics$mean1=NA
basics$sd1=NA

# Compute the adjusted means and standard deviations using rectified distributions
for (i in 1:nrow(basics)) {
  # nb! This used to be simple, eg. all donations were considered
  # Maybe should add them back
  # Probably best would be to add columns to distinguish between first time and repeat donors
  # Make a function to compile these stats for different subsets of donations: 1) first-time 2) repeat 3) all
  # Then see, what is useful
  # Probably should also convert the different mean etc. to columns, or should this be in fully long form? Maybe a mix
  rv = rectifyDistribution(donation0[donation0$year==basics$year[i],],basics$Sex[i],if(basics$Sex[i]=='Male') 135 else 125,FALSE)
  basics$mean1[i] = rv$mean1
  basics$sd1[i] = rv$sd1
}

basics$dperc0=pnorm(basics$limit,basics$mean0,basics$sd0)

# r.mat.long=pivot_longer(r.mat,colnames(r.mat),names_to='year')
# lm-adjusted means (year x sex)
# lm.data=pivot_longer(cbind(Sex=rownames(r.mat),r.mat)[1:2,],colnames(r.mat),names_to='year',values_to='mean.lm')
# lm.data$year=as.integer(lm.data$year)
# basics=left_join(basics,lm.data,join_by(year,Sex))
# basics$mean.lm[is.na(basics$mean.lm)]=0

# Compute the first adjusted value (dperc.nrm) using the all time averages for Hb (grouped by sex)
basics$dperc.nrm=pnorm(basics$limit+(basics$mean.year-basics$mean.all),basics$mean0,basics$sd0)
# basics$dperc.lm=pnorm(basics$limit+basics$mean.lm,basics$mean0,basics$sd0)
# basics$dperc.nrm.1=pnorm(basics$limit+(basics$mean.year-basics$mean.all),basics$mean1,basics$sd1)

sex.col=data.frame(sex=c('Female','Male'),col=c('Red','Blue'))

# This all should be done more systematically
# Should also be able to look at repeat deferrals
# Given the major differences in sd's, maybe should adjust for them as well;
# or rather, in the theoretical computations, the adjustment is already made.

# For plotting, should adjust:
# - b-labels with correct units (or none at all)
# - should be able to index values to 1 (or 100), especially for means
# - Maybe even multiple variables in same chart to enable comparison (mean,sd,deferral%)
# - and later country

# If and equivalent deferral limit would be applied to men as for women (z-value), the limit would be 138.8
# Similarly for women, the deferrallimit would be 121.5
```

# Tässä itse asiassa lasketaan vuosikohtaisesti perusparametrit, erona vaan että luottamusvälit lisäksi
old.agedist = simple %>%
  mutate(age=as.integer(age)) %>%
  filter(!is.na(Hb),age<=70) %>%
  # nb! This is a pitfall, maybe should be done in other contexts as well for consistency
  # Maybe should manage this somehow differently
  filter(DonationPlaceType!='Garrison') %>% 
  group_by(age,Sex) %>%
  summarise(mean=mean(Hb),sd=sd(Hb,na.rm=TRUE),v=var(Hb),n=n(),.groups='drop') %>%
  mutate(sd.low=sqrt((n-1)*v/qchisq(c(.975), n-1))) %>%
  mutate(sd.hi =sqrt((n-1)*v/qchisq(c(.0275), n-1))) %>%
  mutate(mean.low=mean-qnorm(0.025)*sd/sqrt(n)) %>%
  mutate(mean.hi =mean-qnorm(0.975)*sd/sqrt(n)) %>%
  filter(n>100)
