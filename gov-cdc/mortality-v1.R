##########################################################################################################
##########################################################################################################
# Parsing some CDC Mortality Data
##########################################################################################################
##########################################################################################################
# yeah. 
##########################################################################################################
##########################################################################################################
setwd('~/Desktop/website/gov-cdc/')

# For fuck's sake:
fuckery = system('ls ../../PREMISE/indices/premise.R/R', intern=T)
for(fuck in fuckery) source(paste0('../../PREMISE/indices/premise.R/R/', fuck))
source('../../PREMISE/Hillary_Premise/package/h.get.data/R/functions-get-data.R')
source('../../PREMISE/Hillary_Premise/package/h.utils/R/functions-utils.R')
source('../../PREMISE/Hillary_Premise/package/h.premise.hierarchies/R/functions-hierarchies.R')
source('../../PREMISE/Hillary_Premise/package/h.timeseries//R/functions-timeseries.R')
source('../../PREMISE/Hillary_Premise/utils/mini_functions_time_series.R')
# library(stringdist)
# library(RCurl)
##########################################################################################################
##########################################################################################################

x = read.delim2(file='data/Mortality Data - CDC.txt', stringsAsFactors=F)
colnames(x) = tolower(colnames(x))
colnames(x) = gsub('..', '.', fixed=T, x=colnames(x))

race = 'White'
gender = 'Female'

races = unique(x$race)
genders = unique(x$gender)
races = races[races!='']
genders = genders[genders!='']

for(race in races){
  for(gender in genders){
    
    
    intents = unique(x$injury.intent)
    ages = unique(x$age.group)
    
    x$crude.rate = as.numeric(trim(gsub(' (Unreliable)', '', x$crude.rate, fixed=T)))
    ages = ages[ages!='']
    intents = intents[intents!='']
    cols = get.colors(length(intents), opacity='cc')
    plot(c(0, 45), c(0, sqrt(150)), cex=0, xaxt='n', yaxt='n', cex.axis=.75, bty='n', xlab='', ylab='')
    for(i in 1:length(intents)){
      intent = intents[i]
      y = x[x$injury.intent==intent, ]
      y = y[y$race==race, ]
      y = y[y$gender==gender, ]
      for(age in ages){
        if(age=='< 1 year'){
          age1 = 0; age2 = .25
        } else {
          age1 = as.numeric(strsplit(age, '-')[[1]][1])
          age2 = as.numeric(strsplit(sub(' years', '', age), '-')[[1]][2])
        }
        rate = sum(y$crude.rate[y$age.group==age])
        
        lines(c(age1, age2), sqrt(c(rate, rate)), col=cols[i], lwd=6)
        if(age!='< 1 year'){
          text(mean(c(age1, age2)), -.3, labels=paste0(age1, ' - ', age2), xpd=T, cex=.7)
        } else {
          text(0, -.3, '<1', xpd=T, cex=.7)
        }
      }
    }
    legend('topleft', bty='n', lwd=6, cex=.8, intents, col=cols)
    title(main=paste0(race, ' ', gender, ' (USA), 2002-2011'))
    title(ylab='Rate per 100,000')
    at = c(0, 1, 3, 5, 10, 20, 30, 45, 60, 80, 100, 125, 150)
    axis(side=2, at=sqrt(at), labels=at, cex.axis=.75)
    
    
  }
}

# now plot each by race and age. 

colnames(x) = gsub('.', '_', colnames(x), fixed=T)
write.csv(x, 'data/Mortality Data - CDC.csv')




