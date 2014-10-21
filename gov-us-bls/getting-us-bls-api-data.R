##########################################################################################################
##########################################################################################################
# Getting and Parsing Data via the US-BLS's data API
##########################################################################################################
##########################################################################################################
# There are seven categories of data available via the API: (http://www.bls.gov/help/hlpforma.htm#CX)

# 1) Spending (consumer expentiture surveys)
# 2) Inflation & Prices (consumer price indices, producer price indices)
# 3) Pay & Benefits (employee benefits survey, employment cost index, national compensation, etc...)
# 4) Workplace Injuries
# 5) Productivity
# 6) International (import and export price indices)
# 7) Projections

# Each category has one or more sub-categories, each of which has its own set of item / area / etc... 
# codes that are needed to form a valid API query for data in that sub-category.

# Below, I've made a function for each category/sub-category that gathers code data, parses it, and
# allows the user to submit a query easily via R. 


### next up to do: fix http get via R studio. Maybe make some more of these functions and find something
### interesting. Even visualize the us CPI if that's most interesting vis d3!
### --> Visualize CSV data (or http? To slow) data with d3!

##########################################################################################################
##########################################################################################################
setwd('~/Desktop/website/us-bls/')

# For fuck's sake:
fuckery = system('ls ../../PREMISE/indices/premise.R/R', intern=T)
for(fuck in fuckery) source(paste0('../../PREMISE/indices/premise.R/R/', fuck))
source('../../PREMISE/Hillary_Premise/package/h.get.data/R/functions-get-data.R')
source('../../PREMISE/Hillary_Premise/package/h.utils/R/functions-utils.R')
source('../../PREMISE/Hillary_Premise/package/h.premise.hierarchies/R/functions-hierarchies.R')
source('../../PREMISE/Hillary_Premise/package/h.timeseries//R/functions-timeseries.R')
source('../../PREMISE/Hillary_Premise/utils/mini_functions_time_series.R')
library(stringdist)
library(RCurl)
##########################################################################################################
##########################################################################################################


get.bls.data.spending.consumer.expenditure <- function(params=list(
                                                         item='Total average annual expenditures',
                                                         demographics='Quintiles of income before taxes',
                                                         characteristics='All Consumer Units'),
                                                       seasonally.adjusted=FALSE,
                                                       verbose=T, viz.options=T){
  
  path = 'Spending/consumer-expenditure/'
  # load code dataframes into global environment: (!)
  codes = load.us.bls.codes(path=path, verbose=verbose) 
  # extract the text and code names by using user given params and the loaded US-BLS codes:
  extract.code.names(params=params)

  if(viz.options){
    # viz hierarchies!
  }
  

  #   Series ID    CXUMENBOYSLB0101M
  #   Positions       Value           Field Name
  #   1-2             CX              Prefix
  #   3               U               Seasonal Adjustment Code 
  #   4-11            MENBOYS         Item Code
  #   12-15	   	      LB01	          Demographics Code
  #   16-17           01	            Characteristics Code
  #   18              M               Process Code
  
  prefix = 'CX'
  seasonal.adjustment.code = c('U', 'S')[1+seasonally.adjusted]
  process.code = 'M'
  code <- paste0(prefix,
                  seasonal.adjustment.code,
                  item_code,
                  demographics_code,
                  characteristics_code,
                  process.code
                  )
  
  
  
  series = query.us.bls(code=code, file='us-bls.json')
  series$seasonally.adjusted = seasonally.adjusted
  series$item.code = item_code
  series$item.txt = item_text
  series$characteristics_code = characteristics_code
  series$characteristics_text = characteristics_text
  series$demographics_code = demographics_code
  series$demographics_text = demographics_text
  
  return(series)
  

}


extract.code.names <- function(params){
  #####################################################################################
  #   Get Codes:
  for(code.name in names(params)){
    code = params[[code.name]]
    x = eval(parse(text=code.name))
    if(!code %in% x[ , paste0(code.name, '_code')]){
      code = x[ , paste0(code.name, '_code')][which.min(stringdist(code, x[ , paste0(code.name, '_text')]))]
    } 
    code.txt =  x[ , paste0(code.name, '_text')][x[ , paste0(code.name, '_code')]==code]
    code.txt = names(sort(table(code.txt), d=T)[1]) #in case there's more than one...
    
    assign(paste0(code.name, '_code'), value=code, envir=.GlobalEnv)
    assign(paste0(code.name, '_text'), value=code.txt, envir=.GlobalEnv)
  }
}

query.us.bls <- function(code, get.data=T, verbose=T, file=NULL){
  
  
  # system2('http', stdin='/dev/null')
  foo = fromJSON(getURI('http://api.bls.gov/publicAPI/v1/timeseries/data/CXUTOTALEXPLB0101M'))
  status = foo$status
  catt(status)
  
  foo = foo$series
  foo = foo[[1]]
  foo = foo$data
  
    
    if(!is.null(foo)){
      names = unique(as.character(names(unlist(foo))))
      mat = matrix(NA, ncol=length(names), nrow=length(foo), dimnames=list(NULL, names))
      for(i in 1:length(foo)){
        idx = names(foo[[i]]) %in% names
        fooo = foo[[i]][idx]
        mat[i, names(fooo)] = as.character(fooo)
      }
      mat = fix.factors(as.data.frame(mat))
      if('price' %in% colnames(mat)) mat$price = as.numeric(mat$value)
      if('timestamp' %in% colnames(mat)) {
        mat$timestamp = as.Date(paste0(mat$year, tolower(substr(mat$periodName, 1, 3)),
                                         '01'), format='%Y%b%d')
        mat = mat[order(mat$timestamp), ]
      }
    }
  
  
  return(mat)
}



load.us.bls.codes <- function(path='Spending/consumer-expenditure/', verbose=TRUE){
  codes <- system(paste0('ls codes/', path), intern=T)
  codes = sub('.txt', '', fixed=T, x=codes)
  
  for(code.path in codes){
    x = read.delim2(paste0('codes/', path, code.path, '.txt'), row.names=NULL, stringsAsFactors=F)
    colnames(x) = c(colnames(x)[-1], 'foo')
    x$foo = NULL
    assign(code.path, value=x, envir=.GlobalEnv)
  }
  
  
  if(verbose){
    catt('\nThere are ', length(codes), ' variables in a ', gsub('/', ' ', path), 'API query:\n')
    for(i in length(codes):1){
      x = eval(parse(text=eval(parse(text=paste0('codes[', i, ']')))))
      x$end.node = tail(c(0, x$display_level) - c(x$display_level, 0)>=0, nrow(x))
      catt(codes[i], ': ', nrow(x), ' options, ', length(unique(x$display_level)), ' display levels, ', 
           sum(x$end.node), ' end nodes')
    }
  }
  
  return(codes)
}




