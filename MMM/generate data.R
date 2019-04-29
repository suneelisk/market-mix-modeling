#===============================  1111================

adstock_calc_1<-function(media_var,dec,dim){
  length<-length(media_var)
  adstock<-rep(0,length)
  for(i in 2:length){
    adstock[i]<-(1-exp(-media_var[i]/dim)+dec*adstock[i-1])
  }
  adstock
}
adstock_calc_2<-function(media_var,dec,dim){
  length<-length(media_var)
  adstock<-rep(0,length)
  for(i in 2:length){
    adstock[i]<-1-exp(-(media_var[i]+dec*media_var[i-1])/dim)
  }
  adstock
}
#Function for creating test sets
create_test_sets<-function(base_p, trend_p, season_p, ad_p, dim, dec, adstock_form, error_std){
  #National level model
  #Five years of weekly data
  week<-1:(5*52)
  #Base sales of base_p units
  base<-rep(base_p,5*52)
  #Trend of trend_p extra units per week
  trend<-trend_p*week
  #Winter is season_p*10 units below, summer is season_p*10 units above
  temp<-10*sin(week*3.14/26)
  seasonality<-season_p*temp
  #7 TV campaigns. Carry over is dec, theta is dim, beta is ad_p,
  tv_grps<-rep(0,5*52)
  tv_grps[20:25]<-c(390,250,100,80,120,60)
  tv_grps[60:65]<-c(250,220,100,100,120,120)
  tv_grps[100:103]<-c(100,80,60,100)
  tv_grps[150:155]<-c(500,200,200,100,120,120)
  tv_grps[200:205]<-c(250,120,200,100,120,120)
  tv_grps[220:223]<-c(100,100,80,60)
  tv_grps[240:245]<-c(350,290,100,100,120,120)
  if (adstock_form==2){adstock<-adstock_calc_2(tv_grps, dec, dim)}
  else {adstock<-adstock_calc_1(tv_grps, dec, dim)}
  TV<-ad_p*adstock
  #Error has a std of error_var
  error<-rnorm(5*52, mean=0, sd=error_std)
  #Full series
  sales<-base+trend+seasonality+TV+error
  #Plot
  #plot(sales, type='l', ylim=c(0,1200))
  output<-data.frame(sales, temp, tv_grps, week, adstock)
  output
}
create_test_sets(2,3,4,5,6,7,2,4)

test <- create_test_sets(base_p=1000,trend_p=0.8,season_p=4,ad_p=30,dim=100,dec=0.3,
                          adstock_form=1,
                          error_std=5)
write.csv(test,file = "generate_data.csv")



#================================2222==================

generateCampaigns<-function(n=5, l=30, fromDate=Sys.Date()-364, toDate=Sys.Date()){
 dateseq <- seq(fromDate, toDate, by="1 day")
numPossibleCampaigns<-as.integer(length(dateseq)/l)
numPossibleCampaigns
}

#===========================
generateFromFunction <- function(f, fromDate = Sys.Date() - 1 * 365,
                                 toDate = Sys.Date(),
                                 mynames = c('something', 'something_else')) {
  tmpdf <- tibble::tibble(date = seq(fromDate, toDate, by = "1 day"))
  tmpdf <- data.frame(tmpdf, sapply(mynames, f)) %>% tibble::as_tibble()
  tmpdf
}

#==============================
generateWeatherData <- function(fromDate = Sys.Date() - 1 * 365,
                                toDate = Sys.Date(),
                                mynames = c('sunshine', 'precipitation', 'temperature')) {
  arf <- function(x) as.vector(stats::arima.sim(list(order = c(1, 0, 0), ar = 0.7), n = as.integer(toDate-fromDate)+1))
  generateFromFunction(arf, fromDate = fromDate, toDate = toDate, mynames = mynames)
}

#===============================
generatePriceData <- function(fromDate = Sys.Date() - 1 * 365,
                              toDate = Sys.Date(),
                              mynames = c('product_a', 'product_b', 'product_c')) {
  arf <- function(x) {
    # Initialise HMM
    hmm = HMM::initHMM(
      c("PriceWar", "Normal"),
      c("PriceA", "PriceB", "PriceC", "PriceD"),
      transProbs = matrix(c(.8, .2,
                            .2, .8), 2),
      emissionProbs = matrix(c(.3, .6,
                               .2, .2,
                               .3, .1,
                               .2, .1), 4)
    )
    tmptypedf <-
      tibble::tibble(type = HMM::simHMM(hmm, as.integer(toDate - fromDate) +
                                          1)$observation)
    tmppricedf <-
      tibble::tibble(
        type = c("PriceA", "PriceB", "PriceC", "PriceD"),
        price = c(199, 149, 129, 99)
      )
    tmpdf <- dplyr::left_join(tmptypedf, tmppricedf, by = "type")
    as.vector(tmpdf$price)
  }
  generateFromFunction(arf,
                       fromDate = fromDate,
                       toDate = toDate,
                       mynames = mynames)
}

#===========================
  generateDistributionData <- function(fromDate = Sys.Date() - 1 * 365,
                                    toDate = Sys.Date(),
                                    mynames = c('product_a', 'product_b', 'product_c'),
                                    initDist = c(1000, 500, 800)) {
  arf <- function(x)  (rpois(as.integer(toDate-fromDate)+1, 2)/10+1)/2
  arf <- function(x) {
    # Initialise HMM
    hmm = HMM::initHMM(
      c("Normal", "Turbulent"),
      c("Increase", "Decrease", "Stay"),
      transProbs = matrix(c(.95, .95,
                            .05, .05), 2),
      emissionProbs = matrix(c(.05, .4,
                               .05, .2,
                               .90, .4), 3)
    )
    tmptypedf <-
      tibble::tibble(type = HMM::simHMM(hmm, as.integer(toDate - fromDate) +
                                          1)$observation)
    tmppricedf <-
      tibble::tibble(
        type = c("Increase", "Decrease", "Stay"),
        price = c(1, -1, 0)
      )
    tmpdf <- dplyr::left_join(tmptypedf, tmppricedf, by = "type")
    cumsum(as.vector(tmpdf$price))
  }
  # dautility::qplotez((rpois(10, 2)/10+1)/2)
  a<-generateFromFunction(arf,
                          fromDate = fromDate,
                          toDate = toDate,
                          mynames = mynames)
  a[,-1] <- t(t(a[,-1])+initDist)
  a
}

  #==================
  generateMacroData <- function(fromDate = Sys.Date() - 1 * 365,
                                toDate = Sys.Date(),
                                mynames = c('cpi', 'cci', 'gdp')) {
    arf <- function(x) as.vector(stats::arima.sim(list(order = c(1, 1, 0), ar = 0.7), n = as.integer(toDate-fromDate)))
    generateFromFunction(arf, fromDate = fromDate, toDate = toDate, mynames = mynames)
  }

  #=====================
  generateCompetitorData <- function(fromDate = Sys.Date() - 1 * 365,
                                     toDate = Sys.Date(),
                                     mynames = c('competitor_a', 'competitor_b', 'competitor_c')) {
    arf <- function(x) sample(c(0, seq(10000, 100000, 10000)), as.integer(toDate-fromDate)+1, replace = TRUE)
    generateFromFunction(arf, fromDate = fromDate, toDate = toDate, mynames = mynames)
  }  

  #===================
  generateOnlineData <- function(fromDate = Sys.Date() - 1 * 365,
                                 toDate = Sys.Date(),
                                 mynames = c('display', 'facebook', 'search_branded'),
                                 avgcpm = 0.5, avgnet = 10000) {
    genCampaignStructure <- function(n){
      hmm = HMM::initHMM(
        c("Burst", "Normal", "Off"),
        c("PriceA", "PriceB"),
        transProbs = matrix(c(.7, .05, .1,
                              .1, .9, .1,
                              .2, .05, .8), 3),
        emissionProbs = matrix(c(.3, .7, .5,
                                 .7, .3, .5), 2)
      )
      HMM::simHMM(hmm, n)$states
    }
    
    arf <- function(x) {
      cpms <- c(seq(0.1*avgcpm, 2.5*avgcpm, length.out = 11))
      pricenames <- paste0("Price", LETTERS[1:length(cpms)])
      tmppricedf <- tibble::tibble(cpm = cpms, type = pricenames)
      # Initialise HMM
      hmm = HMM::initHMM(
        c("High", "Low"),
        pricenames,
        transProbs = matrix(c(.7, .3,
                              .3, .7), 2),
        emissionProbs = matrix(c(.0, .3,
                                 .0, .1,
                                 .0, .2,
                                 .2, .2,
                                 .2, .2,
                                 .2, .0,
                                 .1, .0,
                                 .1, .0,
                                 .1, .0,
                                 .05, .0,
                                 .05, 0), 11)
      )
      tmptypedf <-
        tibble::tibble(type = HMM::simHMM(hmm, as.integer(toDate - fromDate) +
                                            1)$observation)
      tmpdf <- dplyr::left_join(tmptypedf, tmppricedf, by = "type")
      as.vector(tmpdf$cpm)
    }
    cpmdf <- generateFromFunction(arf, fromDate = fromDate, toDate = toDate, mynames = mynames)
    campdf <- dplyr::left_join(tibble::tibble(strategy=genCampaignStructure(as.integer(toDate - fromDate) + 1)),
                               tibble::tibble(strategy=c("Burst", "Normal", "Off"), net=c(2.5*avgnet, avgnet, 0)), by="strategy")
    stopifnot(nrow(cpmdf)==nrow(campdf))
    # genspend <- function(x) rnorm(length(x), 1, 1/5)*x
    genimp <- function(x) rnorm(length(x), 1, 1/5)*campdf$net/x*1000
    impdf <- tibble::as_tibble(data.frame(date=cpmdf$date, sapply(dplyr::select(cpmdf, -date), genimp)))
    netdf <- tibble::as_tibble(data.frame(date=impdf$date, dplyr::select(impdf, -date)/1000 * dplyr::select(cpmdf, -date)))
    list(net=netdf, impression=impdf, cpm=cpmdf)
  }  


#=============================
generateOfflineData <- function(fromDate = Sys.Date() - 1 * 365,
                                toDate = Sys.Date(),
                                mynames = c('tv', 'radio', 'ooh'),
                                avgcpm = 0.5, avgnet = 10000) {
  genCampaignStructure <- function(n){
    hmm = HMM::initHMM(
      c("Burst", "Normal", "Off"),
      c("PriceA", "PriceB"),
      transProbs = matrix(c(.7, .05, .1,
                            .1, .9, .1,
                            .2, .05, .8), 3),
      emissionProbs = matrix(c(.3, .7, .5,
                               .7, .3, .5), 2)
    )
    HMM::simHMM(hmm, n)$states
  }
  
  arf <- function(x) {
    cpms <- c(seq(0.1*avgcpm, 2.5*avgcpm, length.out = 11))
    pricenames <- paste0("Price", LETTERS[1:length(cpms)])
    tmppricedf <- tibble::tibble(cpm = cpms, type = pricenames)
    # Initialise HMM
    hmm = HMM::initHMM(
      c("High", "Low"),
      pricenames,
      transProbs = matrix(c(.7, .3,
                            .3, .7), 2),
      emissionProbs = matrix(c(.0, .3,
                               .0, .1,
                               .0, .2,
                               .2, .2,
                               .2, .2,
                               .2, .0,
                               .1, .0,
                               .1, .0,
                               .1, .0,
                               .05, .0,
                               .05, 0), 11)
    )
    tmptypedf <-
      tibble::tibble(type = HMM::simHMM(hmm, as.integer(toDate - fromDate) +
                                          1)$observation)
    tmpdf <- dplyr::left_join(tmptypedf, tmppricedf, by = "type")
    as.vector(tmpdf$cpm)
  }
  cpmdf <- generateFromFunction(arf, fromDate = fromDate, toDate = toDate, mynames = mynames)
  campdf <- dplyr::left_join(tibble::tibble(strategy=genCampaignStructure(as.integer(toDate - fromDate) + 1)),
                             tibble::tibble(strategy=c("Burst", "Normal", "Off"), net=c(2.5*avgnet, avgnet, 0)), by="strategy")
  stopifnot(nrow(cpmdf)==nrow(campdf))
  # genspend <- function(x) rnorm(length(x), 1, 1/5)*x
  genimp <- function(x) rnorm(length(x), 1, 1/5)*campdf$net/x*1000
  impdf <- tibble::as_tibble(data.frame(date=cpmdf$date, sapply(dplyr::select(cpmdf, -date), genimp)))
  netdf <- tibble::as_tibble(data.frame(date=impdf$date, dplyr::select(impdf, -date)/1000 * dplyr::select(cpmdf, -date)))
  list(net=netdf, impression=impdf, cpm=cpmdf)
}


#============================
generateEventData <- function(fromDate = Sys.Date() - 1 * 365,
                              toDate = Sys.Date(),
                              mynames = c('event_a', 'event_b'),
                              freq = 0.01) {
  arf <- function(x) rpois(as.integer(toDate-fromDate)+1, freq)
  generateFromFunction(arf, fromDate = fromDate, toDate = toDate, mynames = mynames)
}


#================================
generateData <-
  function(fromDate=Sys.Date()-1*365,
           toDate=Sys.Date(),
           kpi='acquisitions',
           sector='telekom',
           onlineInsertionNames=c('display', 'facebook', 'search_branded'),
           offlineInsertionNames=c('tv', 'radio', 'ooh', 'print'),
           priceNames=c('product_a', 'product_b', 'product_c'),
           distributionNames=c('product_a', 'product_b', 'product_c'),
           weatherNames=c('sunshine', 'precipitation', 'temperature'),
           competitorNames=c('competitor_a', 'competitor_b', 'competitor_c'),
           macroNames=c('cpi', 'cci', 'gdp'),
           eventNames=c('event_a', 'event_b')) {
    
    mydf <- tibble::tibble(date=seq(fromDate, toDate, by="1 day"))
    
    # These come as list of three tibbles.
    ondf <- generateOnlineData(fromDate, toDate, onlineInsertionNames)
    ofdf <- generateOfflineData(fromDate, toDate, offlineInsertionNames)
    # These come as pure tibbles
    prdf <- generatePriceData(fromDate, toDate, priceNames)
    didf <- generateDistributionData(fromDate, toDate, distributionNames)
    wedf <- generateWeatherData(fromDate, toDate, weatherNames)
    codf <- generateCompetitorData(fromDate, toDate, competitorNames)
    madf <- generateMacroData(fromDate, toDate, macroNames)
    # evdf <- generateEventData(fromDate, toDate, eventNames)
    
    mydf <- Reduce(function(x, y) dplyr::inner_join(x,y, by = "date"), list(mydf, wedf, codf, madf, didf, prdf))
    mydf
  }
