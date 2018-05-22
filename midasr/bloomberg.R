library(Rblpapi)
con <- blpConnect(default = TRUE)     # automatic if option("blpAutoConnect") is TRUE

gdp <- bdh(securities = "TUGPCOQQ Index", 
            fields = "PX_LAST", 
           start.date = as.Date("2003-03-01"))

spx_dollar <- bdh(securities = "USDTRY BGN Curncy", 
           fields = "PX_LAST", 
           start.date = as.Date("2003-03-01"))

spx_euro <- bdh(securities = "EURTRY BGN Curncy", 
                  fields = "PX_LAST", 
                  start.date = as.Date("2003-03-01"))

unemployment <- bdh(securities = "TUUNR Index", 
                fields = "PX_LAST", 
                start.date = as.Date("2003-03-01"))

bist100 <- bdh(securities = "XU100 Index", 
                  fields = "PX_LAST", 
                  start.date = as.Date("2003-03-01"))

consumer_confidence <- bdh(securities = "TUCDCONF Index", 
                      fields = "PX_LAST", 
                      start.date = as.Date("2003-03-01"))

total_export <- bdh(securities = "TUTBEX Index", 
                                fields = "PX_LAST", 
                                start.date = as.Date("2003-03-01"))

total_import <- bdh(securities = "TUTBIM Index", 
                    fields = "PX_LAST", 
                    start.date = as.Date("2003-03-01"))

oil_gas <- bdh(securities = "TKEYTFUO Index", 
                     fields = "PX_LAST", 
                     start.date = as.Date("2003-03-01"))

industrial_production <- bdh(securities = "TUIOWD Index", 
                   fields = "PX_LAST", 
                   start.date = as.Date("2003-03-01"))
# spx_ndx <- bdh(securities = c("SPX Index","NDX Index"), 
#                fields = "PX_LAST",
#                start.date = as.Date("2013-03-01"), 
#                include.non.trading.days = TRUE)
# 
# monthlyOption <- structure(c("ACTUAL", "MONTHLY"),
#                             names = c("periodicityAdjustment",
#                                       "periodicitySelection"))
# 
# spx_ndx_monthly <- bdh(securities = c("TUUNR Index"), 
#                        fields = "PX_LAST",
#                        start.date = as.Date("2005-01-31"), 
#                        options = monthlyOption)
# 
# goog_ge_div <- bdh(securities = c("GOOG Equity","GE Equity"),
#                    fields = c("PX_LAST","CF_DVD_PAID"), 
#                    start.date = as.Date("2012-11-01"))
# 
# goog_ge_px <- bdp(securities = c("GOOG Equity","GE Equity"),
#                   fields = c("PX_LAST","DS002"))
