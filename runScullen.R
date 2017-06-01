### Independent Variables ###

perFired <- c(.05,.10) # fired per subgroup rather than across whole organization

rel <- c(.5, .7) # considered lower bound; inter-rater reliability

val <- c(.3, .5)

sr <- c(.10, .33) # variability in samples

turnOver <- c(0, .05, .1, .15, .2)

### Fixed Factors ###

nOrg <- 100

orgSize <- 100

gSize <- 10

T <- 30

conds <- expand.grid(perFired,rel,val,sr,turnOver,nOrg,orgSize,gSize,T)

allData <- apply(conds[1:5,],1,function(x) {scullen(x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9])})



