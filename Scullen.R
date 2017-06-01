### Function to calculate applicant true score and application score

scullen <- function(perFired,rel,val,sr,turnOver,nOrg,orgSize,gSize,T) {

appHire <- function(validity) {
	true <- rnorm((1/sr),0,1) # creates true scores for each applicant
	app <- validity*true + sqrt(1-validity^2)*rnorm((1/sr),0,1) # calculates app scores for each applicant
return(c(true[which.max(app)],app[which.max(app)]))} # returns the true score and app score for best applicant

#################################################################
########## Simulation Begins ####################################
#################################################################
singleRun <- t(sapply(1:nOrg, function(y) {
	# create initial organization
	org <- data.frame(t(sapply(1:orgSize, function(x) {appHire(val)})))
	names(org) <- c("trueScore","obsScore")
	org$teamID <- rep(1:(orgSize/gSize),each=gSize)

	# determine turnover rate 
	turn <- rpois(T,(turnOver*orgSize)) # calculates voluntary turnover for all years
	pot <- array(NA,dim=c(1,T))
	for(i in 1:T) { # starts the dynamic portion of the sim
		# replace voluntary turnover with new employees #
		org[sample(1:orgSize,turn[i]),1:2] <- data.frame(t(sapply(1:turn[i], function(x) {appHire(val)})))
	
		# generate performance appraisal data for entire organization
		org[,2] <- org[,1]*sqrt(rel)+rnorm(orgSize,0,1)*(1-sqrt(rel))

		# calculate and store organizational potential
		pot[i] <- mean(org[,1])

		# determine the teams from which members will be fired
		if(perFired == .05) {
			if(i %% 2 != 0) teamFire <- seq(1,(orgSize/gSize),2)
			if(i %% 2 == 0) teamFire <- seq(2,(orgSize/gSize),2)
		} else {teamFire <- 1:(orgSize/gSize)}

		# fire lowest performing members and replace with new applicants
		org <- sapply(1:(orgSize/gSize), function(x) {
			temp <- subset(org, teamID == x)
			if (x %in% teamFire) { # checks to see if focal team has a member to be fired
				temp[which(rank(temp[,2]) %in% (1:((orgSize/gSize)*perFired))),1:2] <- data.frame(t(sapply(1:((orgSize/gSize)*perFired), function(x) {appHire(val)})))
			} # closes if statement
			return(data.frame(temp))
		},simplify=F)
		org <- do.call("rbind",org) # structures the data from a list back to a dataframe
	} # closes for loop 
return(pot) })) # save the firms potential over time and close sapply statement

return(singleRun)} # close scullen function and return the single condition data

