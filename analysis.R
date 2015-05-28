#loaddata
library(survey)
library(knitr)
setwd("~/Dropbox/Projects_Research/distancePaper/")
load("complexData.RData")

#Discriptives
with(dclust, svytable(~laa005, Ntotal = 100))
with(dclust, svytable(~inUni, Ntotal = 100))
#-----------------------------------------------
#Model of distance to Any on Expectations
MexpectDany <- MIcombine(with(dclust,
			svyglm(laa005 ~ log(dAny) + pv1math + pv1read + 
				   	pv1scie + escs + indig + sex + cohort+
				   	escs:log(dAny), family = binomial
					)
			)
)

cat("<h3>M0: Log Distance to Nearest University on Expectations</h3>")
sink("/dev/null"); out <- summary(MexpectDany); sink()
kable(out, format = "markdown", padding = 2, digits = 3)
#------------------------------------------------
#Model of distance to Any on Expectations
MexpectDGo8 <- MIcombine(with(dclust,
							  svyglm(laa005 ~ log(dGo8) + pv1math + pv1read + 
							  	   	pv1scie + escs + indig + sex + cohort+
							  	   	escs:log(dGo8), family = binomial
							  )
)
)

cat("<h3>M0: Log Distance to Nearest University on Expectations</h3>")
sink("/dev/null"); out <- summary(MexpectDGo8); sink()
kable(out, format = "markdown", padding = 2, digits = 3)
#------------------------------------------------
#Number of University Campuses in Area on Expectations
MexpectPAny <- MIcombine(with(dclust,
							  svyglm(laa005 ~ pAny + pv1math + pv1read + 
							  	   	pv1scie + escs + indig + sex + cohort+
							  	   	escs:pAny, family = binomial
							  )
)
)

cat("<h3>M0: Log Distance to Nearest University on Expectations</h3>")
sink("/dev/null"); out <- summary(MexpectPAny); sink()
kable(out, format = "markdown", padding = 2, digits = 3)
#------------------------------------------------
#Number of University Campuses in Area on Expectations
MexpectPGo8 <- MIcombine(with(dclust,
							  svyglm(laa005 ~ pGo8 + pv1math + pv1read + 
							  	   	pv1scie + escs + indig + sex + cohort+
							  	   	escs:pGo8, family = binomial
							  )
)
)

cat("<h3>M0: Log Distance to Nearest University on Expectations</h3>")
sink("/dev/null"); out <- summary(MexpectPGo8); sink()
kable(out, format = "markdown", padding = 2, digits = 3)


