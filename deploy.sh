#!/usr/bin/env bash
##deploy.sh: Deploy app to shinyapps.io

##Don't deploy if any testing setting is set to TRUE
sd=$(grep ^showDebug app.R)
if [[ $(wc -l <<<$sd) -gt 1 ]]
then
	echo "ERROR in deploying app.R:"
	echo "  Found duplicate lines for showDebug"
	echo "  Remove duplicate lines from app.R, and re-run this script"
	exit 1
fi
if [[ $sd == *TRUE ]]
then
	echo "ERROR in deploying app.R:"
	echo "  showDebug is set to TRUE"
	echo "  Set it to FALSE, and re-run this script"
fi

##Deploy app to shinyapps.io
Rscript -e "rsconnect::deployApp(appFiles=c('app.R', 'process-batchalign.R', 'elan-utils.R'), launch.browser=FALSE)"
