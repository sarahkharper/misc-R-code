##CALCULATE VARIABILITY METRICS (COV, IQR AND SD) FOR CENTERED VARIABILES.
##OUTPUT SHOULD BE SAVED AS DIFFERENT DF FROM SOURCE DF

centered_var_metrics_by_group = function(data, var_vect, grouping_vect){
  require(dplyr)
  require(magrittr)
  require(reshape2)
    
    ##Locate variable of interest in data frame##
  dfout = data.frame(SUBJ = character(40))
  dfout$SUBJ = grouping_vect
  for(j in 1:length(var_vect)){
    var = var_vect[[j]]
    print(var)
    varnum = which(colnames(data) == var)
    colname = paste(var,"centered", sep = "_")
      
    sdcol = data %>%
      group_by(SUBJ) %>%
      summarize("{colname}-sd" := sd(!!as.name(colname), na.rm = TRUE))
    
    iqrcol = data %>%
      group_by(SUBJ) %>%
      summarize("{colname}-iqr" := IQR(!!as.name(colname), na.rm = TRUE))
    
    means = data %>%
      group_by(SUBJ) %>%
      summarize("means" := mean(!!as.name(colname), na.rm = TRUE))
    
    covcol = (sdcol[,2]/means[,2])*100
    covcol = data.table(covcol)
    colname2 = paste(var,"cov", sep = "-")
    names(covcol)[1] <- colname2
    
    dfout = cbind(dfout, sdcol[,2], iqrcol[,2], covcol)
      
      ##Center data based on mean
      #for(i in 1:length(grouping_vect)){
      # nm = grouping_vect[[i]]
      #  mn = means[[i]]
      # data.subj = subset(data, SUBJ == nm)
      #subjdat = pull(data.subj, var)
      # }
  }
  return(dfout)
  }
  

