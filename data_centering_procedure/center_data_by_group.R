##ADD COLUMNS TO DATA FRAME CONTAINING CENTERED VALUES OF SPECIFIED (OTHER) COLUMNS
##ALREADY IN THE DATA FRAME

#INPUTS:
# seg_vect = string vector containing all of the segments to be examined
# var_vect = string vector of same length as seg_vect. Each item in vector indicates
#            what CL measure is called in the corresponding segment's file
# grouping_vect = vector with list of subject IDs
center_data_by_group = function(seg_vect, var_vect, grouping_vect){
  require(dplyr)
  require(magrittr)
  require(reshape2)
  final = vector()
  for(s in 1:length(seg_vect)){ 
    ##Load data frame for segment##
    seg = seg_vect[[s]]
    print(seg)
    #dfname = paste(toupper(seg),"data_Nov13_reduced_environs", sep = "_")
    dfname = paste(toupper(seg),"data_Nov2020", sep = "_")
    data = get(dfname)
    data = droplevels(data)
    
    ##Locate variable of interest in data frame##
    for(j in 1:length(var_vect)){
        var = var_vect[[j]]
        print(var)
        varnum = which(colnames(data) == var)
        colname = paste(var,"centered", sep = "_")
        
        ##Calculate mean var value by subj
        data = data %>%
          group_by(SUBJ) %>%
          mutate("{colname}" := scale(!!as.name(var), scale = FALSE))
        
        ##Center data based on mean
        #for(i in 1:length(grouping_vect)){
         # nm = grouping_vect[[i]]
        #  mn = means[[i]]
         # data.subj = subset(data, SUBJ == nm)
          #subjdat = pull(data.subj, var)
       # }
        print(colname)
    }
  }
  return(data)
}
