cov_ci_test_new = function(seg_vect, var_vect, grouping_vect){
  require(dplyr)
  require(magrittr)
  final = data.frame()
  for(s in 1:length(seg_vect)){
    seg = seg_vect[[s]]
    print(seg)
    #dfname = "t_COAT"
    #dfname = paste(toupper(seg), "data_Nov13_reduced", sep = "_")
    dfname = paste(toupper(seg),"data_Nov13_reduced", sep = "_")
    #dfname = paste(toupper(seg),"data_checked_Oct2020", sep = "_")
    #dfname = paste(toupper(seg),"data_OLD", sep = "_")
    data = get(dfname)
    data = droplevels(data)
    for(v in 1:length(var_vect)){
      var = var_vect[[v]]
      print(var)
      varnum = which(colnames(data) == var)
      data.scaled = data
      var.scaled = pull(data.scaled, var)
      var.scaled = rescale(var.scaled, to = c(0,100))
      data.scaled[,varnum] = var.scaled
      data.onset.full = subset(data, POSITION == "ON")
      data.coda.full = subset(data, POSITION == "CO")
      data.scaled.onset.full = subset(data.scaled, POSITION == "ON")
      data.scaled.coda.full = subset(data.scaled, POSITION == "CO")
      for(i in 1:length(grouping_vect)){
        intermed = data.frame(SUBJ = character(1))
        nm = grouping_vect[[i]]
        intermed$SUBJ = nm
        intermed$PHONE = seg
        intermed$DIMENSION = var
        #data.subj = list1[[nm]]
        data.subj = subset(data, SUBJ == nm)
        #data.subj2 = list2[[nm]]
        data.subj2 = subset(data.scaled, SUBJ == nm)
        data.onset = subset(data.onset.full, SUBJ == nm)
        data.scaled.onset = subset(data.scaled.onset.full, SUBJ == nm)
        data.coda = subset(data.coda.full, SUBJ == nm)
        data.scaled.coda = subset(data.scaled.coda.full, SUBJ == nm)
        #subjdat = data.subj[,varnum]
        #subjscaled = data.subj2[,varnum]
        subjdat = pull(data.subj, var)
        #subjdat = subjdat
        subjscaled = pull(data.subj2, var)
        onsdat = pull(data.onset, var)
        codat = pull(data.coda, var)
        onsscaledat = pull(data.scaled.onset, var)
        coscaledat = pull(data.scaled.coda, var)
        intermed$mean = mean(subjdat, na.rm=TRUE)
        intermed$sd = sd(subjdat, na.rm=TRUE)
        intermed$med = median(subjdat, na.rm = TRUE)
        intermed$iqr = IQR(subjdat, na.rm = TRUE)
        out = cv_versatile(abs(subjdat), na.rm=TRUE, digits = 3, method = "equal_tailed")
        intermed$est = as.numeric(as.character(out$statistics[1]))
        intermed$low = as.numeric(as.character(out$statistics[2]))
        intermed$high = as.numeric(as.character(out$statistics[3]))
        intermed$mean.scale = mean(subjscaled, na.rm=TRUE)
        intermed$sd.scale = sd(subjscaled, na.rm=TRUE)
        intermed$med.scale = median(subjscaled, na.rm = TRUE)
        intermed$iqr.scale = IQR(subjscaled, na.rm = TRUE)
        out.scale = cv_versatile(subjscaled, na.rm=TRUE, digits = 3, method = "equal_tailed")
        intermed$est.scale = as.numeric(as.character(out.scale$statistics[1]))
        intermed$low.scale = as.numeric(as.character(out.scale$statistics[2]))
        intermed$high.scale = as.numeric(as.character(out.scale$statistics[3]))
        intermed$mean.ons = mean(onsdat, na.rm=TRUE)
        intermed$sd.ons = sd(onsdat, na.rm=TRUE)
        intermed$med.ons = median(onsdat, na.rm = TRUE)
        intermed$iqr.ons = IQR(onsdat, na.rm = TRUE)
        out.ons = cv_versatile(onsdat, na.rm=TRUE, digits = 3, method = "equal_tailed")
        intermed$est.ons = as.numeric(as.character(out.ons$statistics[1]))
        intermed$low.ons = as.numeric(as.character(out.ons$statistics[2]))
        intermed$high.ons = as.numeric(as.character(out.ons$statistics[3]))
        intermed$mean.co = mean(codat, na.rm=TRUE)
        intermed$sd.co = sd(codat, na.rm=TRUE)
        intermed$med.co = median(codat, na.rm = TRUE)
        intermed$iqr.co = IQR(codat, na.rm = TRUE)
        out.co = cv_versatile(codat, na.rm=TRUE, digits = 3, method = "equal_tailed")
        intermed$est.co = as.numeric(as.character(out.co$statistics[1]))
        intermed$low.co = as.numeric(as.character(out.co$statistics[2]))
        intermed$high.co = as.numeric(as.character(out.co$statistics[3]))
        intermed$mean.onsscale = mean(onsscaledat, na.rm=TRUE)
        intermed$sd.onsscale = sd(onsscaledat, na.rm=TRUE)
        intermed$med.onsscale = median(onsscaledat, na.rm = TRUE)
        intermed$iqr.onsscale = IQR(onsscaledat, na.rm = TRUE)
        out.onsscale = cv_versatile(onsscaledat, na.rm=TRUE, digits = 3, method = "equal_tailed")
        intermed$est.onsscale = as.numeric(as.character(out.onsscale$statistics[1]))
        intermed$low.onsscale = as.numeric(as.character(out.onsscale$statistics[2]))
        intermed$high.onsscale = as.numeric(as.character(out.onsscale$statistics[3]))
        intermed$mean.coscale =  mean(coscaledat, na.rm=TRUE)
        intermed$sd.coscale = sd(coscaledat, na.rm=TRUE)
        intermed$med.coscale = median(coscaledat, na.rm = TRUE)
        intermed$iqr.coscale = IQR(coscaledat, na.rm = TRUE)
        out.coscale = cv_versatile(coscaledat, na.rm=TRUE, digits = 3, method = "equal_tailed")
        intermed$est.coscale = as.numeric(as.character(out.coscale$statistics[1]))
        intermed$low.coscale = as.numeric(as.character(out.coscale$statistics[2]))
        intermed$high.coscale = as.numeric(as.character(out.coscale$statistics[3]))
        final = rbind(intermed, final)
      }
    }
  }
  return(final)
}
