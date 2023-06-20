#data = data frame containing data for analysis
#reg.formula = formula to be passed in for regression (e.g., A ~ B + C)
#grouping = string array designating groups within the data frame (e.g., list of subject codes)
logistf_by_subj = function(data, reg.formula, grouping){
  subj = vector()
  var = vector()
  coefs = vector()
  ci.low = vector()
  ci.high = vector()
  pval = vector()
  list1 = split(data, data$SUBJ)
  for(i in 1:length(grouping)){
    nm = grouping[[i]]
    data.subj = list1[[nm]]
    model = logistf(reg.formula,  data = data.subj)
    for(j in 1:length(model$terms)){
      subj = append(subj, nm)
      var = append(var, model$terms[j])
      coefs = append(coefs, model$coefficients[j])
      ci.low = append(ci.low, model$ci.lower[j])
      ci.high = append(ci.high, model$ci.upper[j])
      pval = append(pval, model$prob[j])
    }
  }
  out = data.frame(cbind(subj, var, coefs, ci.low, ci.high, pval))
  out$subj = as.factor(out$subj)
  out$var = as.factor(out$var)
  out$coefs = as.numeric(as.character(out$coefs))
  out$ci.low = as.numeric(as.character(out$ci.low))
  out$ci.high = as.numeric(as.character(out$ci.high))
  out$pval = as.numeric(as.character(out$pval))
  return(out)
}