brglm_over_groups = function(df, grouping_vect, formula, level_vect){
  require(brglm)
  require(dplyr)
  require(magrittr)
  require(broom)
  intermed = data.frame()
  for(i in 1:length(grouping_vect)){
    subjnm = grouping_vect[[i]]
    subjdat = df %>% filter(SUBJ == !!subjnm)
    subj.brglm = brglm(formula, data = subjdat)
    subj.coefs = as.data.frame(tidy(subj.brglm))
    subj.coefs = subset(subj.coefs, term != "(Intercept)")
    subj.coefs$DIR = ifelse(subj.coefs$estimate < 0, "S", "SH")
    subj.coefs$SIG = ifelse(subj.coefs$p.value < 0.05, "YES", "NO")
    subj.coefs$SUBJ = subjnm
    intermed = rbind(intermed, subj.coefs)
  }
  intermed = as.data.frame(intermed)
  intermed$term = as.factor(intermed$term)
  levels(intermed$term) <- level_vect
  return(intermed)
}