##CENTERING PROCEDURE FOR LOGISTIC REGRESSION ANALYSIS##

#LIQUIDS#
#calculate difference between means for segments of interest
L_data_SumStats_Nov13_reduced %<>% arrange(SUBJ, DIMENSION)
R_data_SumStats_Nov13_reduced %<>% arrange(SUBJ, DIMENSION)
liquid_mean_diff = R_data_SumStats_Nov13_reduced$mean - L_data_SumStats_Nov13_reduced$mean
liquid_mean_diff_scale = R_data_SumStats_Nov13_reduced$mean.scale - L_data_SumStats_Nov13_reduced$mean.scale

#add centered dimensions to DF and combine to one DF for further analyses
R_data_Nov13_reduced = center_data_by_group("R", var_vect, subj)
L_data_Nov13_reduced = center_data_by_group("L", var_vect, subj)
liquids_data_Nov13_reduced = rbind(L_data_Nov13_reduced, R_data_Nov13_reduced)

#calculate variability metrics (CoV, IQR and SD) for dimensions of interest
liquids_centered_SumStats = centered_var_metrics_by_group(liquids_data_Nov13_reduced, var_vect, subj)
liquids_centered_SumStats_melt = melt(liquids_centered_SumStats)
liquids_centered_SumStats_melt = separate(liquids_centered_SumStats_melt, col = variable, into = c("DIMENSION", "DIM.MEAS"), sep = "-")
liquids_centered_SumStats_melt %<>% arrange(DIM.MEAS, SUBJ, DIMENSION)
liquids_centered_SumStats_melt$mean_diff = liquid_mean_diff
write.csv(liquids_centered_SumStats_melt, "liquids_centered_SumStats_melt.csv")

#FRICATIVES#
#calculate difference between means for segments of interest
S_data_SumStats_Nov13_reduced %<>% arrange(SUBJ, DIMENSION)
SH_data_SumStats_Nov13_reduced %<>% arrange(SUBJ, DIMENSION)
fric_mean_diff = S_data_SumStats_Nov13_reduced$mean - SH_data_SumStats_Nov13_reduced$mean
fric_mean_diff_scale = S_data_SumStats_Nov13_reduced$mean.scale - SH_data_SumStats_Nov13_reduced$mean.scale

#add centered dimensions to DF and combine to one DF for further analyses
S_data_Nov13_reduced = center_data_by_group("S", var_vect, subj)
SH_data_Nov13_reduced = center_data_by_group("SH", var_vect, subj)
fric_data_Nov13_reduced = rbind(S_data_Nov13_reduced, SH_data_Nov13_reduced)

#calculate variability metrics (CoV, IQR and SD) for dimensions of interest
fric_centered_SumStats = centered_var_metrics_by_group(fric_data_Nov13_reduced, var_vect, subj)
fric_centered_SumStats_melt = melt(fric_centered_SumStats)
fric_centered_SumStats_melt = separate(fric_centered_SumStats_melt, col = variable, into = c("DIMENSION", "DIM.MEAS"), sep = "-")
fric_centered_SumStats_melt %<>% arrange(DIM.MEAS, SUBJ, DIMENSION)
fric_centered_SumStats_melt$mean_diff = fric_mean_diff
write.csv(fric_centered_SumStats_melt, "fric_centered_SumStats_melt.csv")

#LIQUIDS - ENVIRONS#
#calculate difference between means for segments of interest
L_data_SumStats_Nov13_reduced_environs %<>% arrange(SUBJ, DIMENSION)
R_data_SumStats_Nov13_reduced_environs %<>% arrange(SUBJ, DIMENSION)
liquid_mean_diff_environs = R_data_SumStats_Nov13_reduced_environs$mean - L_data_SumStats_Nov13_reduced_environs$mean
liquid_mean_diff_scale_environs = R_data_SumStats_Nov13_reduced_environs$mean.scale - L_data_SumStats_Nov13_reduced_environs$mean.scale

#add centered dimensions to DF and combine to one DF for further analyses
R_data_Nov13_reduced_environs = center_data_by_group("R", var_vect, subj)
L_data_Nov13_reduced_environs = center_data_by_group("L", var_vect, subj)
liquids_data_Nov13_reduced_environs = rbind(L_data_Nov13_reduced_environs, R_data_Nov13_reduced_environs)


#calculate variability metrics (CoV, IQR and SD) for dimensions of interest
liquids_centered_SumStats_environs = centered_var_metrics_by_group(liquids_data_Nov13_reduced_environs, var_vect, subj)
liquids_centered_SumStats_environs_melt = melt(liquids_centered_SumStats_environs)
liquids_centered_SumStats_environs_melt = separate(liquids_centered_SumStats_environs_melt, col = variable, into = c("DIMENSION", "DIM.MEAS"), sep = "-")
liquids_centered_SumStats_environs_melt %<>% arrange(DIM.MEAS, SUBJ, DIMENSION)
liquids_centered_SumStats_environs_melt$mean_diff = liquid_mean_diff_environs
write.csv(liquids_centered_SumStats_environs_melt, "liquids_centered_SumStats_environs_melt.csv")

#FRICATIVES - ENVIRONS#
#calculate difference between means for segments of interest
S_data_SumStats_Nov13_reduced %<>% arrange(SUBJ, DIMENSION)
SH_data_SumStats_Nov13_reduced %<>% arrange(SUBJ, DIMENSION)
fric_mean_diff_environs = S_data_SumStats_Nov13_reduced_environs$mean - SH_data_SumStats_Nov13_reduced_environs$mean
fric_mean_diff_scale_environs = S_data_SumStats_Nov13_reduced_environs$mean.scale - SH_data_SumStats_Nov13_reduced_environs$mean.scale

#add centered dimensions to DF and combine to one DF for further analyses
S_data_Nov13_reduced_environs = center_data_by_group("S", var_vect, subj)
SH_data_Nov13_reduced_environs = center_data_by_group("SH", var_vect, subj)
fric_data_Nov13_reduced_environs = rbind(S_data_Nov13_reduced_environs, SH_data_Nov13_reduced_environs)

#calculate variability metrics (CoV, IQR and SD) for dimensions of interest
fric_centered_SumStats_environs = centered_var_metrics_by_group(fric_data_Nov13_reduced_environs, var_vect, subj)
fric_centered_SumStats_environs_melt = melt(fric_centered_SumStats_environs)
fric_centered_SumStats_environs_melt = separate(fric_centered_SumStats_environs_melt, col = variable, into = c("DIMENSION", "DIM.MEAS"), sep = "-")
fric_centered_SumStats_environs_melt %<>% arrange(DIM.MEAS, SUBJ, DIMENSION)
fric_centered_SumStats_environs_melt$mean_diff = fric_mean_diff_environs
write.csv(fric_centered_SumStats_environs_melt, "fric_centered_SumStats_environs_melt.csv")


#FRICATIVES#
#calculate difference between means for segments of interest
S_acoustic_data_SumStats_Nov13_reduced %<>% arrange(SUBJ, DIMENSION)
SH_acoustic_data_SumStats_Nov13_reduced %<>% arrange(SUBJ, DIMENSION)
fric_acoustic_mean_diff = S_acoustic_data_SumStats_Nov13_reduced$mean - SH_acoustic_data_SumStats_Nov13_reduced$mean
fric_acoustic_mean_diff_scale = S_acoustic_data_SumStats_Nov13_reduced$mean.scale - SH_acoustic_data_SumStats_Nov13_reduced$mean.scale

#add centered dimensions to DF and combine to one DF for further analyses
S_acoustic_data_Nov13_reduced = center_data_by_group("S", var_vect_fric, subj)
SH_acoustic_data_Nov13_reduced = center_data_by_group("SH", var_vect_fric, subj)
fric_acoustic_data_Nov13_reduced = rbind(S_acoustic_data_Nov13_reduced, SH_acoustic_data_Nov13_reduced)

#calculate variability metrics (CoV, IQR and SD) for dimensions of interest
fric_acoustic_centered_SumStats = centered_var_metrics_by_group(fric_acoustic_data_Nov13_reduced, var_vect_fric, subj)
fric_acoustic_centered_SumStats_melt = melt(fric_acoustic_centered_SumStats)
fric_acoustic_centered_SumStats_melt = separate(fric_acoustic_centered_SumStats_melt, col = variable, into = c("DIMENSION", "DIM.MEAS"), sep = "-")
fric_acoustic_centered_SumStats_melt %<>% arrange(DIM.MEAS, SUBJ, DIMENSION)
fric_acoustic_centered_SumStats_melt$mean_diff = fric_acoustic_mean_diff
write.csv(fric_acoustic_centered_SumStats_melt, "fric_acoustic_centered_SumStats_melt.csv")
