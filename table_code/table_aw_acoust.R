acoustic_data_aw_Jun25_corr2$estimate = as.numeric(acoustic_data_aw_Jun25_corr2$estimate)
acoustic_data_aw_Jun25_corr2$p.value = as.numeric(acoustic_data_aw_Jun25_corr2$p.value)
acoustic_data_aw_Jun25_corr2$p.value_BH = as.numeric(acoustic_data_aw_Jun25_corr2$p.value_BH)
acoustic_data_aw_Jun25_corr2_wide = acoustic_data_aw_Jun25_corr2 %>% pivot_wider(names_from = c(PHONE), values_from = c(estimate, p.value, p.value_BH))
#acoustic_data_aw_Jun25_corr2_wide %<>% mutate_if(is.numeric, round, 3)
acoustic_data_aw_Jun25_corr2_wide$DIMENSION <- as.factor(acoustic_data_aw_Jun25_corr2_wide$DIMENSION)
levels(acoustic_data_aw_Jun25_corr2_wide$DIMENSION) <- c("M1", "F1", "F2", "F2-F1", "F3", "F4", "M3", "M4", "M2") #c("A12", "LA", "LP", "CD", "CL") 
acoustic_data_aw_Jun25_corr2_wide %<>% mutate(DIMENSION = factor(DIMENSION, levels = c("M1", "M2","M3", "M4", "F1", "F2","F3", "F4","F2-F1"))) %>% arrange(DIMENSION) #c("CL", "CD", "A12", "LA", "LP")
#acoustic_data_aw_Jun25_corr2_wide %<>% arrange(MODEL)
acoustic_data_aw_Jun25_corr2_wide <- acoustic_data_aw_Jun25_corr2_wide[,c(1,2,6,10,3,7,11,4,8,12,5,9,13)]
nm <- names(acoustic_data_aw_Jun25_corr2_wide)
typology7 <- data.frame(
  col_keys = c(nm[2:length(nm)]),
  factor = c("/s/", "/s/", "/s/", "/ʃ/", "/ʃ/", "/ʃ/", "/l/","/l/","/l/","/ɹ/","/ɹ/","/ɹ/"),
  stat = c("𝝆", "p", "q", "𝝆", "p", "q", "𝝆", "p", "q","𝝆", "p", "q"),
  stringsAsFactors = FALSE)
acoustic_data_aw_Jun25_corr2_table <- flextable(acoustic_data_aw_Jun25_corr2_wide)
acoustic_data_aw_Jun25_corr2_table <- merge_v(acoustic_data_aw_Jun25_corr2_table, j = 1)
acoustic_data_aw_Jun25_corr2_table <- set_header_df(acoustic_data_aw_Jun25_corr2_table, mapping = typology7, key = "col_keys")
acoustic_data_aw_Jun25_corr2_table <- merge_h(acoustic_data_aw_Jun25_corr2_table, part = "header")
acoustic_data_aw_Jun25_corr2_table <- theme_booktabs(acoustic_data_aw_Jun25_corr2_table)
acoustic_data_aw_Jun25_corr2_table <- fix_border_issues(acoustic_data_aw_Jun25_corr2_table)
acoustic_data_aw_Jun25_corr2_table <- align(acoustic_data_aw_Jun25_corr2_table, align = "center", part = "header")
acoustic_data_aw_Jun25_corr2_table <- hline(acoustic_data_aw_Jun25_corr2_table, i = c(4), border = fp_border(color = "black", style = "dashed", width = 1.5))
acoustic_data_aw_Jun25_corr2_table <- align(acoustic_data_aw_Jun25_corr2_table, align = "center", part = "body")
acoustic_data_aw_Jun25_corr2_table <- vline(acoustic_data_aw_Jun25_corr2_table, j = c(4,7,10,13), border = fp_border(color = "black", style = "dotted", width = 1.5))
acoustic_data_aw_Jun25_corr2_table <- colformat_num(acoustic_data_aw_Jun25_corr2_table, j = c(2:13), na_str = "", digits = 3)
acoustic_data_aw_Jun25_corr2_table <- fit_to_width(acoustic_data_aw_Jun25_corr2_table, 7.5)
