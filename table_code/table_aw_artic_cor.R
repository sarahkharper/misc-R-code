USE_all_data_aw_corr_Jan15$estimate = as.numeric(USE_all_data_aw_corr_Jan15$estimate)
USE_all_data_aw_corr_Jan15$p.value = as.numeric(USE_all_data_aw_corr_Jan15$p.value)
USE_all_data_aw_corr_Jan15$p.value_BH = as.numeric(USE_all_data_aw_corr_Jan15$p.value_BH)
USE_all_data_aw_corr_Jan15_wide = USE_all_data_aw_corr_Jan15 %>% pivot_wider(names_from = c(PHONE), values_from = c(estimate, p.value, p.value_BH))
#USE_all_data_aw_corr_Jan15_wide %<>% mutate_if(is.numeric, round, 3)
USE_all_data_aw_corr_Jan15_wide$DIMENSION <- as.factor(USE_all_data_aw_corr_Jan15_wide$DIMENSION)
levels(USE_all_data_aw_corr_Jan15_wide$DIMENSION) <- c("A12", "LA", "LP", "CD", "CL") #c("M1", "F1", "F2", "F2-F1", "F3", "F4", "M3", "M4", "M2")
USE_all_data_aw_corr_Jan15_wide %<>% mutate(DIMENSION = factor(DIMENSION, levels = c("CL", "CD", "A12", "LA", "LP"))) %>% arrange(DIMENSION) #c("M1", "M2","M3", "M4", "F1", "F2","F3", "F4","F2-F1")
#USE_all_data_aw_corr_Jan15_wide %<>% arrange(MODEL)
USE_all_data_aw_corr_Jan15_wide <- USE_all_data_aw_corr_Jan15_wide[,c(1,3,8,13,2,7,12,4,9,14,5,10,15,6,11,16)]
nm <- names(USE_all_data_aw_corr_Jan15_wide)
typology6 <- data.frame(
  col_keys = c(nm[2:length(nm)]),
  factor = c("/t/", "/t/", "/t/", "/s/", "/s/", "/s/", "/ʃ/", "/ʃ/", "/ʃ/", "/l/","/l/","/l/","/ɹ/","/ɹ/","/ɹ/"),
  stat = c("𝝆", "p", "q", "𝝆", "p", "q", "𝝆", "p", "q", "𝝆", "p", "q","𝝆", "p", "q"),
  stringsAsFactors = FALSE)
USE_all_data_aw_corr_Jan15_table <- flextable(USE_all_data_aw_corr_Jan15_wide)
USE_all_data_aw_corr_Jan15_table <- merge_v(USE_all_data_aw_corr_Jan15_table, j = 1)
USE_all_data_aw_corr_Jan15_table <- set_header_df(USE_all_data_aw_corr_Jan15_table, mapping = typology6, key = "col_keys")
USE_all_data_aw_corr_Jan15_table <- merge_h(USE_all_data_aw_corr_Jan15_table, part = "header")
USE_all_data_aw_corr_Jan15_table <- theme_booktabs(USE_all_data_aw_corr_Jan15_table)
USE_all_data_aw_corr_Jan15_table <- fix_border_issues(USE_all_data_aw_corr_Jan15_table)
USE_all_data_aw_corr_Jan15_table <- align(USE_all_data_aw_corr_Jan15_table, align = "center", part = "header")
#USE_all_data_aw_corr_Jan15_table <- hline(USE_all_data_aw_corr_Jan15_table, i = c(5), border = fp_border(color = "black", style = "dashed", width = 1.5))
USE_all_data_aw_corr_Jan15_table <- align(USE_all_data_aw_corr_Jan15_table, align = "center", part = "body")
USE_all_data_aw_corr_Jan15_table <- vline(USE_all_data_aw_corr_Jan15_table, j = c(4,7,10,13), border = fp_border(color = "black", style = "dotted", width = 1.5))
USE_all_data_aw_corr_Jan15_table <- colformat_num(USE_all_data_aw_corr_Jan15_table, j = c(2:16), na_str = "", digits = 2)
USE_all_data_aw_corr_Jan15_table <- fit_to_width(USE_all_data_aw_corr_Jan15_table, 7.5)
