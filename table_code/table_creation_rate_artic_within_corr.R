rate_within_corr_Jan15_fortable = rate_within_corr_Jan15_fortable %>% group_by(DIMENSION,PHONE) %>% mutate(SPEAR.PVAL_BH = p.adjust(SPEAR.PVAL, method = "BH"))
#artic_dimension_within_corr$SPEAR.EST = as.numeric(artic_dimension_within_corr$SPEAR.EST)
rate_within_corr_Jan15_fortable = rate_within_corr_Jan15_fortable[,-c(1,5:6,9:12)]
rate_within_corr_Jan15_fortable_wide = rate_within_corr_Jan15_fortable %>% pivot_wider(names_from = c(DIMENSION), values_from = c(SPEAR.EST, SPEAR.PVAL, SPEAR.PVAL_BH))
#rate_within_corr_Jan15_fortable_wide %<>% mutate_if(is.numeric, round, 3)
rate_within_corr_Jan15_fortable_wide$FACTOR <- as.factor(rate_within_corr_Jan15_fortable_wide$FACTOR)
levels(rate_within_corr_Jan15_fortable_wide$FACTOR) <- c("IntD_MED", "IntD_SD", "PR_MED", "PR_SD", "UVD_MED", "UVD_SD")
#rate_within_corr_Jan15_fortable_wide$DIMENSION <- as.factor(rate_within_corr_Jan15_fortable_wide$DIMENSION)
#levels(rate_within_corr_Jan15_fortable_wide$DIMENSION) <- c("A12", "LA", "CD", "CL", "LP") #c("M1", "F1", "F2", "F2-F1", "F3", "F4", "M3", "M4", "M2")
rate_within_corr_Jan15_fortable_wide$PHONE <- as.factor(rate_within_corr_Jan15_fortable_wide$PHONE)
levels(rate_within_corr_Jan15_fortable_wide$PHONE) <- c("/t/", "/s/", "/ʃ/", "/l/", "/ɹ/") #c("M1", "F1", "F2", "F2-F1", "F3", "F4", "M3", "M4", "M2")
rate_within_corr_Jan15_fortable_wide %<>% mutate(PHONE = factor(PHONE, levels = c("/t/", "/s/", "/ʃ/", "/l/", "/ɹ/"))) %>% arrange(PHONE) #c("M1", "M2","M3", "M4", "F1", "F2","F3", "F4","F2-F1")
rate_within_corr_Jan15_fortable_wide %<>% mutate(FACTOR = factor(FACTOR, levels = c("IntD_MED", "IntD_SD", "UVD_MED", "UVD_SD", "PR_MED", "PR_SD"))) %>% arrange(FACTOR)
#rate_within_corr_Jan15_fortable_wide %<>% mutate(DIMENSION = factor(DIMENSION, levels = c("CL", "CD", "A12", "LA", "LP"))) %>% arrange(DIMENSION) #c("M1", "M2","M3", "M4", "F1", "F2","F3", "F4","F2-F1")
#rate_within_corr_Jan15_fortable_wide %<>% arrange(MODEL)
t = str_split_fixed(rate_within_corr_Jan15_fortable_wide$FACTOR, "_", 2)
rate_within_corr_Jan15_fortable_wide$FACTOR = t[,1]
rate_within_corr_Jan15_fortable_wide$STAT = t[,2]
rate_within_corr_Jan15_fortable_wide <- rate_within_corr_Jan15_fortable_wide[,c(18,1,2,3,8,13,4,9,14,5,10,15,6,11,16,7,12,17)]
levels(rate_within_corr_Jan15_fortable_wide$STAT) <- c("Median", "SD") #c("M1", "F1", "F2", "F2-F1", "F3", "F4", "M3", "M4", "M2")
rate_within_corr_Jan15_fortable_wide %<>% mutate(STAT = factor(STAT, levels = c("Median", "SD"))) %>% arrange(STAT)
nm <- names(rate_within_corr_Jan15_fortable_wide)
typology12 <- data.frame(
  col_keys = nm[4:length(nm)],
  factor = c("CL", "CL","CL","CD", "CD","CD","CO", "CO","CO","LA","LA","LA", "LP", "LP", "LP"),
  stat = c("𝝆", "p", "q", "𝝆", "p", "q", "𝝆", "p", "q","𝝆", "p", "q","𝝆", "p", "q"),
  stringsAsFactors = FALSE)
rate_within_corr_table <- flextable(rate_within_corr_Jan15_fortable_wide)
rate_within_corr_table <- merge_v(rate_within_corr_table, j = 1)
rate_within_corr_table <- merge_v(rate_within_corr_table, j = 2)
rate_within_corr_table <- set_header_df(rate_within_corr_table, mapping = typology12, key = "col_keys")
rate_within_corr_table <- merge_h(rate_within_corr_table, part = "header")
rate_within_corr_table <- theme_booktabs(rate_within_corr_table)
rate_within_corr_table <- fix_border_issues(rate_within_corr_table)
rate_within_corr_table <- align(rate_within_corr_table, align = "center", part = "header")
rate_within_corr_table <- hline(rate_within_corr_table, i = c(5, 10, 20, 25), border = fp_border(color = "black", style = "dashed", width = 1.5))
rate_within_corr_table <- hline(rate_within_corr_table, i = c(15), border = fp_border(color = "black",  width = 1.5))
rate_within_corr_table <- align(rate_within_corr_table, align = "center", part = "body")
rate_within_corr_table <- vline(rate_within_corr_table, j = c(6,9,12,15), border = fp_border(color = "black", style = "dotted", width = 1.5))
rate_within_corr_table <- colformat_num(rate_within_corr_table, j = c(4:18), na_str = "", digits = 2)
