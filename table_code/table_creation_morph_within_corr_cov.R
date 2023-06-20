morph_within_corr_cov_Jan15_fortable = morph_within_corr_cov_Jan15_fortable %>% group_by(DIMENSION,PHONE) %>% mutate(SPEAR.PVAL.NOOUTS_BH = p.adjust(SPEAR.PVAL.NOOUTS, method = "BH"))
#artic_dimension_within_corr$SPEAR.EST = as.numeric(artic_dimension_within_corr$SPEAR.EST)
morph_within_corr_cov_Jan15_fortable = morph_within_corr_cov_Jan15_fortable[,-c(1,5:10)]
morph_within_corr_cov_Jan15_fortable_wide = morph_within_corr_cov_Jan15_fortable %>% pivot_wider(names_from = c(DIMENSION), values_from = c(SPEAR.EST.NOOUTS, SPEAR.PVAL.NOOUTS, SPEAR.PVAL.NOOUTS_BH))
#morph_within_corr_cov_Jan15_fortable_wide %<>% mutate_if(is.numeric, round, 3)
morph_within_corr_cov_Jan15_fortable_wide$FACTOR <- as.factor(morph_within_corr_cov_Jan15_fortable_wide$FACTOR)
levels(morph_within_corr_cov_Jan15_fortable_wide$FACTOR) <- c("OralCav", "Pal_Area", "Pal_Height", "Pal_Length", "Pal_Slope", "PharCav")
#morph_within_corr_cov_Jan15_fortable_wide$DIMENSION <- as.factor(morph_within_corr_cov_Jan15_fortable_wide$DIMENSION)
#levels(morph_within_corr_cov_Jan15_fortable_wide$DIMENSION) <- c("A12", "LA", "CD", "CL", "LP") #c("M1", "F1", "F2", "F2-F1", "F3", "F4", "M3", "M4", "M2")
morph_within_corr_cov_Jan15_fortable_wide$PHONE <- as.factor(morph_within_corr_cov_Jan15_fortable_wide$PHONE)
levels(morph_within_corr_cov_Jan15_fortable_wide$PHONE) <- c("/t/", "/s/", "/ʃ/", "/l/", "/ɹ/") #c("M1", "F1", "F2", "F2-F1", "F3", "F4", "M3", "M4", "M2")
morph_within_corr_cov_Jan15_fortable_wide %<>% mutate(PHONE = factor(PHONE, levels = c("/t/", "/s/", "/ʃ/", "/l/", "/ɹ/"))) %>% arrange(PHONE) #c("M1", "M2","M3", "M4", "F1", "F2","F3", "F4","F2-F1")
morph_within_corr_cov_Jan15_fortable_wide %<>% mutate(FACTOR = factor(FACTOR, levels = c("Pal_Area", "Pal_Length", "Pal_Height", "Pal_Slope", "OralCav", "PharCav"))) %>% arrange(FACTOR)
#morph_within_corr_cov_Jan15_fortable_wide %<>% mutate(DIMENSION = factor(DIMENSION, levels = c("CL", "CD", "A12", "LA", "LP"))) %>% arrange(DIMENSION) #c("M1", "M2","M3", "M4", "F1", "F2","F3", "F4","F2-F1")
#morph_within_corr_cov_Jan15_fortable_wide %<>% arrange(MODEL)
morph_within_corr_cov_Jan15_fortable_wide <- morph_within_corr_cov_Jan15_fortable_wide[,c(1,2,3,5,7,4,6,8)]
nm <- names(morph_within_corr_cov_Jan15_fortable_wide)
typology11cov <- data.frame(
  col_keys = c(nm[3:length(nm)]),
  factor = c("CD", "CD","CD", "LA","LA","LA"),
  stat = c("𝝆", "p", "q", "𝝆", "p", "q"),
  stringsAsFactors = FALSE)
morph_within_corr_table_cov <- flextable(morph_within_corr_cov_Jan15_fortable_wide)
morph_within_corr_table_cov <- merge_v(morph_within_corr_table_cov, j = 1)
morph_within_corr_table_cov <- set_header_df(morph_within_corr_table_cov, mapping = typology11cov, key = "col_keys")
morph_within_corr_table_cov <- merge_h(morph_within_corr_table_cov, part = "header")
morph_within_corr_table_cov <- theme_booktabs(morph_within_corr_table_cov)
morph_within_corr_table_cov <- fix_border_issues(morph_within_corr_table_cov)
morph_within_corr_table_cov <- align(morph_within_corr_table_cov, align = "center", part = "header")
morph_within_corr_table_cov <- hline(morph_within_corr_table_cov, i = c(5,10,15,20, 25), border = fp_border(color = "black", style = "dashed", width = 1.5))
morph_within_corr_table_cov <- align(morph_within_corr_table_cov, align = "center", part = "body")
morph_within_corr_table_cov <- vline(morph_within_corr_table_cov, j = c(5), border = fp_border(color = "black", style = "dotted", width = 1.5))
morph_within_corr_table_cov <- colformat_num(morph_within_corr_table_cov, j = c(3:8), na_str = "", digits = 2)
morph_within_corr_table_cov <- fit_to_width(morph_within_corr_table_cov, 7.5)