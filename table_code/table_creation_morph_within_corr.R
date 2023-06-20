morph_within_corr_Jan15 = morph_within_corr_Jan15 %>% group_by(DIMENSION,PHONE) %>% mutate(SPEAR.PVAL.NOOUTS_BH = p.adjust(SPEAR.PVAL.NOOUTS, method = "BH"))
#artic_dimension_within_corr$SPEAR.EST = as.numeric(artic_dimension_within_corr$SPEAR.EST)
morph_within_corr_Jan15_fortable = morph_within_corr_Jan15[,-c(1,5:10)]
morph_within_corr_Jan15_fortable_wide = morph_within_corr_Jan15_fortable %>% pivot_wider(names_from = c(DIMENSION), values_from = c(SPEAR.EST.NOOUTS, SPEAR.PVAL.NOOUTS, SPEAR.PVAL.NOOUTS_BH))
#morph_within_corr_Jan15_fortable_wide %<>% mutate_if(is.numeric, round, 3)
morph_within_corr_Jan15_fortable_wide$FACTOR <- as.factor(morph_within_corr_Jan15_fortable_wide$FACTOR)
levels(morph_within_corr_Jan15_fortable_wide$FACTOR) <- c("OralCav", "Pal_Area", "Pal_Height", "Pal_Length", "Pal_Slope", "PharCav")
#morph_within_corr_Jan15_fortable_wide$DIMENSION <- as.factor(morph_within_corr_Jan15_fortable_wide$DIMENSION)
#levels(morph_within_corr_Jan15_fortable_wide$DIMENSION) <- c("A12", "LA", "CD", "CL", "LP") #c("M1", "F1", "F2", "F2-F1", "F3", "F4", "M3", "M4", "M2")
#morph_within_corr_Jan15_fortable_wide$PHONE <- as.factor(morph_within_corr_Jan15_fortable_wide$PHONE)
levels(morph_within_corr_Jan15_fortable_wide$PHONE) <- c("/t/", "/s/", "/ʃ/", "/l/", "/ɹ/") #c("M1", "F1", "F2", "F2-F1", "F3", "F4", "M3", "M4", "M2")
morph_within_corr_Jan15_fortable_wide %<>% mutate(PHONE = factor(PHONE, levels = c("/t/", "/s/", "/ʃ/", "/l/", "/ɹ/"))) %>% arrange(PHONE) #c("M1", "M2","M3", "M4", "F1", "F2","F3", "F4","F2-F1")
morph_within_corr_Jan15_fortable_wide %<>% mutate(FACTOR = factor(FACTOR, levels = c("Pal_Area", "Pal_Length", "Pal_Height", "Pal_Slope", "OralCav", "PharCav"))) %>% arrange(FACTOR)
#morph_within_corr_Jan15_fortable_wide %<>% mutate(DIMENSION = factor(DIMENSION, levels = c("CL", "CD", "A12", "LA", "LP"))) %>% arrange(DIMENSION) #c("M1", "M2","M3", "M4", "F1", "F2","F3", "F4","F2-F1")
#morph_within_corr_Jan15_fortable_wide %<>% arrange(MODEL)
morph_within_corr_Jan15_fortable_wide <- morph_within_corr_Jan15_fortable_wide[,c(1,2,3,8,13,4,9,14,5,10,15,6,11,16,7,12,17)]
nm <- names(morph_within_corr_Jan15_fortable_wide)
typology11 <- data.frame(
  col_keys = col_keys,
  factor = c("CL", "CL","CL","CD", "CD","CD","A12", "A12","A12","LA","LA","LA", "LP", "LP", "LP"),
  stat = c("𝝆", "p", "q", "𝝆", "p", "q", "𝝆", "p", "q","𝝆", "p", "q","𝝆", "p", "q"),
  stringsAsFactors = FALSE)
morph_within_corr_table <- flextable(morph_within_corr_Jan15_fortable_wide)
morph_within_corr_table <- merge_v(morph_within_corr_table, j = 1)
morph_within_corr_table <- set_header_df(morph_within_corr_table, mapping = typology11, key = "col_keys")
morph_within_corr_table <- merge_h(morph_within_corr_table, part = "header")
morph_within_corr_table <- theme_booktabs(morph_within_corr_table)
morph_within_corr_table <- fix_border_issues(morph_within_corr_table)
morph_within_corr_table <- align(morph_within_corr_table, align = "center", part = "header")
morph_within_corr_table <- hline(morph_within_corr_table, i = c(5,10,15,20, 25), border = fp_border(color = "black", style = "dashed", width = 1.5))
morph_within_corr_table <- align(morph_within_corr_table, align = "center", part = "body")
morph_within_corr_table <- vline(morph_within_corr_table, j = c(5,8,11,14), border = fp_border(color = "black", style = "dotted", width = 1.5))
morph_within_corr_table <- colformat_num(morph_within_corr_table, j = c(3:17), na_str = "", digits = 2)
morph_within_corr_table <- fit_to_width(morph_within_corr_table, 7.5)