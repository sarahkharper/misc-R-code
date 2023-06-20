morph_acoustic_within_corr_Jan15 = morph_acoustic_within_corr_Jan15 %>% group_by(DIMENSION,PHONE) %>% mutate(SPEAR.PVAL.NOOUTS_BH = p.adjust(SPEAR.PVAL.NOOUTS, method = "BH"))
#artic_dimension_within_corr$SPEAR.EST = as.numeric(artic_dimension_within_corr$SPEAR.EST)
morph_acoustic_within_corr_Jan15_fortable = morph_acoustic_within_corr_Jan15[,-c(1,5:10)]
morph_acoustic_within_corr_Jan15_fortable = morph_acoustic_within_corr_Jan15_fortable[,-c(7)]
morph_acoustic_within_corr_Jan15_fortable = subset(morph_acoustic_within_corr_Jan15_fortable, FACTOR != "Pal_Area_Ref" & FACTOR != "Pal_Height_Ref" & FACTOR != "Pal_Length" & FACTOR != "Pal_Slope_Height_Ref")
morph_acoustic_within_corr_Jan15_fortable_wide = morph_acoustic_within_corr_Jan15_fortable %>% pivot_wider(names_from = c(DIMENSION), values_from = c(SPEAR.EST.NOOUTS, SPEAR.PVAL.NOOUTS, SPEAR.PVAL.NOOUTS_BH))
#morph_acoustic_within_corr_Jan15_fortable_wide %<>% mutate_if(is.numeric, round, 3)
morph_acoustic_within_corr_Jan15_fortable_wide$FACTOR <- as.factor(morph_acoustic_within_corr_Jan15_fortable_wide$FACTOR)
levels(morph_acoustic_within_corr_Jan15_fortable_wide$FACTOR) <- c("OralCav", "Pal_Area", "Pal_Height", "Pal_Length", "Pal_Slope", "PharCav")
#morph_acoustic_within_corr_Jan15_fortable_wide$DIMENSION <- as.factor(morph_acoustic_within_corr_Jan15_fortable_wide$DIMENSION)
#levels(morph_acoustic_within_corr_Jan15_fortable_wide$DIMENSION) <- c("A12", "LA", "CD", "CL", "LP") #c("M1", "F1", "F2", "F2-F1", "F3", "F4", "M3", "M4", "M2")
morph_acoustic_within_corr_Jan15_fortable_wide$PHONE <- as.factor(morph_acoustic_within_corr_Jan15_fortable_wide$PHONE)
levels(morph_acoustic_within_corr_Jan15_fortable_wide$PHONE) <- c("/l/", "/ɹ/", "/s/", "/ʃ/") #c("M1", "F1", "F2", "F2-F1", "F3", "F4", "M3", "M4", "M2")
morph_acoustic_within_corr_Jan15_fortable_wide %<>% mutate(PHONE = factor(PHONE, levels = c("/s/", "/ʃ/", "/l/", "/ɹ/"))) %>% arrange(PHONE) #c("M1", "M2","M3", "M4", "F1", "F2","F3", "F4","F2-F1")
morph_acoustic_within_corr_Jan15_fortable_wide %<>% mutate(FACTOR = factor(FACTOR, levels = c("Pal_Area", "Pal_Length", "Pal_Height", "Pal_Slope", "OralCav", "PharCav"))) %>% arrange(FACTOR)
#morph_acoustic_within_corr_Jan15_fortable_wide %<>% mutate(DIMENSION = factor(DIMENSION, levels = c("CL", "CD", "A12", "LA", "LP"))) %>% arrange(DIMENSION) #c("M1", "M2","M3", "M4", "F1", "F2","F3", "F4","F2-F1")
#morph_acoustic_within_corr_Jan15_fortable_wide %<>% arrange(MODEL)
fric_morph_acoustic_within_corr_Jan15_fortable_wide = morph_acoustic_within_corr_Jan15_fortable_wide[,c(1,2,3:6,12:15,21:24)]
liquids_morph_acoustic_within_corr_Jan15_fortable_wide = morph_acoustic_within_corr_Jan15_fortable_wide[,c(1,2,7:11,16:20,25:29)]
fric_morph_acoustic_within_corr_Jan15_fortable_wide <- fric_morph_acoustic_within_corr_Jan15_fortable_wide[,c(1,2,3,7,11,4,8,12,5,9,13,6,10,14)]
liquids_morph_acoustic_within_corr_Jan15_fortable_wide <- liquids_morph_acoustic_within_corr_Jan15_fortable_wide[,c(1,2,3,8,13,4,9,14,5,10,15,6,11,16,7,12,17)]

fricnm <- names(fric_morph_acoustic_within_corr_Jan15_fortable_wide)
typology_fric <- data.frame(
  col_keys = c(fricnm[3:length(fricnm)]),
  factor = c("M1", "M1","M1", "M2", "M2","M2","M3", "M3","M3","M4","M4","M4"),
  stat = c("𝝆", "p", "q", "𝝆", "p", "q", "𝝆", "p", "q","𝝆", "p", "q"),
  stringsAsFactors = FALSE)
fric_morph_acoustic_within_corr_Jan15_fortable_wide = subset(fric_morph_acoustic_within_corr_Jan15_fortable_wide, PHONE == "/s/" | PHONE == "/ʃ/")
fric_morph_within_corr_table <- flextable(fric_morph_acoustic_within_corr_Jan15_fortable_wide)
fric_morph_within_corr_table <- merge_v(fric_morph_within_corr_table, j = 1)
fric_morph_within_corr_table <- set_header_df(fric_morph_within_corr_table, mapping = typology_fric, key = "col_keys")
fric_morph_within_corr_table <- merge_h(fric_morph_within_corr_table, part = "header")
fric_morph_within_corr_table <- theme_booktabs(fric_morph_within_corr_table)
fric_morph_within_corr_table <- fix_border_issues(fric_morph_within_corr_table)
fric_morph_within_corr_table <- align(fric_morph_within_corr_table, align = "center", part = "header")
fric_morph_within_corr_table <- hline(fric_morph_within_corr_table, i = c(2,4,6,8,10), border = fp_border(color = "black", style = "dashed", width = 1.5))
fric_morph_within_corr_table <- align(fric_morph_within_corr_table, align = "center", part = "body")
fric_morph_within_corr_table <- vline(fric_morph_within_corr_table, j = c(5,8,11), border = fp_border(color = "black", style = "dotted", width = 1.5))
fric_morph_within_corr_table <- colformat_num(fric_morph_within_corr_table, j = c(3:14), na_str = "", digits = 2)

liquidnm <- names(liquids_morph_acoustic_within_corr_Jan15_fortable_wide)
typology_liquid <- data.frame(
  col_keys = c(liquidnm[3:length(liquidnm)]),
  factor = c("F1", "F1","F1", "F2", "F2","F2","F3", "F3","F3","F4","F4","F4", "F2-F1", "F2-F1", "F2-F1"),
  stat = c("𝝆", "p", "q", "𝝆", "p", "q", "𝝆", "p", "q","𝝆", "p", "q","𝝆", "p", "q"),
  stringsAsFactors = FALSE)
liquids_morph_acoustic_within_corr_Jan15_fortable_wide = subset(liquids_morph_acoustic_within_corr_Jan15_fortable_wide, PHONE == "/l/" | PHONE == "/ɹ/")
liquid_morph_within_corr_table <- flextable(liquids_morph_acoustic_within_corr_Jan15_fortable_wide)
liquid_morph_within_corr_table <- merge_v(liquid_morph_within_corr_table, j = 1)
liquid_morph_within_corr_table <- set_header_df(liquid_morph_within_corr_table, mapping = typology_liquid, key = "col_keys")
liquid_morph_within_corr_table <- merge_h(liquid_morph_within_corr_table, part = "header")
liquid_morph_within_corr_table <- theme_booktabs(liquid_morph_within_corr_table)
liquid_morph_within_corr_table <- fix_border_issues(liquid_morph_within_corr_table)
liquid_morph_within_corr_table <- align(liquid_morph_within_corr_table, align = "center", part = "header")
liquid_morph_within_corr_table <- hline(liquid_morph_within_corr_table, i = c(2,4,6,8,10), border = fp_border(color = "black", style = "dashed", width = 1.5))
liquid_morph_within_corr_table <- align(liquid_morph_within_corr_table, align = "center", part = "body")
liquid_morph_within_corr_table <- vline(liquid_morph_within_corr_table, j = c(5,8,11,14), border = fp_border(color = "black", style = "dotted", width = 1.5))
liquid_morph_within_corr_table <- colformat_num(liquid_morph_within_corr_table, j = c(3:17), na_str = "", digits = 2)
