rate_within_acoust_corr_fortable = rate_within_acoust_corr_fortable %>% group_by(DIMENSION,PHONE) %>% mutate(SPEAR.PVAL.NOOUTS_BH = p.adjust(SPEAR.PVAL.NOOUTS, method = "BH"))
#artic_dimension_within_corr$SPEAR.EST = as.numeric(artic_dimension_within_corr$SPEAR.EST)
rate_within_acoust_corr_fortable = rate_within_acoust_corr_fortable[,-c(1,5:10)]
#rate_within_acoust_corr_fortable = rate_within_acoust_corr_fortable[,-c(7)]
rate_within_acoust_corr_fortable = subset(rate_within_acoust_corr, FACTOR == "unstress_avg_all_MED_BYTASK" | FACTOR == "unstress_avg_all_SD_BYTASK"| FACTOR == "all_dur_MED_BYTASK" | FACTOR == "all_dur_SD_BYTASK" | FACTOR == "sp_freq_SD_BYTASK"| FACTOR == "sp_freq_MED_BYTASK")
rate_within_acoust_corr_fortable_wide = rate_within_acoust_corr_fortable %>% pivot_wider(names_from = c(DIMENSION), values_from = c(SPEAR.EST.NOOUTS, SPEAR.PVAL.NOOUTS, SPEAR.PVAL.NOOUTS_BH))
#rate_within_acoust_corr_fortable_wide %<>% mutate_if(is.numeric, round, 3)
rate_within_acoust_corr_fortable_wide$FACTOR <- as.factor(rate_within_acoust_corr_fortable_wide$FACTOR)
levels(rate_within_acoust_corr_fortable_wide$FACTOR) <- c("IntD_MED", "IntD_SD", "PR_MED", "PR_SD", "UVD_MED", "UVD_SD")
#rate_within_acoust_corr_fortable_wide$DIMENSION <- as.factor(rate_within_acoust_corr_fortable_wide$DIMENSION)
#levels(rate_within_acoust_corr_fortable_wide$DIMENSION) <- c("A12", "LA", "CD", "CL", "LP") #c("M1", "F1", "F2", "F2-F1", "F3", "F4", "M3", "M4", "M2")
rate_within_acoust_corr_fortable_wide$PHONE <- as.factor(rate_within_acoust_corr_fortable_wide$PHONE)
levels(rate_within_acoust_corr_fortable_wide$PHONE) <- c("/l/", "/ɹ/", "/s/", "/ʃ/") #c("M1", "F1", "F2", "F2-F1", "F3", "F4", "M3", "M4", "M2")
rate_within_acoust_corr_fortable_wide %<>% mutate(PHONE = factor(PHONE, levels = c("/s/", "/ʃ/", "/l/", "/ɹ/"))) %>% arrange(PHONE) #c("M1", "M2","M3", "M4", "F1", "F2","F3", "F4","F2-F1")
rate_within_acoust_corr_fortable_wide %<>% mutate(FACTOR = factor(FACTOR, levels = c("IntD_MED", "PR_MED", "UVD_MED", "IntD_SD", "PR_SD", "UVD_SD"))) %>% arrange(FACTOR)
#rate_within_acoust_corr_fortable_wide %<>% mutate(DIMENSION = factor(DIMENSION, levels = c("CL", "CD", "A12", "LA", "LP"))) %>% arrange(DIMENSION) #c("M1", "M2","M3", "M4", "F1", "F2","F3", "F4","F2-F1")
#rate_within_acoust_corr_fortable_wide %<>% arrange(MODEL)
t = str_split_fixed(rate_within_acoust_corr_fortable_wide$FACTOR, "_", 2)
rate_within_acoust_corr_fortable_wide$FACTOR = t[,1]
rate_within_acoust_corr_fortable_wide$STAT = t[,2]
fric_rate_within_acoust_corr_fortable_wide = rate_within_acoust_corr_fortable_wide[,c(1,2,3:6,12:15,21:24, 30)]
liquids_rate_within_acoust_corr_fortable_wide = rate_within_acoust_corr_fortable_wide[,c(1,2,7:11,16:20,25:29, 30)]
fric_rate_within_acoust_corr_fortable_wide <- fric_rate_within_acoust_corr_fortable_wide[,c(15, 1,2,3,7,11,4,8,12,5,9,13,6,10,14)]
liquids_rate_within_acoust_corr_fortable_wide <- liquids_rate_within_acoust_corr_fortable_wide[,c(18, 1,2,3,8,13,4,9,14,5,10,15,6,11,16,7,12,17)]

fricnm <- names(fric_rate_within_acoust_corr_fortable_wide)
typology_fric <- data.frame(
  col_keys = c(fricnm[4:length(fricnm)]),
  factor = c("M1", "M1","M1", "M2", "M2","M2","M3", "M3","M3","M4","M4","M4"),
  stat = c("𝝆", "p", "q", "𝝆", "p", "q", "𝝆", "p", "q","𝝆", "p", "q"),
  stringsAsFactors = FALSE)
fric_rate_within_acoust_corr_fortable_wide = subset(fric_rate_within_acoust_corr_fortable_wide, PHONE == "/s/" | PHONE == "/ʃ/")
fric_rate_within_corr_table <- flextable(fric_rate_within_acoust_corr_fortable_wide)
fric_rate_within_corr_table <- merge_v(fric_rate_within_corr_table, j = 1)
fric_rate_within_corr_table <- merge_v(fric_rate_within_corr_table, j = 2)
fric_rate_within_corr_table <- set_header_df(fric_rate_within_corr_table, mapping = typology_fric, key = "col_keys")
fric_rate_within_corr_table <- merge_h(fric_rate_within_corr_table, part = "header")
fric_rate_within_corr_table <- theme_booktabs(fric_rate_within_corr_table)
fric_rate_within_corr_table <- fix_border_issues(fric_rate_within_corr_table)
fric_rate_within_corr_table <- align(fric_rate_within_corr_table, align = "center", part = "header")
fric_rate_within_corr_table <- hline(fric_rate_within_corr_table, i = c(2,4,8,10), border = fp_border(color = "black", style = "dashed", width = 1.5))
fric_rate_within_corr_table <- hline(fric_rate_within_corr_table, i = c(6), border = fp_border(color = "black", width = 1.5))
fric_rate_within_corr_table <- align(fric_rate_within_corr_table, align = "center", part = "body")
fric_rate_within_corr_table <- vline(fric_rate_within_corr_table, j = c(6,9,12), border = fp_border(color = "black", style = "dotted", width = 1.5))
fric_rate_within_corr_table <- colformat_num(fric_rate_within_corr_table, j = c(4:15), na_str = "", digits = 2)

liquidnm <- names(liquids_rate_within_acoust_corr_fortable_wide)
typology_liquid <- data.frame(
  col_keys = c(liquidnm[4:length(liquidnm)]),
  factor = c("F1", "F1","F1", "F2", "F2","F2","F3", "F3","F3","F4","F4","F4", "F2-F1", "F2-F1", "F2-F1"),
  stat = c("𝝆", "p", "q", "𝝆", "p", "q", "𝝆", "p", "q","𝝆", "p", "q","𝝆", "p", "q"),
  stringsAsFactors = FALSE)
liquids_rate_within_acoust_corr_fortable_wide = subset(liquids_rate_within_acoust_corr_fortable_wide, PHONE == "/l/" | PHONE == "/ɹ/")
liquid_rate_within_corr_table <- flextable(liquids_rate_within_acoust_corr_fortable_wide)
liquid_rate_within_corr_table <- merge_v(liquid_rate_within_corr_table, j = 1)
liquid_rate_within_corr_table <- merge_v(liquid_rate_within_corr_table, j = 2)
liquid_rate_within_corr_table <- set_header_df(liquid_rate_within_corr_table, mapping = typology_liquid, key = "col_keys")
liquid_rate_within_corr_table <- merge_h(liquid_rate_within_corr_table, part = "header")
liquid_rate_within_corr_table <- theme_booktabs(liquid_rate_within_corr_table)
liquid_rate_within_corr_table <- fix_border_issues(liquid_rate_within_corr_table)
liquid_rate_within_corr_table <- align(liquid_rate_within_corr_table, align = "center", part = "header")
liquid_rate_within_corr_table <- hline(liquid_rate_within_corr_table, i = c(2,4,8,10), border = fp_border(color = "black", style = "dashed", width = 1.5))
liquid_rate_within_corr_table <- hline(liquid_rate_within_corr_table, i = c(6), border = fp_border(color = "black", width = 1.5))
liquid_rate_within_corr_table <- align(liquid_rate_within_corr_table, align = "center", part = "body")
liquid_rate_within_corr_table <- vline(liquid_rate_within_corr_table, j = c(6,9,12,15), border = fp_border(color = "black", style = "dotted", width = 1.5))
liquid_rate_within_corr_table <- colformat_num(liquid_rate_within_corr_table, j = c(3:18), na_str = "", digits = 2)
