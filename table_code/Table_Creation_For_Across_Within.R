##TABLE CREATION - ACROSS/WITHIN ANALYSES##

#IQR - NORMAL
all_data_iqr_aw_corr %<>% mutate_if(is.numeric, round, 3)
IQRAWNormalMelt = melt(all_data_iqr_aw_corr, id = c("PHONE", "DIMENSION", "DIM.MEAS"), measure = c("estimate", "p.value"))
names(IQRAWNormalMelt)[4] <- "STAT"
IQRAWNormalMelt = IQRAWNormalMelt[,-c(3)]
levels(IQRAWNormalMelt$STAT) <- c("r", "pval")
IQRAWNormalMelt$DIMENSION = as.factor(IQRAWNormalMelt$DIMENSION)
levels(IQRAWNormalMelt$DIMENSION) <- c("A12", "A13", "A14", "LA", "LL_LP", "CD", "CL", "CL_NORM", "UL_LP")
IQRAWNormalWide <- dcast(IQRAWNormalMelt, PHONE ~ DIMENSION + STAT)
nm <- names(IQRAWNormalWide)
col_order = c(nm[1],nm[14:17],nm[12:13],nm[2:11],nm[18:19])
IQRAWNormalWide <- IQRAWNormalWide[,col_order]
nm <- names(IQRAWNormalWide)
typology <- data.frame(
  col_keys = c(nm[2:length(nm)]),
  factor = c( "CL", "CL", "CL_NORM", "CL_NORM", "CD", "CD", "A12", "A12", "A13",
              "A13", "A14", "A14", "LA", "LA", "LL_LP", "LL_LP", "UL_LP", "UL_LP"),
  stat = c("r", "pval", "r", "pval", "r", "pval",
           "r", "pval", "r", "pval", "r", "pval",
           "r", "pval", "r", "pval", "r", "pval"),
  stringsAsFactors = FALSE)

IQRAWNormalTable <- flextable(IQRAWNormalWide)
IQRAWNormalTable <- merge_v(IQRAWNormalTable, j = c("PHONE"))
IQRAWNormalTable <- set_header_df(IQRAWNormalTable, mapping = typology, key = "col_keys")
IQRAWNormalTable <- merge_h(IQRAWNormalTable, part = "header")
IQRAWNormalTable <- theme_booktabs(IQRAWNormalTable)
IQRAWNormalTable <- autofit(IQRAWNormalTable)
IQRAWNormalTable <- set_table_properties(layout = "autofit")
IQRAWNormalTable <- fix_border_issues(IQRAWNormalTable)
IQRAWNormalTable <- align(IQRAWNormalTable, align = "center", part = "header")
#IQRAWNormalTable <- hline(IQRAWNormalTable, i = c(3,5,7,9,11,13,15,17,19), border = fp_border(color="black", width = 1.5))
IQRAWNormalTable <- vline(IQRAWNormalTable, j = c(3,5,7,9,11,13,15,17,19), border = fp_border(color="black", style = "dashed", width = 1.5), part = "body")
CL_rowid = with(IQRAWNormalWide, CL_pval < 0.05)
CL_colid <- c("CL_pval", "CL_r")
CLNORM_rowid = with(IQRAWNormalWide, CL_NORM_pval < 0.05)
CLNORM_colid <- c("CL_NORM_pval", "CL_NORM_r")
CD_rowid = with(IQRAWNormalWide, CD_pval < 0.05)
CD_colid <- c("CD_pval", "CD_r")
A12_rowid = with(IQRAWNormalWide, A12_pval < 0.05)
A12_colid <- c("A12_pval", "A12_r")
A13_rowid = with(IQRAWNormalWide, A13_pval < 0.05)
A13_colid <- c("A13_pval", "A13_r")
A14_rowid = with(IQRAWNormalWide, A14_pval < 0.05)
A14_colid <- c("A14_pval", "A14_r")
LA_rowid = with(IQRAWNormalWide, LA_pval < 0.05)
LA_colid <- c("LA_pval", "LA_r")
LLLP_rowid = with(IQRAWNormalWide, LL_LP_pval < 0.05)
LLLP_colid <- c("LL_LP_pval", "LL_LP_r")
ULLP_rowid = with(IQRAWNormalWide, UL_LP_pval < 0.05)
ULLP_colid <- c("UL_LP_pval", "UL_LP_r")
IQRAWNormalTable <- IQRAWNormalTable %>% 
  color(i = CL_rowid, j = CL_colid, color = "chartreuse3") %>% bold(i = CL_rowid, j = CL_colid, bold = TRUE) %>%
  color(i = CLNORM_rowid, j = CLNORM_colid, color = "chartreuse3") %>% bold(i = CLNORM_rowid, j = CLNORM_colid, bold = TRUE) %>%
  color(i = CD_rowid, j = CD_colid, color = "chartreuse3") %>% bold(i = CD_rowid, j = CD_colid, bold = TRUE) %>%
  color(i = A12_rowid, j = A12_colid, color = "chartreuse3") %>% bold(i = A12_rowid, j = A12_colid, bold = TRUE) %>%
  color(i = A13_rowid, j = A13_colid, color = "chartreuse3") %>% bold(i = A13_rowid, j = A13_colid, bold = TRUE) %>%
  color(i = A14_rowid, j = A14_colid, color = "chartreuse3") %>% bold(i = A14_rowid, j = A14_colid, bold = TRUE) %>%
  color(i = LA_rowid, j = LA_colid, color = "chartreuse3") %>% bold(i = LA_rowid, j = LA_colid, bold = TRUE) %>%
  color(i = LLLP_rowid, j = LLLP_colid, color = "chartreuse3") %>% bold(i = LLLP_rowid, j = LLLP_colid, bold = TRUE) %>%
  color(i = ULLP_rowid, j = ULLP_colid, color = "chartreuse3") %>% bold(i = ULLP_rowid, j = ULLP_colid, bold = TRUE)
save_as_image(IQRAWNormalTable, 'IQR_AW_Normal_Table.png')
save_as_docx(IQRAWNormalTable, path = 'IQR_AW_Normal_Table.docx')

#IQR - DIFFERENCE
all_data_iqrdiff_aw_corr %<>% mutate_if(is.numeric, round, 3)
IQRDiffAWNormalMelt = melt(all_data_iqrdiff_aw_corr, id = c("PHONE", "DIMENSION", "DIM.MEAS"), measure = c("estimate", "p.value"))
names(IQRDiffAWNormalMelt)[4] <- "STAT"
IQRDiffAWNormalMelt = IQRDiffAWNormalMelt[,-c(3)]
levels(IQRDiffAWNormalMelt$STAT) <- c("r", "pval")
IQRDiffAWNormalMelt$DIMENSION = as.factor(IQRDiffAWNormalMelt$DIMENSION)
levels(IQRDiffAWNormalMelt$DIMENSION) <- c("A12", "A13", "A14", "LA", "LL_LP", "CD", "CL", "CL_NORM", "UL_LP")
IQRDiffAWNormalWide <- dcast(IQRDiffAWNormalMelt, PHONE ~ DIMENSION + STAT)
nm <- names(IQRDiffAWNormalWide)
col_order = c(nm[1],nm[14:17],nm[12:13],nm[2:11],nm[18:19])
IQRDiffAWNormalWide <- IQRDiffAWNormalWide[,col_order]
nm <- names(IQRDiffAWNormalWide)
typology <- data.frame(
  col_keys = c(nm[2:length(nm)]),
  factor = c( "CL", "CL", "CL_NORM", "CL_NORM", "CD", "CD", "A12", "A12", "A13",
              "A13", "A14", "A14", "LA", "LA", "LL_LP", "LL_LP", "UL_LP", "UL_LP"),
  stat = c("r", "pval", "r", "pval", "r", "pval",
           "r", "pval", "r", "pval", "r", "pval",
           "r", "pval", "r", "pval", "r", "pval"),
  stringsAsFactors = FALSE)

IQRDiffAWNormalTable <- flextable(IQRDiffAWNormalWide)
IQRDiffAWNormalTable <- merge_v(IQRDiffAWNormalTable, j = c("PHONE"))
IQRDiffAWNormalTable <- set_header_df(IQRDiffAWNormalTable, mapping = typology, key = "col_keys")
IQRDiffAWNormalTable <- merge_h(IQRDiffAWNormalTable, part = "header")
IQRDiffAWNormalTable <- theme_booktabs(IQRDiffAWNormalTable)
IQRDiffAWNormalTable <- autofit(IQRDiffAWNormalTable)
IQRDiffAWNormalTable <- set_table_properties(layout = "autofit")
IQRDiffAWNormalTable <- fix_border_issues(IQRDiffAWNormalTable)
IQRDiffAWNormalTable <- align(IQRDiffAWNormalTable, align = "center", part = "header")
#IQRDiffAWNormalTable <- hline(IQRDiffAWNormalTable, i = c(3,5,7,9,11,13,15,17,19), border = fp_border(color="black", width = 1.5))
IQRDiffAWNormalTable <- vline(IQRDiffAWNormalTable, j = c(3,5,7,9,11,13,15,17,19), border = fp_border(color="black", style = "dashed", width = 1.5), part = "body")
CL_rowid = with(IQRDiffAWNormalWide, CL_pval < 0.05)
CL_colid <- c("CL_pval", "CL_r")
CLNORM_rowid = with(IQRDiffAWNormalWide, CL_NORM_pval < 0.05)
CLNORM_colid <- c("CL_NORM_pval", "CL_NORM_r")
CD_rowid = with(IQRDiffAWNormalWide, CD_pval < 0.05)
CD_colid <- c("CD_pval", "CD_r")
A12_rowid = with(IQRDiffAWNormalWide, A12_pval < 0.05)
A12_colid <- c("A12_pval", "A12_r")
A13_rowid = with(IQRDiffAWNormalWide, A13_pval < 0.05)
A13_colid <- c("A13_pval", "A13_r")
A14_rowid = with(IQRDiffAWNormalWide, A14_pval < 0.05)
A14_colid <- c("A14_pval", "A14_r")
LA_rowid = with(IQRDiffAWNormalWide, LA_pval < 0.05)
LA_colid <- c("LA_pval", "LA_r")
LLLP_rowid = with(IQRDiffAWNormalWide, LL_LP_pval < 0.05)
LLLP_colid <- c("LL_LP_pval", "LL_LP_r")
ULLP_rowid = with(IQRDiffAWNormalWide, UL_LP_pval < 0.05)
ULLP_colid <- c("UL_LP_pval", "UL_LP_r")
IQRDiffAWNormalTable <- IQRDiffAWNormalTable %>% 
  color(i = CL_rowid, j = CL_colid, color = "chartreuse3") %>% bold(i = CL_rowid, j = CL_colid, bold = TRUE) %>%
  color(i = CLNORM_rowid, j = CLNORM_colid, color = "chartreuse3") %>% bold(i = CLNORM_rowid, j = CLNORM_colid, bold = TRUE) %>%
  color(i = CD_rowid, j = CD_colid, color = "chartreuse3") %>% bold(i = CD_rowid, j = CD_colid, bold = TRUE) %>%
  color(i = A12_rowid, j = A12_colid, color = "chartreuse3") %>% bold(i = A12_rowid, j = A12_colid, bold = TRUE) %>%
  color(i = A13_rowid, j = A13_colid, color = "chartreuse3") %>% bold(i = A13_rowid, j = A13_colid, bold = TRUE) %>%
  color(i = A14_rowid, j = A14_colid, color = "chartreuse3") %>% bold(i = A14_rowid, j = A14_colid, bold = TRUE) %>%
  color(i = LA_rowid, j = LA_colid, color = "chartreuse3") %>% bold(i = LA_rowid, j = LA_colid, bold = TRUE) %>%
  color(i = LLLP_rowid, j = LLLP_colid, color = "chartreuse3") %>% bold(i = LLLP_rowid, j = LLLP_colid, bold = TRUE) %>%
  color(i = ULLP_rowid, j = ULLP_colid, color = "chartreuse3") %>% bold(i = ULLP_rowid, j = ULLP_colid, bold = TRUE)
save_as_image(IQRDiffAWNormalTable, 'IQRDiff_AW_Normal_Table.png')
