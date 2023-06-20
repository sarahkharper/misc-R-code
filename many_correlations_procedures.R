acoustic_aw_corr_cov = acoustic_data_plotting %>%
  nest(data = -c(PHONE, DIMENSION)) %>%
  mutate(
    test = map(data, ~cor.test(.x$cov_across, .x$cov_within, method = c("spearman"))),
    tidied = map(test,tidy)
  ) %>%
  unnest(tidied)
acoustic_aw_corr_cov = acoustic_aw_corr_cov[,-c(3,4)]

all_data_iqrwithin_pallength_corr_spearman = all_data_iqrwithin_pallength_corr_spearman[,-c(3,4)]

acoustic_data_Jun25_mean_sd_corr = acoustic_data_aw_Jun25 %>%
  nest(data = -c(PHONE, DIMENSION)) %>%
  mutate(
    test = map(data, ~cor.test(.x$subj.mean, .x$sd_within, method = c("spearman"))),
    tidied = map(test,tidy)
  ) %>%
  unnest(tidied)

acoustic_data_Jun25_mean_sd_corr = acoustic_data_Jun25_mean_sd_corr[,-c(3,4)]

artic_data_coda_iqr_aw_corr = all_coda_aw %>%
  nest(data = -c(PHONE, DIMENSION)) %>%
  mutate(
    test = map(data, ~cor.test(.x$iqr_within, .x$iqr_across, method = c("spearman"))),
    tidied = map(test,tidy)
  ) %>%
  unnest(tidied)

artic_data_coda_iqr_aw_corr = artic_data_coda_iqr_aw_corr[,-c(3,4)]

artic_data_onset_iqr_aw_corr = all_onset_aw %>%
  nest(data = -c(PHONE, DIMENSION)) %>%
  mutate(
    test = map(data, ~cor.test(.x$iqr_within, .x$iqr_across, method = c("spearman"))),
    tidied = map(test,tidy)
  ) %>%
  unnest(tidied)

artic_data_onset_iqr_aw_corr = artic_data_onset_iqr_aw_corr[,-c(3,4)]

artic_data_coda_iqr_aw_corr = all_coda_aw %>%
  nest(data = -c(PHONE, DIMENSION)) %>%
  mutate(
    test = map(data, ~cor.test(.x$iqr_within, .x$iqr_across, method = c("spearman"))),
    tidied = map(test,tidy)
  ) %>%
  unnest(tidied)

artic_data_coda_iqr_aw_corr = artic_data_coda_iqr_aw_corr[,-c(3,4)]

artic_data_onset_cov_aw_corr = all_onset_aw %>%
  nest(data = -c(PHONE, DIMENSION)) %>%
  mutate(
    test = map(data, ~cor.test(.x$cov_within, .x$cov_across, method = c("spearman"))),
    tidied = map(test,tidy)
  ) %>%
  unnest(tidied)

artic_data_onset_cov_aw_corr = artic_data_onset_cov_aw_corr[,-c(3,4)]

artic_data_coda_iqr_aw_corr = all_coda_aw %>%
  nest(data = -c(PHONE, DIMENSION)) %>%
  mutate(
    test = map(data, ~cor.test(.x$iqr_within, .x$iqr_across, method = c("spearman"))),
    tidied = map(test,tidy)
  ) %>%
  unnest(tidied)

artic_data_coda_iqr_aw_corr = artic_data_coda_iqr_aw_corr[,-c(3,4)]

artic_data_onset_sd_aw_corr = all_onset_aw %>%
  nest(data = -c(PHONE, DIMENSION)) %>%
  mutate(
    test = map(data, ~cor.test(.x$sd_within, .x$sd_across, method = c("spearman"))),
    tidied = map(test,tidy)
  ) %>%
  unnest(tidied)

artic_data_onset_sd_aw_corr = artic_data_onset_sd_aw_corr[,-c(3,4)]

artic_data_coda_sd_aw_corr = all_coda_aw %>%
  nest(data = -c(PHONE, DIMENSION)) %>%
  mutate(
    test = map(data, ~cor.test(.x$iqr_within, .x$iqr_across, method = c("spearman"))),
    tidied = map(test,tidy)
  ) %>%
  unnest(tidied)

artic_data_coda_sd_aw_corr = artic_data_coda_sd_aw_corr[,-c(3,4)]
