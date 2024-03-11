#' Create two files with the variables in the meta-analysis and in the IPD MA
#'  
#' @param study The name of the study which should be saved in the file name, e.g., "102.us-lls"
#' @param path A path to the folder where the files should be written, e.g., "data/102.us-lls/2.data-checks".
#' @returns Two excel files with an overview of the data in the MA and in the IPD MA
#' @examples
# 'needs to be added

meta_outcome <- read_excel("docs/20231221-g2-parenting.xlsx") 
meta_predictor <- read_excel("docs/20231221-g1-parenting.xlsx") 

# Filter both outcomes and predictors on the study ID (should be there according to the MA)
present_outcomes <- meta_outcome %>%
  filter(S_ID == study_id_ma) %>% 
  select(S_ID, Outcome_name, Outcome_description, Outcome_age_child_m) %>% 
  mutate(variable_type = "outcome_ma") %>%
  relocate(variable_type, .after = S_ID) %>%
  rename(name = Outcome_name, description = Outcome_description)

present_predictors <- meta_predictor %>%
  filter(S_ID == study_id_ma) %>%
  select(S_ID, Predictor_name, Predictor_description, Predictor_age_child_m) %>%
  mutate(variable_type = "predictor_ma") %>%
  relocate(variable_type, .after = S_ID) %>%
  rename(name = Predictor_name, description = Predictor_description)

# Select info from data_dictionary (received data)
predictor_in_received_dataset <- rename_basic %>%
  filter(str_detect(new_name, "par") & str_detect(new_name, "g1")) %>%
  select(name, label, new_name) %>%
  mutate(variable_type = "received_data") %>%
  rename(description = label)

outcome_in_received_dataset <- rename_basic %>%
  filter(str_detect(new_name, "par") & str_detect(new_name, "g2")) %>%
  select(name, label, new_name) %>%
  mutate(variable_type = "received_data") %>%
  rename(description = label)

compare_ma_with_received_predictor <- bind_rows(present_predictors,
                                      predictor_in_received_dataset)

compare_ma_with_received_outcome <- bind_rows(present_outcomes,
                                                outcome_in_received_dataset)

openxlsx::write.xlsx(compare_ma_with_received_predictor, predictorpath)
openxlsx::write.xlsx(compare_ma_with_received_outcome, compare_ma_with_received_predictor.xlsx,rowNames = FALSE)


as.data