library(tidyverse) ; library(readxl) ; library(qlcData)
library(stringi)

df <- read_xlsx("data-source/Modigliani_1894_wordlist.xlsx")

# check italics marker
df |> 
  filter(if_any(where(is.character), 
                ~str_detect(., "(<\\/?i>|\\-o\\-|\\-ng\\-|-([a-z]{2})-)")))

# set the wide table
df_wide <- df |> 
  mutate(across(matches("MALESE|NIAS|TOBA_BATACCO|ENGANESE"), 
                ~str_split(., "\\s\\;\\s"))) |> 
  unnest_longer(col = MALESE) |> 
  unnest_longer(col = NIAS) |> 
  unnest_longer(TOBA_BATACCO) |> 
  unnest_longer(ENGANESE) |> 
  mutate(REMARK = str_split(REMARK, "\\s\\;\\s")) |> 
  unnest_longer(REMARK) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  mutate(across(matches("(MALESE|NIAS|TOBA_BATACCO|ENGANESE)"),
                ~str_replace_all(., 
                                 "\\-([oO]|w|[nN]g|[a-z]{2})\\-", 
                                 "<i>\\1</i>"))) |> 
  mutate(across(matches("(MALESE|NIAS|TOBA_BATACCO|ENGANESE)"),
                ~str_replace_all(., 
                                 "<\\/?i>", 
                                 ""),
                .names = "{.col}_1")) |> 
  
  # Extract the tag
  mutate(MALESE_1_sem = str_extract(MALESE_1, "(?<=\\<sem\\=\")[^\"]+?(?=\"\\>)"),
         MALESE_1_rm = str_extract(MALESE_1, "(?<=\\<rm\\=\")[^\"]+?(?=\"\\>)"),
         NIAS_1_sem = str_extract(NIAS_1, "(?<=\\<sem\\=\")[^\"]+?(?=\"\\>)"),
         NIAS_1_rm = str_extract(NIAS_1, "(?<=\\<rm\\=\")[^\"]+?(?=\"\\>)"),
         TOBA_BATACCO_1_sem = str_extract(TOBA_BATACCO_1, "(?<=\\<sem\\=\")[^\"]+?(?=\"\\>)"),
         TOBA_BATACCO_1_rm = str_extract(TOBA_BATACCO_1, "(?<=\\<rm\\=\")[^\"]+?(?=\"\\>)"),
         ENGANESE_1_sem = str_extract(ENGANESE_1, "(?<=\\<sem\\=\")[^\"]+?(?=\"\\>)"),
         ENGANESE_1_rm = str_extract(ENGANESE_1, "(?<=\\<rm\\=\")[^\"]+?(?=\"\\>)")) |> 
  
  # Remove tags from the `..._1` columns
  mutate(across(matches("_1$"), ~str_replace_all(., "\\<[^\\>]+\\>", ""))) |> 
  
  # Select only column(s) whose values are NOT ALL NAs
  select(where(\(z) !all(is.na(z)))) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  relocate(REMARK, .after = ENGANESE_1_rm) |> 
  relocate(TOBA_BATAK_SOURCE, .after = REMARK)
wide_cols <- colnames(df_wide) |> 
  str_replace_all("MALESE", "MALAY") |> 
  str_replace_all("ENGANESE", "ENGGANO") |> 
  str_replace_all("TOBA_BATACCO", "TOBA_BATAK") |> 
  str_replace_all("ITALIANO", "ITALIAN")
colnames(df_wide) <- wide_cols

eno <- stringi::stri_trans_nfc(str_to_lower(df_wide$ENGGANO_1))
eno_df <- qlcData::tokenize(strings = eno,
                            profile = "ortho/_10-modi1894_profile-skeleton.tsv",
                            method = "global",
                            transliterate = "Replacement",
                            ordering = NULL,
                            normalize = "NFC",
                            sep.replace = "#",
                            regex = TRUE)$strings
eno_df <- eno_df |>   
  as_tibble() |> 
  rename(ENO_TOKEN = tokenized,
         ENO_TRANSLITERATED = transliterated) |> 
  mutate(ENO_COMMON = str_replace_all(ENO_TRANSLITERATED, "\\s", ""),
         ENO_COMMON = str_replace_all(ENO_COMMON, "\\#", " "))
eno_df |> write_tsv("ortho/ortho_enggano.tsv")

# Need to figure out for Nias orthography independently as its sole profile
# nias <- stringi::stri_trans_nfc(str_to_lower(df_wide$NIAS_1))
# nias_df <- qlcData::tokenize(strings = nias,
#                             profile = "ortho/_10-modi1894_profile-skeleton.tsv",
#                             method = "global",
#                             transliterate = "Replacement",
#                             ordering = NULL,
#                             normalize = "NFC",
#                             sep.replace = "#",
#                             regex = TRUE)
# 
# nias_df <- nias_df %>%
#   .$strings |> 
#   as_tibble() |> 
#   rename(nias_TOKEN = tokenized,
#          nias_TRANSLITERATED = transliterated) |> 
#   mutate(nias_COMMON = str_replace_all(nias_TRANSLITERATED, "\\s", ""),
#          nias_COMMON = str_replace_all(nias_COMMON, "\\#", " "))
# nias_df |> write_tsv("ortho/ortho_nias.tsv")

# word count
# df |> mutate(across(matches("(MALESE|NIAS|TOBA_BATACCO|ENGANESE)"), ~str_count(., "\\b[^;\\s]+\\b"), .names = "nwrd_{.col}"))

df_long1 <- df |> 
	pivot_longer(cols = c(NIAS, TOBA_BATACCO, ENGANESE), 
	names_to = "DOCULECT", values_to = "WORDS") |>
	mutate(TOBA_BATAK_SOURCE = if_else(DOCULECT != "TOBA_BATACCO", NA, TOBA_BATAK_SOURCE))

df_long1

df_long2 <- df_long1 |>
		   mutate(WORD = str_split(WORDS, "\\s;\\s")) |>
		   select(-WORDS) |>
		   unnest_longer(WORD) |>
		   mutate(WORD = str_replace_all(WORD, "\\-([oO]|w|[nN]g|[a-z]{2})\\-", "<i>\\1</i>"))

df_long2

df_long3 <- df_long2 |>
	mutate(WORD2 = str_to_lower(str_replace_all(WORD, "<\\/?i>", ""))) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  mutate(REMARK = if_else(str_detect(WORD2, "\\<"),
                          str_extract(WORD2, "\\<.+?\\>"),
                          REMARK)) |> 
  mutate(WORD2 = str_replace_all(WORD2, "\\<.+?\\>", "")) |> 
  relocate(TOBA_BATAK_SOURCE, .after = WORD2) |> 
  relocate(REMARK, .after = TOBA_BATAK_SOURCE)

df_long3
