library(tidyverse) ; library(readxl)

df <- read_xlsx("data-source/Modigliani_1894_wordlist.xlsx")

# check italics marker
df |> filter(if_any(where(is.character), ~str_detect(., "(<\\/?i>|\\-o\\-|\\-ng\\-|-([a-z]{2})-)")))

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
                .names = "{.col}_1"))

# word count
# df |> mutate(across(matches("(MALESE|NIAS|TOBA_BATACCO|ENGANESE)"), ~str_count(., "\\b[^;\\s]+\\b"), .names = "nwrd_{.col}"))

df_long <- df |> 
	pivot_longer(cols = c(NIAS, TOBA_BATACCO, ENGANESE), 
	names_to = "DOCULECT", values_to = "WORDS") |>
	mutate(TOBA_BATAK_SOURCE = if_else(DOCULECT != "TOBA_BATACCO", NA, TOBA_BATAK_SOURCE))
df_long

df_long <- df_long |>
		   mutate(WORD = str_split(WORDS, "\\s;\\s")) |>
		   select(-WORDS) |>
		   unnest_longer(WORD) |>
		   mutate(WORD = str_replace_all(WORD, "\\-([oO]|w|[nN]g|[a-z]{2})\\-", "<i>\\1</i>"))
df_long

df_long <- df_long |>
	mutate(WORD2 = str_to_lower(str_replace_all(WORD, "<\\/?i>", "")))

df_long
