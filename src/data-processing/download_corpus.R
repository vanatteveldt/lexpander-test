# Load packages
library(tidyverse)
library(manifestoR)

mp_setapikey(key.file = "manifesto_apikey.txt")
mp_use_corpus_version(versionid = 20150708174629)

# codebook: https://manifesto-project.wzb.eu/down/datasets/pimpo/PImPo_codebook.pdf
partynames = tribble(~party, ~partyname,
                     22110, "GL",
                     22220, "SP",
                     22320, "PvdA",
                     22330, "D66",
                     22420, "VVD",
                     22521, "CDA",
                     22526, "CU",
                     22722, "PVV",
                     22951, "PvdD",
                     22952, "SGP")

url = "https://manifesto-project.wzb.eu/down/datasets/pimpo/PImPo_qsl_wo_verbatim.csv"
annotations = read_csv(url)

# Access verbatim from corpus
corpus = annotations %>%
  group_by(party, date) %>%
  slice(1) %>% ungroup() %>%
  mp_metadata() %>%
  filter(annotations == TRUE) %>%
  mp_corpus() %>%
  as.data.frame(with.meta = TRUE) %>%
  filter(text != ".", !is.na(cmp_code)) %>%
  mutate(country = as.integer(substr(as.character(party), 1, 2))) %>%
  select(date, party, pos_corpus = pos, cmp_code, text) %>%
  filter(!is.na(cmp_code))


# Add verbatim to the immigration dataset
d = left_join(annotations, corpus) %>% filter(!is.na(pos_corpus)) %>% left_join(partynames)
d_nl = d %>%
  filter(country == 22) %>%
  select(date, party, partyname, pos_corpus, topic, direction, text)


write_csv(d_nl, here("data/intermediate/d_nl.csv"))
