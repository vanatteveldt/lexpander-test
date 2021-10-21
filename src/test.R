library(lexpander)
library(here)
library(fastTextR)
library(quanteda)
library(tidyverse)

seed_nl = c("asiel*", "deport*", "immigr*", "integrat*", "migrant*", "*migrati*", "minderhe*", "multicultur*", "naturliser*", "naturlisat*", "vluchteling*")
seed_en = c("asylum*", "deport*", "ethnic*", "immigr*", "integrat*", "migrant*", "*migration*", "minorit*", "multicultur*", "naturlis*", "naturliz*", "refug*")

metrics = function(computer, manual) {
  tp = sum(computer == 1 & manual == 1)
  fp = sum(computer == 1 & manual == 0)
  fn = sum(computer == 0 & manual == 1)

  tibble_row(n=length(computer), pr=tp/(tp+fp), re=tp/(tp+fn)) %>%
    mutate(f1=2*pr*re/(pr+re))
}

if (!file.exists(here("data/tmp/cc.en.300.bin"))) {
  url = "https://dl.fbaipublicfiles.com/fasttext/vectors-crawl/cc.en.300.bin.gz"
  options(timeout=4300)  # assuming 1Mb/s
  download.file(url, destfile = here("data/tmp/cc.en.300.bin.gz"))
  R.utils::gunzip(here("data/tmp/cc.en.300.bin"))
}
if (!file.exists(here("data/tmp/fasttext-aem-amcat50.bin"))) {
  url = 'https://surfdrive.surf.nl/files/index.php/s/91TFDG1qsEMtX4d/download'
  options(timeout=4300)
  download.file(url, destfile = here("data/tmp/fasttext-aem-amcat50.bin"))
}

ft_model = ft_load(here("data/tmp/fasttext-aem-amcat50.bin"))
corpus = read_csv(here("data/intermediate/d_nl.csv")) %>% mutate(doc_id=paste(date,partyname,pos_corpus, sep="_")) %>% corpus()

dfm = corpus %>%
  tokens(remove_punct=T, remove_numbers=T, remove_symbols=T) %>%
  dfm()


seed_pattern = str_c("(?:", glob2rx(seed_nl), ")", collapse = "|")

words = quanteda.textstats::textstat_frequency(dfm) %>%
  as_tibble() %>%
  select(word=feature, freq=frequency) %>%
  filter(str_detect(word, seed_pattern))

g = pairwise_similarities(ft_model, words$word) %>%
  similarity_graph(words, threshold = .7)

igraph::E(g)$width = 1 + scales::rescale(igraph::E(g)$similarity)^3*10
plot(g)

distance_from_centroid(ft_model, words$word) |>
  enframe(name = "word", value="similarity") |>
  arrange(similarity)
# --> including minderhe* is probably a bad idea (and maybe multicultu*?)


dict_seed = dictionary(list(immig=words$word))

dfm_lookup(dfm, dict) %>%
  convert(to="data.frame") %>%
  as_tibble() %>%
  bind_cols(docvars(dfm, c("topic", "direction"))) %>%
  mutate(seed=as.numeric(immig>0),
         manual=as.numeric(!is.na(topic))
         ) %>%
  with(metrics(seed, manual))

metrics(comp$immig, comp$topic_binary)


