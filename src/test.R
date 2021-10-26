library(lexpander)
library(here)
library(fastTextR)
library(quanteda)
library(tidyverse)

seed_nl = c("asiel*", "deport*", "immigr*", "integrat*", "migrant*", "*migrati*", "minderhe*", "multicultur*", "naturliser*", "naturalisat*", "vluchteling*")

seed_nl = c("immigr*", "integrat*")

seed_en = c("asylum*", "deport*", "ethnic*", "immigr*", "integrat*", "migrant*", "*migration*", "minorit*", "multicultur*", "naturlis*", "naturliz*", "refug*")

expand_terms_nfold = function(ft_model, terms, vocabulary=NULL, split=.5, n=5, k=1000, show_progress=TRUE)  {
  results = list()
  for (i in 1:n) {
    scores = expand_terms(ft_model, terms, vocabulary = vocabulary, split = split, k = k)
    results[[as.character(i)]] = scores %>% select(rank, precision, recall, f4) %>% add_column(i=i)
  }
  bind_rows(results)
}

metrics = function(computer, manual) {
  tp = sum(computer == 1 & manual == 1)
  fp = sum(computer == 1 & manual == 0)
  fn = sum(computer == 0 & manual == 1)

  tibble_row(ndoc=length(computer), nhit=sum(computer==1), htrue=sum(manual==1), pr=tp/(tp+fp), re=tp/(tp+fn)) %>%
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

dfm_lookup(dfm, dict_seed) %>%
  convert(to="data.frame") %>%
  as_tibble() %>%
  bind_cols(docvars(dfm, c("topic", "direction"))) %>%
  mutate(seed=as.numeric(immig>0),
         manual=as.numeric(!is.na(topic))
         ) %>%
  with(metrics(seed, manual))

metrics(comp$immig, comp$topic_binary)

expanded = names(nearest_neighbors(ft_model, words$word, 5000))
expanded = c(words$word, expanded) %>% unique() %>% intersect(colnames(dfm)) %>% setdiff("beleid")


results_gold = list()
pb = txtProgressBar(max=500, style=3)
for(i in 0:500) {
  nterms = nrow(words) + i
  setTxtProgressBar(pb, i)
  dict_expanded = dictionary(list(immig=c(words$word, head(expanded, nterms))))
  row = dfm_lookup(dfm, dict_expanded) %>%
    convert(to="data.frame") %>%
    as_tibble() %>%
    bind_cols(docvars(dfm, c("topic", "direction"))) %>%
    mutate(seed=as.numeric(immig>0),
           manual=as.numeric(!is.na(topic))
    ) %>%
    with(metrics(seed, manual)) %>%
    add_column(nterms=nterms)
  results_gold[[as.character(i)]] = row
}
results_gold = bind_rows(results_gold)
close(pb)

results %>% select(nterms, pr, re, f1) %>% pivot_longer(-nterms, names_to = "measure") %>%
  ggplot(aes(x=nterms, y=value, color=measure)) + geom_line() +
  ggtitle("Performance of expanded dictionary on CMP/Pimpo 'immigration'") +
  ggthemes::theme_clean()

results %>% select(nterms, nhit, htrue) %>% pivot_longer(-nterms, names_to = "measure") %>%
  ggplot(aes(x=nterms, y=value, color=measure)) + geom_line()

View(results)

head(expanded, 88) %>% tail()

set.seed(123)
results_sh = expand_terms_nfold(ft_model, words$word, vocabulary = colnames(dfm), k=5000, n=5)
results_sh %>%
  group_by(rank) %>%
  summarize(precision=mean(precision), recall=mean(recall), f4=mean(f4)) %>%
  pivot_longer(-rank, names_to = "measure") %>%
  ggplot(aes(x=rank, y=value, color=measure)) + geom_line() +
  ggtitle("Query expansion measures", paste0("max(f4)=", round(max(results_sh$f4),2), ", at n=", which.max(results_sh$f4)))+
  ggthemes::theme_clean()


results_sh_agg = results_sh %>%
  group_by(rank) %>%
  summarize(precision=mean(precision), recall=mean(recall), f4=mean(f4))

comb = results_gold %>% select(rank=nterms, precision_gold=pr, recall_gold=re, f1_gold=f1) %>% full_join(results_sh_agg)
max_f4 = comb %>% arrange(-f4) %>% slice(1)
max_f1 = comb %>% arrange(-f1_gold) %>% slice(1)
subtitle = glue::glue("max(f4)={round(max_f4$f4,2)} at rank={max_f4$rank}; max(f1_gold)={round(max_f1$f1_gold,2)} at rank={max_f1$rank}")

comb %>%
  pivot_longer(-rank, names_to = "measure") %>%
  mutate(measure=fct_relevel(measure, "precision", "recall", "f4", "precision_gold", "recall_gold", "f1_gold")) %>%
  ggplot(aes(x=rank, y=value, color=measure, linetype=measure)) + geom_line() +
  scale_linetype_manual(values=c(1,1,1,2,2,2)) +
  ggtitle("Query expansion measures", subtitle)+
  ggthemes::theme_clean() +
  labs(color  = "Guide name", linetype = "Guide name") +
  geom_vline(xintercept=max_f4$rank, color="green", linetype=3) +
  geom_vline(xintercept=max_f1$rank, color="pink", linetype=3)

