suppressPackageStartupMessages({
  library(tercen)
  library(dplyr, warn.conflicts = FALSE)
  library(flowCore)
})

ctx = tercenCtx()

df <- ctx %>% as.matrix()
rn <- ctx$rselect() %>% tidyr::unite("label")

colnames(df) <- 1:ncol(df)
rownames(df) <- rn$label

ff <- tim::matrix_to_flowFrame(df)

pars <- flowCore::estimateLogicle(ff, channels = 1:ncol(df))
fs_trans <- transform(ff, pars)

df_out <- exprs(fs_trans)
df_out
colnames(df_out) <- seq_len(ncol(df_out)) - 1L

df_out %>% 
  as_tibble() %>%
  mutate(.ri = seq_len(nrow(.)) - 1L) %>%
  tidyr::pivot_longer(!matches(".ri"), names_to = ".ci") %>%
  mutate(.ci = as.integer(.ci)) %>%
  ctx$addNamespace() %>%
  ctx$save()
