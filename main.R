suppressPackageStartupMessages({
  library(tercen)
  library(dplyr, warn.conflicts = FALSE)
  library(flowCore)
})

ctx = tercenCtx()

df <- ctx %>% as.matrix()
rn <- ctx$rselect() %>% tidyr::unite("label")
rownames(df) <- rn$label
colnames(df) <- 1:nrow(df)

ff <- tim::matrix_to_flowFrame(df)
pars <- flowCore::estimateLogicle(ff, channels = rn$label)
fs_trans <- transform(ff, pars)

df_out <- exprs(fs_trans)
colnames(df_out) <- seq_len(ncol(df_out)) - 1L

df_out %>% 
  as_tibble() %>%
  mutate(.ci = seq_len(nrow(.)) - 1L) %>%
  tidyr::pivot_longer(!matches(".ci"), names_to = ".ri") %>%
  mutate(.ri = as.integer(.ri)) %>%
  ctx$addNamespace() %>%
  ctx$save()
