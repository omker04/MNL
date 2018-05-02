# calculate similarity measures based on Rooderkerk, van Heerde, Bijmolt (2013)

# similarity for discrete attributes
# sd(A, B) = 0
# sd(A, A) = (1 - Pr(A)) where Pr(A) = n_a / n
# so, (1 - share) == similarity

# Calculate similarity between discrete attributes
# Can return only the numbers required,
# but this returns matrix just in case we need to combine promotion of the other upcs

calc_discrete_similarity_matrix <- function(ds){
  #ds <- ds_df %>% t() %>% as.vector() %>% factor()
  ds_df <- data.frame(ds)
  similarity <- ds_df %>%
    count(ds) %>%
    mutate(share = n/sum(n)) %>%
    mutate(similarity = 1 - share) %>%
    select(ds, similarity)
  
  ds_sim <- ds_df %>% left_join(similarity, by="ds") %>% extract2("similarity")
  
  idx_same_level <- outer(ds, ds, function(x, y){x == y})
  idx_same_level[] <- idx_same_level %>% as.numeric()
  
  result <- idx_same_level * ds_sim
  diag(result) <- 0
  rownames(result) <- colnames(result) <- ds
  result
}

calc_discrete_similarity <- . %>% calc_discrete_similarity_matrix() %>% rowSums()

calc_cross_impact_with_discrete_similarity <- function(ps, ds){
  stopifnot(length(ps) == length(ds))
  
  sim_mat <- calc_discrete_similarity_matrix(ds)
  as.vector(sim_mat %*% ps)
}


# similarity for continuous attributes
clean_model_matrix <- function(mm){
  attributes(mm)$contrasts <- NULL
  attributes(mm)$assign <- NULL
  mm
}

get_matching_matrix <- function(fs){
  if(!is.factor(fs)) { fs <- factor(fs) }
  mm <- data_frame(fs) %>%
    model.matrix( ~ 0 + fs, .) %>%
    clean_model_matrix()
  mm
}

# similarity for continuous attributes
calc_continuous_similarity_matrix <- function(cs){
  freq_info <- data_frame(cs) %>% count(cs)
  get_freq_within <- function(x1, x2){
    xmin <- min(x1, x2)
    xmax <- max(x1, x2)
    freq_info %>% filter(xmin <= cs & cs <= xmax) %>% extract2('n') %>% sum()
  }
  css <- freq_info$cs %>% sort()
  freq_mat_small <- outer(css, css, Vectorize(get_freq_within))
  
  mm <- get_matching_matrix(cs)
  freq_mat_large <- mm %*% freq_mat_small %*% t(mm)
  
  n_alt <- length(cs)
  result <- 1 - (freq_mat_large / n_alt)
  diag(result) <- 0
  rownames(result) <- colnames(result) <- cs
  result
}

calc_continuous_similarity <- . %>% calc_continuous_similarity_matrix() %>% rowSums()

calc_cross_impact_with_continuous_similarity <- function(ps, cs){
  stopifnot(length(ps) == length(cs))
  
  sim_mat <- calc_continuous_similarity_matrix(cs)
  as.vector(sim_mat %*% ps)
}

