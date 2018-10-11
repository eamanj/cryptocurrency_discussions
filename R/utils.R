setwd(dir = "~/research/bitcoin/")
source("R/elastic_net.R")

get_essential_independent_vars = function() {
  all_independent_vars = c("user_ages_mean",
                           "log_user_ages_mean",
                           "user_announcement_posts_entropy_normalized",
                           #"rest_1_posters_posts_entropy_normalized",
                           #"num_unique_announcement_threads",
                           #"log_num_unique_announcement_threads",
                           "log_num_unique_threads")
  return(all_independent_vars)
}

get_tiny_independent_vars = function() {
  all_independent_vars = c("user_num_posts",
                           "log_user_num_posts",
                           "user_ages_mean",
                           "log_user_ages_mean",
                           "log_num_unique_users",
                           "user_announcement_posts_entropy_normalized",
                           "num_unique_threads",
                           "num_unique_announcement_threads",
                           "log_num_unique_threads")
  return(all_independent_vars)
}

get_very_small_independent_vars = function() {
  all_independent_vars = c("user_num_posts",
                           "log_user_num_posts",
                           "num_unique_users",
                           "log_num_unique_users",
                           "user_ages_mean",
                           "log_user_ages_mean",
                           "user_announcement_posts_entropy_normalized",
                           "user_all_threads_mean",
                           "user_all_threads_entropy_normalized",
                           "user_all_announcement_posts_mean",
                           "user_all_announcement_posts_entropy_normalized",
                           "user_all_announcement_threads_mean",
                           "user_all_announcement_threads_entropy_normalized",
                           "log_num_unique_threads",
                           "num_unique_announcement_threads",
                           "thread_all_announcement_threads_entropy_normalized")
  return(all_independent_vars)
}

get_small_independent_vars = function() {
  all_independent_vars = c("user_num_posts",
                           "log_user_num_posts",
                           "fraction_user_num_replies",
                           "threads_per_users",
                           "num_unique_users",
                           "log_num_unique_users",
                           "user_announcement_posts_entropy_normalized",
                           "user_all_threads_mean",
                           "user_all_threads_entropy_normalized",
                           "user_all_announcement_posts_mean",
                           "user_all_announcement_posts_entropy_normalized",
                           "user_all_announcement_threads_mean",
                           "user_all_announcement_threads_entropy_normalized",
                           "user_ages_mean",
                           "log_user_ages_mean",
                           "user_ages_discrete_entropy",
                           "log_num_unique_threads",
                           "num_unique_announcement_threads",
                           "thread_all_announcement_threads_entropy_normalized",
                           "announcement_posts_tokens_mean",
                           "normalized_rel_sentiment_entropy_normalized")
  return(all_independent_vars)
}

get_independent_vars = function() {
  all_independent_vars = c("user_num_posts", "fraction_user_num_replies",
                           "log_user_num_posts",
                           "num_unique_users","user_announcement_posts_mean",
                           "log_num_unique_users",
                           "user_announcement_posts_entropy_normalized",
                           "user_all_posts_mean",
                           "user_all_posts_entropy_normalized",
                           "user_all_threads_mean",
                           "user_all_threads_entropy_normalized",
                           "user_all_announcement_posts_mean",
                           "user_all_announcement_posts_entropy_normalized",
                           "user_all_announcement_threads_mean",
                           "user_all_announcement_threads_entropy_normalized",
                           "user_ages_mean", "user_ages_median",
                           "log_user_ages_mean",
                           "user_ages_discrete_entropy",
                           "num_unique_threads", "thread_all_threads_mean",
                           "log_num_unique_threads", 
                           "thread_all_threads_entropy_normalized",
                           "num_unique_announcement_threads","thread_all_announcement_threads_mean",
                           "thread_all_announcement_threads_entropy_normalized",
                           "num_conversations", "num_replies_total",
                           "converstaion_replies_mean",
                           "converstaion_replies_median",
                           "total_tokens", "announcement_posts_tokens_mean",
                           "announcement_posts_tokens_median",
                           "announcement_posts_sentimental_tokens_mean",
                           "normalized_rel_sentiment_mean",
                           "normalized_rel_sentiment_entropy_normalized", "normalized_rel_sentiment_pairwise_diffs_mean",
                           "normalized_rel_sentiment_gravity_centers_distance", "normalized_rel_sentiment_negative_positive_size_diff",
                           "normalized_rel_sentiment_morales_polarization")
  return(all_independent_vars)
}

get_large_independent_vars = function() {
  all_independent_vars = c("user_num_posts", "fraction_user_num_replies",
                           "log_user_num_posts",
                           "threads_per_users",
                           "num_unique_users","user_announcement_posts_mean",
                           "user_announcement_posts_entropy",
                           "user_announcement_posts_entropy_normalized",
                           "user_all_posts_mean",
                           "user_all_posts_entropy",
                           "user_all_posts_entropy_normalized",
                           "user_all_threads_mean",
                           "user_all_threads_entropy",
                           "user_all_threads_entropy_normalized",
                           "user_all_announcement_posts_mean",
                           "user_all_announcement_posts_entropy",
                           "user_all_announcement_posts_entropy_normalized",
                           "user_all_announcement_threads_mean",
                           "user_all_announcement_threads_entropy",
                           "user_all_announcement_threads_entropy_normalized",
                           "user_ages_mean", "user_ages_median",
                           "user_ages_std",
                           "user_ages_entropy",
                           "user_ages_discrete_entropy",
                           "num_unique_threads", "thread_all_threads_mean",
                           "log_num_unique_threads", 
                           "thread_all_threads_entropy",
                           "thread_all_threads_entropy_normalized",
                           "num_unique_announcement_threads","thread_all_announcement_threads_mean",
                           "thread_all_announcement_threads_entropy",
                           "thread_all_announcement_threads_entropy_normalized",
                           "num_conversations", "num_replies_total",
                           "converstaion_replies_mean",
                           "converstaion_replies_std", "converstaion_replies_max",
                           "converstaion_replies_median",
                           "total_tokens", "announcement_posts_tokens_mean",
                           "announcement_posts_tokens_std",
                           "announcement_posts_tokens_median",
                           "total_sentimental_tokens",
                           "announcement_posts_sentimental_tokens_mean",
                           "announcement_posts_sentimental_tokens_std","announcement_posts_sentimental_tokens_median",
                           "total_abs_sentiment_score", "abs_sentiment_score_mean", "abs_sentiment_score_std",
                           "abs_sentiment_score_median",
                           "total_rel_sentiment_score_all_tokens", "rel_sentiment_score_mean_all_tokens", "rel_sentiment_score_std_all_tokens",
                           "rel_sentiment_score_median_all_tokens",
                           "total_rel_sentiment_score_sentimental_tokens", "rel_sentiment_score_mean_sentimental_tokens", "rel_sentiment_score_std_sentimental_tokens",
                           "rel_sentiment_score_median_sentimental_tokens",
                           "normalized_rel_sentiment_mean",
                           "normalized_rel_sentiment_skewness", "normalized_rel_sentiment_kurtosis",
                           "normalized_rel_sentiment_entropy_normalized", "normalized_rel_sentiment_pairwise_diffs_mean",
                           "normalized_rel_sentiment_gravity_centers_distance", "normalized_rel_sentiment_negative_positive_size_diff",
                           "normalized_rel_sentiment_morales_polarization")
  return(all_independent_vars)
}

get_dependent_vars = function(var, stat, property, period_type, normalized, num_days = c(100, 200)) {
  num_days = paste0(num_days, "days")
  if(normalized) {
    dependent_vars = paste("normalized", var, stat, property, period_type, num_days, sep="_")
  } else {
    dependent_vars = paste(var, stat, property, period_type, num_days, sep="_")
    dependent_vars = c(dependent_vars, paste0("log_", dependent_vars))
  }
  return(dependent_vars)
}

add_demeaned_vars_by_period = function(data, vars) {
  period_means = aggregate(data[vars], by=data["period"], FUN=mean)
  period_sds = aggregate(data[vars], by=data["period"], FUN=sd)
  period_counts = data.frame(table(data$period))
  colnames(period_counts) = c("period", "period_count")
  data = merge(data, period_means, by="period", suffixes = c("", "_period_mean"))
  data = merge(data, period_sds, by="period", suffixes = c("", "_period_sd"))
  data = merge(data, period_counts, by="period")
  for(var in vars) {
    data[paste0(var, "_demeaned")] = data[var] - data[paste0(var, "_period_mean")]
    data[paste0(var, "_z_score")] = (data[var] - data[paste0(var, "_period_mean")]) / data[paste0(var, "_period_sd")]
  }
  return(data)
}

control_for_variables = function(data, controls, vars) {
  for(var in vars) {
    lm_formula = paste(var, "~", paste(controls, collapse=" + "))
    lmfit = lm(as.formula(lm_formula), data)
    data[,var] = lmfit$residuals
  }
  return(data)
}

add_btc_normalized_dependent_vars = function(data, vars) {
  for(var in vars) {
    data[,paste0(var, "_btc_diff")] = data[,var] - data[,paste0("btc_", var)]
    data[,paste0(var, "_btc_ratio")] = data[,var] / data[,paste0("btc_", var)]
  }
  return(data)
}

weighted_cor = function(data, dep_vars, indep_vars, wt = rep(1,nrow(data))) {
  res = cov.wt(data[,c(dep_vars, indep_vars)], wt=wt, cor=T)$cor
  cors = as.data.frame(as.table(res[indep_vars, dep_vars]))
  return(cors)
}

unweighted_cor = function(data, dep_vars, indep_vars, method) {
  #res = cor(data[,c(indep_vars,dep_vars)], use="pairwise.complete.obs")
  res = cor(data[,c(indep_vars,dep_vars)], method=method)
  cors = as.data.frame(as.table(res[indep_vars, dep_vars]))
  return(cors)
}

get_cors = function(data, dep_vars, indep_vars, positive, threshold, method = "pearson", wt = rep(1,nrow(data))) {
  #cors = weighted_cor(data, dep_vars, indep_vars, wt)
  cors = unweighted_cor(data, dep_vars, indep_vars, method)
  cors = cors[order(abs(cors$Freq), decreasing = TRUE),]
  if(positive) {
    result = cors[cors$Freq > 0 & abs(cors$Freq) > threshold & !is.na(cors$Freq),]
  } else {
    result = cors[cors$Freq < 0 & abs(cors$Freq) > threshold & !is.na(cors$Freq),]
  }
  return(result)
}

print_cors = function(data, dep_vars, indep_vars, threshold, method = "pearson", wt=rep(1,nrow(data))) {
  result = get_cors(data, dep_vars, indep_vars, TRUE, threshold, method, wt)
  if (nrow(result) > 0) {
    print(result)
  }
  result = get_cors(data, dep_vars, indep_vars, FALSE, threshold, method, wt)
  if (nrow(result) > 0) {
    print(result)
  }
}

run_elastic_net = function(df, indep_vars, dep_var) {
  df = df[complete.cases(df[,indep_vars]),]
  x = data.matrix(df[,indep_vars])
  y = df[,dep_var]
  # find the best elastic net model config and get nonzero coefficients on best alpha 
  alphas=seq(1,1,by=0.05)
  best_model = cross_validate_alphas(x, y, alphas)
  best_alpha = best_model[2]
  nonzero_coefs = extract_nonzero_coefs(best_model$coefs)
  print(nonzero_coefs)
  if(length(nonzero_coefs) > 0) {
    # Run simple ols
    lm_formula = paste(dep_var, "~", paste(nonzero_coefs, collapse=" + "))
    lmfit = lm(as.formula(lm_formula), df)
    print(summary(lmfit))
  }
  return(nonzero_coefs)
}


filter_data = function(data, min_trades, min_volume, min_volume_days, min_num_posts, period_type,
                       min_days_on_forum, max_normalized_volume_volatility, not_na_var,
                       max_not_na_var, positive_vars) {
  num_days = paste0(min_volume_days, "days")
  volume_mean_var = paste("volume_mean_mean", period_type, num_days, sep="_")
  volume_25_var = paste("volume_mean_25", period_type, num_days, sep="_")
  volume_volatility_var = paste("normalized_volume_mean_std", period_type, num_days, sep="_")
  price_mean_var = paste("price_mean_mean", period_type, num_days, sep="_")
  data1 = data
  data1 = data1[is.finite(data1[,not_na_var]),]
  data1 = data1[!is.nan(data1[,volume_mean_var]),]
  data1 = data1[data1[volume_mean_var]/data1[price_mean_var] > min_trades,]
  data1 = data1[is.finite(data1[,volume_25_var]),]
  data1 = data1[data1[volume_25_var] > min_volume,]
  data1 = data1[data1[volume_mean_var] > min_volume,]
  data1 = data1[data1$user_num_posts > min_num_posts | data1$days_on_forum > min_days_on_forum,]
  data1 = data1[data1$days_on_forum > min_days_on_forum,]
  data1 = data1[data1$user_num_posts > min_num_posts,]
  data1 = data1[data1$days_on_forum > min_days_on_forum,]
  data1 = data1[data1[volume_volatility_var] < max_normalized_volume_volatility,]
  data1 = data1[data1[,not_na_var] < max_not_na_var,]
  for(pos_var in positive_vars) {
    data1 = data1[data1[,pos_var] > 0,]
  }
  return(data1)
}

bin_variables = function(data, vars, num_bins) {
  for(var in vars) {
    binned_var = paste0(var, "_binned")
    data[, binned_var] = as.numeric(cut(data[,var],
                                        breaks=quantile(data[,var], probs=seq(0,1,1.0/num_bins)),
                                        include.lowest=TRUE))
  }
  return(data)
}


run_ols = function(data, indep_vars, dep_var) {
  lm_formula = paste(dep_var, "~", paste(indep_vars, collapse=" + "))
  lmfit = lm(as.formula(lm_formula), data)
  print(summary(lmfit))
  plot(data[,dep_var], predict(lmfit))
}

scale_columns = function(data, vars, rename) {
  if(rename) {
    data[,paste0("scaled_", vars)] = scale(data[,vars], center = TRUE, scale=TRUE)
  } else {
    data[,vars] = scale(data[,vars], center = TRUE, scale=TRUE)
  }
  return(data)
}
