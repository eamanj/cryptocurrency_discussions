library(igraph)
library(ggplot2)
library(Rmpfr)
setwd(dir = "~/research/bitcoin/")
source("R/utils.R")



prepare_network = function(data, network_file, v_attribute) {
  network_df = read.csv(network_file)
  network_df$log_users_intersect = log(network_df$users_intersect)
  network_df$log_jaccard_users_intersect = log(network_df$jaccard_users_intersect)
  
  undirected = grepl("undirected", network_file)
  g = graph_from_data_frame(network_df, directed = !undirected)
  vertex_names = V(g)$name
  v_ids = sapply(vertex_names, function(x) which(data$symbol == x))
  # remove empty entries (missing names)
  #v_ids = v_ids[lapply(v_ids, length) > 0]
  idx = !(sapply(v_ids, length))
  v_ids[idx] = NA
  v_ids = unlist(v_ids, use.names = FALSE)
  v_attr =  data[v_ids, v_attribute]
  
  # delete vertices that have no attribute
  length(V(g))
  g1 = delete_vertices(g, which(is.na(v_attr)))
  length(V(g1))
  vertex_names = V(g1)$name
  v_ids = sapply(vertex_names, function(x) which(data$symbol == x))
  v_ids = unlist(v_ids)
  v_attr =  data[v_ids, v_attribute]
  g1 = set_vertex_attr(graph = g1, name = v_attribute, value = v_attr)
  num_users =  data[v_ids, "num_unique_users"]
  g1 = set_vertex_attr(graph = g1, name = "num_users", value = num_users)
  log_num_users =  log(data[v_ids, "num_unique_users"] + 1)
  g1 = set_vertex_attr(graph = g1, name = "log_num_users", value = log_num_users)

  num_posts =  data[v_ids, "user_num_posts"]
  g1 = set_vertex_attr(graph = g1, name = "num_posts", value = num_posts)
  log_num_posts =  log(data[v_ids, "user_num_posts"] + 1)
  g1 = set_vertex_attr(graph = g1, name = "log_num_posts", value = log_num_posts)

  days_on_forum =  data[v_ids, "days_on_forum"]
  g1 = set_vertex_attr(graph = g1, name = "days_on_forum", value = num_posts)
  
  volume = data[v_ids, "volume_mean_mean_fixed_100days"]
  g1 = set_vertex_attr(graph = g1, name = "volume_fixed_100days", value = volume)
  log_volume =  log(data[v_ids, "volume_mean_mean_fixed_100days"] + 1)
  g1 = set_vertex_attr(graph = g1, name = "log_volume_fixed_100days", value = log_volume)
    return(g1)
}

add_log_price_metrics = function(data) {
  num_days = c(5, 10, 20, 30, 50, 100, 200)
  num_days = paste0(num_days, "days")
  dep_vars = paste0("normalized_price_mean_std_fixed_", num_days)
  data[,paste0("log_", dep_vars)] = log(data[,dep_vars])
  dep_vars = paste0("price_mean_volatility_fixed_", num_days)
  data[,paste0("log_", dep_vars)] = log(data[,dep_vars])
  dep_vars = paste0("volume_mean_mean_fixed", num_days)
  dep_vars = paste0("volume_mean_mean_fixed", num_days)
  return(data)
}

cdf_common_users = function(N, n1, n2, x, num_bits=256) {
  term1 = factorialMpfr(n1,num_bits) * factorialMpfr(n2,num_bits) * factorialMpfr(N-n1,num_bits) * factorialMpfr(N-n2,num_bits) / factorialMpfr(N,num_bits)
  term2 = 0
  for(i in 0:x) {
    if((N + i) >= (n1 + n2)) {
      term2 = term2 + 1.0/(factorialMpfr(i,num_bits) * factorialMpfr(n1-i,num_bits) * factorialMpfr(n2-i,num_bits) * factorialMpfr(N-n1-n2+i,num_bits))
    }
  }
  return(term1 * term2)
}

# TODO: CEHCK WHAT'S WRONG WITH THIS
term1 = factorialMpfr(N-n1) / factorialMpfr(N)
term2 = 0
for(i in 0:x) {
  xx1 = sapply(0:(i-1), function(j) ((n1-j) * (n2-j)))
  xx2 = sapply(0:(n1-i+1), function(j) (N-n2-j+1))
  xx1 =  mpfr(xx1, 50)
  xx2 =  mpfr(xx2, 50)
  term2 = term2 + (prod(xx1)/ (prod(xx2) * factorialMpfr(i, 50)))
}
term1 * term2


prob_common_users = function(N, n1, n2, x) {
  if(x >= 1) {
    terms1 = sapply(0:(x-1), function(i) ((n1-i) * (n2-i)/(x-i)))
  } else {
    terms1 = c(1)
  }
  if(n1 >= x) {
    terms2 = sapply(0:(n1-x-1), function(i) ((N - n2 - i) / (N - i)))
  } else {
    terms2 = c(1)
  }
  if(n1 >= 1 & x >= 1) {
    terms3 = sapply((n1-x):(n1-1), function(i) (1.0 / (N - i))) 
  } else {
    terms3 = c(1)
  }
    
  terms1 =  mpfr(terms1, 50)
  terms2 =  mpfr(terms2, 50)
  terms3 =  mpfr(terms3, 50)
  return(prod(terms1) * prod(terms2) * prod(terms3))
}

# function returns the minimum number of common users between two choices with n1, and n2 users each
# where the tail probability (of common users greater than threshold) is less than prob
common_users_threshold = function(N, n1, n2, prob) {
 threshold = max(n1, n2)
 tail_prob = prob_common_users(N, n1, n2, threshold)
 while(tail_prob < prob) {
   threshold = threshold - 1
   tail_prob = tail_prob + prob_common_users(N, n1, n2, threshold)
 }
 return(threshold)
}



################# FILTERING VARIABLES #####################
period_type = "fixed"
# ignore next two vars
prices_earliest_trade_shift = 0
threads_earliest_trade_shift = 0
# the currency in which price measurements are made. Can be either 'btc' or 'usd'
currency = 'usd'
# the end date of price volatility measurement period.
prices_enddate = "2016-11-01"           
# the end date of thread activity measurement period.
threads_enddate = "2016-11-01"
# the length of time, in number of days, during which thread measures are computed
threads_period_length = 600
# users who have made this many of less posts in a thread are removed from that thread as participants in the thread
threads_participation_absolute_threshold = 0
threads_participation_quantile_threshold = 0
# number of days to look for filtering based on volume levels
min_volume_days = 100
# the minimum level of 25% quantile of daily volumes (during the min_volume_days period) for the coin not to get filtered
min_volume = 25
# the minimum threshold for the average number of daily trades
min_trades = 250
# the minimum threshold  for the number of posts made in the announcement thread during the thread measurement period
min_num_posts = 25
# the maximum threshold for the volume volatility (measured in terms of standardized standard deviation). The idea is that the coin
# should have steady daily volume to be included in the analysis
max_normalized_volume_volatility = 100
# the coin announcement should be on the forum for at least this number of days at the start of the thread measurement period.
# it can be negative since the announcement could made few days after the start of the thread measurement period
min_days_on_forum = -300
# ensure this variable is not nan, so we remove missing data
not_na_var = "normalized_price_mean_std_fixed_50days"
# the prepare_data methods creates dummy varialbes for the age of the coin. For example the oldest coin will have its age ("period") dummy
# variable set to 1. Age of the coin is determined based on the first dated it goes on the market. It is important to control for the
# age of the coin since older coins might be more establisehd and stable. normalization_period_length determines the lenght of periods
# for constructing age dummy variables. the dummy variable "period" is set to number 1,2,3,4...while dummy variables period1, period2,
# period3 will be set to 1,0 which determine whether the coin was added to market within that 180 day period.
normalization_period_length = 180



v_attribute =  "normalized_price_mean_std_fixed_100days"
v_attribute = "beta_mean_returns_fixed_100days"
v_attribute = "volume_mean_mean_fixed_100days"
#v_attribute = "price_mean_volatility_fixed_100days"
#v_attribute = "log_price_mean_volatility_fixed_200days"
#e_attribute = "users_intersect"
#e_attribute = "log_users_intersect"
#e_attribute = "log_jaccard_users_intersect"
#e_attribute = "intersect_posts_commonality"

v_attribute = "log_price_mean_volatility_fixed_100days"
e_attribute = "jaccard_users_intersect"

data = prepare_data(currency, prices_enddate, prices_earliest_trade_shift,
                    threads_enddate, threads_period_length,
                    threads_participation_absolute_threshold, threads_earliest_trade_shift,
                    normalization_period_length)
data = filter_data(data, min_trades, min_volume, min_volume_days,
                   min_num_posts, period_type, min_days_on_forum,
                   max_normalized_volume_volatility, not_na_var)
data = add_log_price_metrics(data)
users_data = read.csv("./results/threads/dec_2016/user_analysis_by_period_t0.csv")
num_total_users = users_data[users_data$end_date == threads_enddate &
                             users_data$period_num_days == threads_period_length, "total_num_user"]

network_file = paste0("./results/threads/dec_2016/undirected_network_enddate-", threads_enddate,
                      "_l", threads_period_length,
                      "_ta", threads_participation_absolute_threshold,
                      "_tq", threads_participation_quantile_threshold,
                      "_s", threads_earliest_trade_shift, ".csv")
g1 = prepare_network(data, network_file, v_attribute)
# remove self loops
is_directed(g1)
length(E(g1))
length(V(g1))
g1 = simplify(g1, remove.multiple = FALSE, remove.loops = TRUE)
length(E(g1))
length(V(g1))

min_users_intersect = 5
min_symbol_users = 50
g2 = delete.edges(g1, which(edge_attr(g1, "users_intersect") < min_users_intersect |
                            edge_attr(g1, "symbol1_users") < min_symbol_users |
                            edge_attr(g1, "symbol2_users") < min_symbol_users))
length(E(g2))
length(V(g2))
g2 = delete.vertices(g2, which(degree(g2, mode="total") < 1))
length(V(g2))

users_intersect = edge_attr(g2, "users_intersect")
symbol1_users = edge_attr(g2, "symbol1_users")
symbol2_users = edge_attr(g2, "symbol2_users")
max(symbol1_users)
hist(symbol1_users, breaks=50)
num_total_users = 6000
p_vals = mapply(cdf_common_users, num_total_users, symbol1_users, symbol2_users, users_intersect)
p_vals_numeric = mapply(asNumeric, p_vals)
hist(p_vals_numeric, breaks=50)
df = data.frame(symbol1_users, symbol2_users, users_intersect, p_vals_numeric)

#hist(edge_attr(g2, e_attribute), breaks=40)
#e_attr_threshold = quantile(edge_attr(g2, e_attribute), 0.97)
#e_attr_threshold
#length(E(g2))
#g3 = delete.edges(g2, which(edge_attr(g2, e_attribute) < e_attr_threshold))
#length(E(g3))
#hist(edge_attr(g3, e_attribute), breaks=40)


hist(edge_attr(g2, e_attribute), breaks=40)
length(E(g2))
g3 = delete.edges(g2, which(p_vals_numeric < 0.9999999))
length(E(g3))
hist(edge_attr(g3, e_attribute), breaks=40)

hist(vertex_attr(g3, v_attribute), breaks=40)
length(V(g3))
g3 = delete.vertices(g3, which(degree(g3, mode="total") < 1))
length(V(g3))


assortativity(g3, types1 = vertex_attr(g3, v_attribute), directed = TRUE)
assortativity(g3, types1 = vertex_attr(g3, "days_on_forum"), directed = TRUE)

fine = 100 # this will adjust the resolving power.
palt = colorRampPalette(c('blue','red'))
#this gives you the colors you want for every point
vertex_colors = palt(fine)[as.numeric(cut(vertex_attr(g3, v_attribute), breaks=fine))]
plot.igraph(g3, vertex.size=10, vertex.label.cex=0.5, edge.width=0.6, edge.arrow.size=0.1, layout=layout_with_fr, vertex.color=vertex_colors)
plot.igraph(g3, vertex.size=10, vertex.label.cex=0.5, edge.width=0.6, edge.arrow.size=0.1, vertex.color=vertex_colors)

vertex_size = 2*vertex_attr(g3, "log_num_users")
plot.igraph(g3, vertex.size=vertex_size, vertex.label.cex=0.5, edge.width=0.6, edge.arrow.size=0.1, vertex.color=vertex_colors)

vertex_size = (1.6 ^ vertex_attr(g3, "log_num_users"))
plot.igraph(g3, vertex.size=vertex_size, vertex.label.cex=0.5, edge.width=0.6, edge.arrow.size=0.1, vertex.color=vertex_colors)

vertex_size = vertex_attr(g3, "log_volume_fixed_100days")
plot.igraph(g3, vertex.size=vertex_size, vertex.label.cex=0.5, edge.width=0.6, edge.arrow.size=0.1, vertex.color=vertex_colors)

# assortativity based on number of users
assortativity(g3, types1 = vertex_attr(g3, "log_num_users"), directed = TRUE)
vertex_colors = palt(fine)[cut(vertex_attr(g3, "log_num_users"), breaks=fine)]
plot.igraph(g3, vertex.size=10, vertex.label.cex=0.5, edge.width=0.6, edge.arrow.size=0.1, layout=layout_with_fr, vertex.color=vertex_colors)


################# PREDICT BASED ON NEIGHBOR #################
neighbor_attr_mean = function(vertex_name, g, v_attribute, e_attribute) {
  if(is_directed(g)) {
    vertices = neighbors(g, vertex_name, mode="in")
  } else {
    vertices = neighbors(g, vertex_name, mode="all")
  }
  if(length(vertices) == 0) {
    return(NA) 
  }
  
  tmp = function(x) return(get.edge.ids(g, c(x, vertex_name), directed=TRUE))
  edge_ids = sapply(vertices$name, tmp)
  edges = E(g)[edge_ids]
  edges_weights = edge_attr(g, e_attribute, edges)
  vertices_attrs = vertex_attr(g, v_attribute, vertices)
  return(weighted.mean(vertices_attrs, edges_weights))
  
#  if(is_directed(g)) {
#    edges = incident(g, vertex_name, mode="in")
#  } else {
#    edges = incident(g, vertex_name, mode="all")
#  }
#  if(length(edges) == 0) {
#    return(NA) 
#  }
#  edges_weights = edge_attr(g, e_attribute, edges)
  # get incoming neighbors
#  vertices = ends(g, edges)[,1]
#  vertices_attrs = vertex_attr(g, v_attribute, vertices)
#  return(weighted.mean(vertices_attrs, edges_weights))
}

# plot neighbor average of vertex attribute vs actual value
actual_vals = vertex_attr(g3, v_attribute)
neighbor_means = sapply(V(g3)$name, neighbor_attr_mean,
                        g=g3, v_attribute=v_attribute, e_attribute=e_attribute)
res = data.frame(actual_vals, neighbor_means)
ggplot(res, aes(actual_vals, neighbor_means)) +
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_smooth(method='lm')
cor(res$actual_vals, res$neighbor_means, use="pairwise.complete.obs")


# do the same on original graph, with no edges removed
actual_vals = vertex_attr(g2, v_attribute)
neighbor_means = sapply(V(g2)$name, neighbor_attr_mean,
                        g=g2, v_attribute=v_attribute, e_attribute=e_attribute)
res = data.frame(actual_vals, neighbor_means)
ggplot(res, aes(actual_vals, neighbor_means)) +
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_smooth(method='lm')
cor(res$actual_vals, res$neighbor_means, use="pairwise.complete.obs")
