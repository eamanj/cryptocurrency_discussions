library(ggplot2)
library(xtable)
library(stargazer)
library(gridExtra)
library(lubridate)

setwd(dir = "~/research/cryptocurrency_discussions/")
source("R/utils.R")

perform_regression_type1 = function(data, scale, dep_var, dep_var_description,
                                    indep_vars, indep_var_descriptions,
                                    control_vars, control_var_descriptions, output_file) {
  df = data
  if (scale) {
    df = scale_columns(df, c(dep_var, indep_vars, control_vars), rename=FALSE)
  }
  models = list()
  num_obs = c()
  # iteratively include more vars
  for(i in 1:length(indep_vars)) {
    vars = indep_vars[1:i]
    formula = paste(dep_var, "~", paste(vars, collapse=" + "), sep = " ")
    lmfit = lm(formula, df)
    
    models[[i]] = lmfit
    num_obs = c(num_obs, length(df[,2]) - length(lmfit$na.action))
  }
  
  # include each control only once
  i = length(indep_vars)
  for(control_var in control_vars) {
    vars = c(indep_vars, control_var)
    formula = paste(dep_var, "~", paste(vars, collapse=" + "), sep = " ")
    lmfit = lm(formula, df)
   
    i = i+1 
    models[[i]] = lmfit
    num_obs = c(num_obs, length(df[,2]) - length(lmfit$na.action))
  }
  output_type = ifelse(grepl("\\.tex", output_file), "latex", "html")
  a = capture.output(
    stargazer(as.list(models), type=output_type,
              out = output_file,
              report=('vc*p'),
              omit = c("Constant"),
              covariate.labels = c(indep_var_descriptions, control_var_descriptions),
              dep.var.labels.include = F,
              dep.var.caption = dep_var_description,
              star.cutoffs = c(0.05, 0.01, 0.005)))
              #add.lines = list(
              #  c("Observations", num_obs))))
}

perform_regression = function(data_sets, scale,
                              dep_var, dep_var_description,
                              indep_vars, indep_var_descriptions,
                              control_vars, control_var_descriptions,
                              column_labs,
                              output_file) {
  models = list()
  num_obs = c()
  m_id = 1
  column_sep = c()
  for(i in seq(1:length(data_sets))) {
    df = data_sets[[i]]
    if (scale) {
      df = scale_columns(df, c(dep_var, indep_vars, control_vars), rename=FALSE)
    }
    # First only indep vars
    formula = paste(dep_var, "~", paste(indep_vars, collapse=" + "), sep = " ")
    lmfit = lm(formula, df)
    models[[m_id]] = lmfit
    m_id = m_id + 1
    num_obs = c(num_obs, length(df[,2]) - length(lmfit$na.action))
    
    # Now add controls
    formula = paste(dep_var, "~", paste(c(indep_vars, control_vars), collapse=" + "), sep = " ")
    lmfit = lm(formula, df)
    print(summary(lmfit))
     
    models[[m_id]] = lmfit
    m_id = m_id + 1
    num_obs = c(num_obs, length(df[,2]) - length(lmfit$na.action))
    
    column_sep = c(column_sep, 2)
  }
  output_type = ifelse(grepl("\\.tex", output_file), "latex", "html")
  a = capture.output(
    stargazer(as.list(models), type=output_type,
              out = output_file,
              report=('vc*p'),
              omit = c("Constant"),
              covariate.labels = c(indep_var_descriptions, control_var_descriptions),
              dep.var.labels.include = F,
              dep.var.caption = dep_var_description,
              column.separate = column_sep,
              column.labels = column_labs,
              column.sep.width = "0.01pt",
              star.cutoffs = c(0.05, 0.01, 0.005)))
              #add.lines = list(
              #  c("Observations", num_obs))))
}




data0 = read.csv("data/final_prices_2016_11_100days_threads_2016_11_300days.csv")
data1 = read.csv("data/final_prices_2016_11_100days_threads_2016_11_200days.csv")
data2 = read.csv("data/final_prices_2016_01_100days_threads_2016_01_200days.csv")
data3 = read.csv("data/final_prices_initial_100days_threads_initial_2000days.csv")
data = data1
cor(data[,c("user_announcement_posts_entropy_normalized", "log_num_unique_threads", "user_ages_mean", "num_unique_announcement_threads")],
    data$log_price_mean_volatility_fixed_100days, method="spearman")
cor(data[,c("user_announcement_posts_entropy_normalized", "log_num_unique_threads", "user_ages_mean", "num_unique_announcement_threads")],
    data$log_price_mean_volatility_fixed_100days, method="pearson")


################################### DESRIPTIVE DATA ###############################
point_size = 5
axis_size = 52
text_size = 3
pdf_width = 8
pdf_height = 7

df = data1
p = ggplot(data=df, aes(user_announcement_posts_entropy_normalized)) + 
  geom_histogram(bins=25, fill="black", col="white") +
  xlab("Normalized Entropy") + ylab("Count") +
  xlim(c(0.6,0.95)) +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 6, 9, 5.5), "pt")) +
  theme(axis.text=element_text(colour="black", size=axis_size),
        axis.title=element_text(colour="black", size=axis_size))
p
ggsave(filename = "~/Desktop/hist_posts_entropy_d1.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(data=df, aes(log_num_unique_threads)) + 
  geom_histogram(bins=25, fill="black", col="white") +
  labs(x="n", y="Count") + 
  xlab("Log Degree") + ylab("Count") +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 6, 9, 5.5), "pt")) +
  theme(axis.text=element_text(colour="black", size=axis_size),
        axis.title=element_text(colour="black", size=axis_size))
p
ggsave(filename = "~/Desktop/hist_log_num_unique_threads_d1.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(data=df, aes(user_ages_mean)) + 
  geom_histogram(bins=25, fill="black", col="white") +
  labs(x="n", y="Count") + 
  xlab("Users Average Age") + ylab("Count") +
  xlim(c(130, 700)) +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 6, 9, 5.5), "pt")) +
  theme(axis.text=element_text(colour="black", size=axis_size),
        axis.title=element_text(colour="black", size=axis_size))
p
ggsave(filename = "~/Desktop/hist_user_ages_mean_d1.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(data=df, aes(log_volume_mean_mean_fixed_100days)) + 
  geom_histogram(bins=25, fill="black", col="white") +
  labs(x="n", y="Count") + 
  xlab("Log Average Volume") + ylab("Count") +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 6, 9, 5.5), "pt")) +
  theme(axis.text=element_text(colour="black", size=axis_size),
        axis.title=element_text(colour="black", size=axis_size))
p
ggsave(filename = "~/Desktop/hist_log_mean_volume_d1.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(data=df, aes(age)) + 
  geom_histogram(bins=25, fill="black", col="white") +
  labs(x="n", y="Count") + 
  xlab("Coin Age") + ylab("Count") +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 6, 9, 5.5), "pt")) +
  xlim(50, 1400) +
  theme(axis.text=element_text(colour="black", size=axis_size),
        axis.title=element_text(colour="black", size=axis_size))
p
ggsave(filename = "~/Desktop/hist_coin_age_d1.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(data=df, aes(log_user_num_posts)) + 
  geom_histogram(bins=25, fill="black", col="white") +
  labs(x="n", y="Count") + 
  xlab("Log Users Num. Posts") + ylab("Count") +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 6, 9, 5.5), "pt")) +
  theme(axis.text=element_text(colour="black", size=axis_size),
        axis.title=element_text(colour="black", size=axis_size))
p
ggsave(filename = "~/Desktop/hist_log_num_posts_d1.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")





# DESIGN 3
df = data3
p = ggplot(data=df, aes(user_announcement_posts_entropy_normalized)) + 
  geom_histogram(bins=30, fill="black", col="white") +
  xlab("Normalized Entropy") + ylab("Count") +
  xlim(c(0.645,1.0001)) +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 6, 9, 5.5), "pt")) +
  theme(axis.text=element_text(colour="black", size=axis_size),
        axis.title=element_text(colour="black", size=axis_size))
p
ggsave(filename = "~/Desktop/hist_posts_entropy_d3.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(data=df, aes(log_num_unique_threads)) + 
  geom_histogram(bins=30, fill="black", col="white") +
  labs(x="n", y="Count") + 
  xlab("Log Degree") + ylab("Count") +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 6, 9, 5.5), "pt")) +
  theme(axis.text=element_text(colour="black", size=axis_size),
        axis.title=element_text(colour="black", size=axis_size))
p
ggsave(filename = "~/Desktop/hist_log_num_unique_threads_d3.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(data=df, aes(user_ages_mean)) + 
  geom_histogram(bins=30, fill="black", col="white") +
  labs(x="n", y="Count") + 
  xlab("Users Average Age") + ylab("Count") +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 6, 9, 5.5), "pt")) +
  theme(axis.text=element_text(colour="black", size=axis_size),
        axis.title=element_text(colour="black", size=axis_size))
p
ggsave(filename = "~/Desktop/hist_user_ages_mean_d3.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(data=df, aes(log_volume_mean_mean_initial_100days)) + 
  geom_histogram(bins=30, fill="black", col="white") +
  labs(x="n", y="Count") + 
  xlab("Log Average Volume") + ylab("Count") +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 6, 9, 5.5), "pt")) +
  theme(axis.text=element_text(colour="black", size=axis_size),
        axis.title=element_text(colour="black", size=axis_size))
p
ggsave(filename = "~/Desktop/hist_log_mean_volume_d3.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(data=df, aes(age)) + 
  geom_histogram(bins=30, fill="black", col="white") +
  labs(x="n", y="Count") + 
  xlab("Coin Age") + ylab("Count") +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 6, 9, 5.5), "pt")) +
  #xlim(50, 1400) +
  theme(axis.text=element_text(colour="black", size=axis_size),
        axis.title=element_text(colour="black", size=axis_size))
p
ggsave(filename = "~/Desktop/hist_coin_age_d3.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(data=df, aes(log_user_num_posts)) + 
  geom_histogram(bins=30, fill="black", col="white") +
  labs(x="n", y="Count") + 
  xlab("Log Users Num. Posts") + ylab("Count") +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 6, 9, 5.5), "pt")) +
  theme(axis.text=element_text(colour="black", size=axis_size),
        axis.title=element_text(colour="black", size=axis_size))
p
ggsave(filename = "~/Desktop/hist_log_num_posts_d3.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")


# correlation matrics
df = data1
indep_vars = c("user_announcement_posts_entropy_normalized", "log_num_unique_threads", "user_ages_mean")
indep_vars_desc = c("Posts Entropy", "Log Degree", "User Ages")
control_vars = c("log_user_num_posts", "age", "log_volume_mean_mean_fixed_100days")
control_vars_desc = c("Log Num Posts", "Coin Age", "Log Volume")
df = df[,c(indep_vars, control_vars)]
colnames(df) = c(indep_vars_desc, control_vars_desc)
cor_matrix = round(cor(df[,c(indep_vars_desc, control_vars_desc)], method="spearman"), 2)
a = capture.output(stargazer(cor_matrix, type="latex",
                             out = "~/Desktop/correlation_table_d1.tex"))

df = data3
indep_vars = c("user_announcement_posts_entropy_normalized", "log_num_unique_threads", "log_user_ages_mean")
indep_vars_desc = c("Posts Entropy", "Log Degree", "User Ages")
control_vars = c("log_user_num_posts", "age", "log_volume_mean_mean_initial_100days")
control_vars_desc = c("Log Num Posts", "Coin Age", "Log Volume")
df = df[,c(indep_vars, control_vars)]
colnames(df) = c(indep_vars_desc, control_vars_desc)
cor_matrix = round(cor(df[,c(indep_vars_desc, control_vars_desc)], method="spearman"), 2)
a = capture.output(stargazer(cor_matrix, type="latex",
                             out = "~/Desktop/correlation_table_d3.tex"))

################################### FIXED PERIOD ###############################
# final plots
point_size = 5.5
axis_size = 25
text_size = 20
pdf_width = 7
pdf_height = 6

df = data1
df = bin_variables(df, c("age", "log_volume_mean_mean_fixed_100days", "log_user_num_posts"), 4)
p = ggplot(df, aes(user_announcement_posts_entropy_normalized, log_price_mean_volatility_fixed_100days)) +
  geom_point(aes(colour=log_volume_mean_mean_fixed_100days_binned), size=point_size+1, shape=18) +
  scale_colour_gradient(low="blue", high="red") +
  labs(color="Volume") +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Normalized Entropy of Announcement Posts") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size),
        legend.title=element_text(size=text_size),
        legend.key.width=unit(2, "line"),
        legend.key.height=unit(2, "line"))
p
ggsave(filename = "~/Desktop/log_volatility_vs_posts_entropy_d1.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(log_num_unique_threads, log_price_mean_volatility_fixed_100days)) +
  geom_point(aes(colour=log_user_num_posts_binned), size=point_size+1, shape=18) +
  scale_colour_gradient(low="yellow2", high="#4d2600") +
  labs(color="Num\nPosts") +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Log Degree of the Announcement Thread") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size),
        legend.key.width=unit(2, "line"),
        legend.key.height=unit(2, "line"))
p
ggsave(filename = "~/Desktop/log_volatility_vs_log_num_unique_threads_d1.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(user_ages_mean, log_price_mean_volatility_fixed_100days)) +
  geom_point(aes(colour=age_binned), size=point_size+1, shape=18) +
  #scale_colour_gradient(low="blue", high="red") +
  scale_colour_gradient(low="purple3", high="green") +
  labs(color="Coin\nAge") +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Average Age of Users in Days") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size),
        legend.key.width=unit(2, "line"),
        legend.key.height=unit(2, "line"))
p
ggsave(filename = "~/Desktop/log_volatility_vs_user_ages_mean_d1.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")



df = data2
df = bin_variables(df, c("age", "log_volume_mean_mean_fixed_50days", "log_user_num_posts"), 4)
p = ggplot(df, aes(user_announcement_posts_entropy_normalized, log_price_mean_volatility_fixed_50days)) +
  geom_point(aes(colour=log_volume_mean_mean_fixed_50days_binned), size=point_size) +
  scale_colour_gradient(low="blue", high="red") +
  labs(color="Volume") +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Normalized Entropy of Announcement Posts") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size),
        legend.key.width=unit(2, "line"),
        legend.key.height=unit(2, "line"))
p
ggsave(filename = "~/Desktop/log_volatility_vs_posts_entropy_d2.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(log_num_unique_threads, log_price_mean_volatility_fixed_50days)) +
  geom_point(aes(colour=log_user_num_posts_binned), size=point_size) +
  scale_colour_gradient(low="yellow2", high="#4d2600") +
  labs(color="Num\nPosts") +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Log Degree of the Announcement Thread") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size), 
        legend.key.width=unit(2, "line"),
        legend.key.height=unit(2, "line"))
p
ggsave(filename = "~/Desktop/log_volatility_vs_log_num_unique_threads_d2.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(user_ages_mean, log_price_mean_volatility_fixed_50days)) +
  geom_point(aes(colour=age_binned), size=point_size) +
  scale_colour_gradient(low="purple3", high="green") +
  labs(color="Coin\nAge") +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Average Age of Users in Days") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size), 
        legend.key.width=unit(2, "line"),
        legend.key.height=unit(2, "line"))
p
ggsave(filename = "~/Desktop/log_volatility_vs_user_ages_mean_d2.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")


df = data3
df = bin_variables(df, c("age", "log_volume_mean_mean_initial_100days", "log_user_num_posts",
                         "user_announcement_posts_entropy_normalized", "first_1_posters_fraction_posts"), 4)
p = ggplot(df, aes(user_announcement_posts_entropy_normalized, log_price_mean_volatility_initial_100days)) +
  geom_point(aes(colour=log_volume_mean_mean_initial_100days_binned), size=point_size) +
  #geom_point(aes(colour=log_user_num_posts_binned), size=point_size) +
  scale_colour_gradient(low="blue", high="red") +
  labs(color="Volume") +
  ylim(-3.1,-0.1) +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Normalized Entropy of Announcement Posts") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size), 
        legend.key.width=unit(2, "line"),
        legend.key.height=unit(2, "line"))
p
ggsave(filename = "~/Desktop/log_volatility_vs_posts_entropy_d3.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(first_1_posters_fraction_posts, user_announcement_posts_entropy_normalized)) +
  geom_point(aes(colour=log_volume_mean_mean_initial_100days_binned), size=point_size) +
  #geom_point(aes(colour=log_user_num_posts_binned), size=point_size) +
  scale_colour_gradient(low="blue", high="red") +
  labs(color="Volume") +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Fraction of Posts by Developer") + ylab("Normalized Entropy of Announcement Posts") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size), 
        legend.key.width=unit(2, "line"),
        legend.key.height=unit(2, "line"))
p
ggsave(filename = "~/Desktop/posts_entropy_vs_first_1_poster_fraction_d3.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(log_num_unique_threads, log_price_mean_volatility_initial_100days)) +
  geom_point(aes(colour=log_user_num_posts_binned), size=point_size) +
  scale_colour_gradient(low="yellow2", high="#4d2600") +
  labs(color="Num\nPosts") +
  ylim(-3.1,-0.1) +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Log Degree of the Announcement Thread") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size), 
        legend.key.width=unit(2, "line"),
        legend.key.height=unit(2, "line"))
p
ggsave(filename = "~/Desktop/log_volatility_vs_log_num_unique_threads_d3.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(log_user_ages_mean, log_price_mean_volatility_initial_100days)) +
  geom_point(aes(colour=age_binned), size=point_size) +
  scale_colour_gradient(low="purple3", high="green") +
  labs(color="Coin\nAge") +
  ylim(-3.1,-0.3) +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Log Average Age of Users in Days") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size),
        legend.key.width=unit(2, "line"),
        legend.key.height=unit(2, "line"))
p
ggsave(filename = "~/Desktop/log_volatility_vs_user_ages_mean_d3.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")


################################### TABLE ###############################
perform_regression_type1(data,
                         FALSE,
                         "log_price_mean_volatility_fixed_100days",
                         "Price Volatility over 100 Days",
                         c("user_ages_mean",
                           "user_announcement_posts_entropy_normalized",
                           "log_num_unique_threads",
                           "age", "log_volume_mean_mean_fixed_100days", 
                           "log_user_num_posts"),
                         c("Average Age of Users",
                           "Entropy of Number of Posts",
                           "Log Degree in Thread Network",
                           "Age of the Coin",
                           "Log Average Daily Volume",
                           "Log Number of Posts by Users"),
                         c(),
                         c(),
                          "~/Desktop/regression_table_type1.html")

df1 = data1
df1$volatility  = df1$log_price_mean_volatility_fixed_100days
df1$user_age = df1$user_ages_mean
df1$entropy = df1$user_announcement_posts_entropy_normalized
df1$degree = df1$log_num_unique_threads
df1$coin_age = df1$age
df1$volume = df1$log_volume_mean_mean_fixed_100days
df1$num_posts = df1$log_user_num_posts

df2 = data2
df2$volatility  = df2$log_price_mean_volatility_fixed_50days
df2$user_age = df2$user_ages_mean
df2$entropy = df2$user_announcement_posts_entropy_normalized
df2$degree = df2$log_num_unique_threads
df2$coin_age = df2$age
df2$volume = df2$log_volume_mean_mean_fixed_50days
df2$num_posts = df2$log_user_num_posts

df3 = data3
df3$volatility  = df3$log_price_mean_volatility_initial_100days
df3$user_age = df3$log_user_ages_mean
df3$entropy = df3$user_announcement_posts_entropy_normalized
df3$degree = df3$log_num_unique_threads
df3$coin_age = df3$age
df3$volume = df3$log_volume_mean_mean_initial_100days
df3$num_posts = df3$log_user_num_posts

data_sets = list(df1, df2, df3)

perform_regression(data_sets,
                   TRUE,
                   "volatility",
                   "Price Volatility over 100 Days",
                   c("user_age", "entropy", "degree"),
                   c("Average User Age", "Number of Posts Entropy", "Thread Network Degree"),
                   c("coin_age", "volume", "num_posts"),
                   c("Coin Age", "Average Daily Volume", "Number of Posts"),
                   c("November 2016", "January 2016", "Initial Trading"),
                    "~/Desktop/regression_table.html")

# how does the result look like for model 3 when we remove first user and measure entropy?
df3$entropy = df3$rest_1_posters_posts_entropy_normalized
data_sets = list(df1, df2, df3)
perform_regression(data_sets,
                   TRUE,
                   "volatility",
                   "Price Volatility over 100 Days",
                   c("user_age", "entropy", "degree"),
                   c("Average User Age", "Number of Posts Entropy", "Thread Network Degree"),
                   c("coin_age", "volume", "num_posts"),
                   c("Coin Age", "Average Daily Volume", "Number of Posts"),
                   c("November 2016", "January 2016", "Initial Trading"),
                    "~/Desktop/model_rest_1_entropy.html")


############################ CONTROL VARIABLES ########################################
point_size = 5
axis_size = 27
text_size = 3
pdf_width = 7
pdf_height = 7

df = data1
p = ggplot(df, aes(log_volume_mean_mean_fixed_100days, log_price_mean_volatility_fixed_100days)) +
  geom_point(size=point_size) +
  xlab("Log of Average Daily Volume") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size))
cor.test(df$log_volume_mean_mean_fixed_100days, df$log_price_mean_volatility_fixed_100days)
p
ggsave(filename = "~/Desktop/log_volatility_vs_log_mean_volume.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(age, log_price_mean_volatility_log_fixed_100days)) +
  geom_point(size=point_size) +
  xlab("Age of the Coin (Days)") + ylab("Log Price Volatility") +
  xlim(0,1550) +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size))
p
cor.test(df$age, df$log_price_mean_volatility_fixed_100days)
ggsave(filename = "~/Desktop/log_volatility_vs_age.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(log_user_num_posts, log_price_mean_volatility_fixed_100days)) +
  geom_point(size=point_size) +
  xlab("Log Number of Posts in Thread") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size))
p
cor.test(df$log_user_num_posts, df$log_price_mean_volatility_fixed_100days)
ggsave(filename = "~/Desktop/log_volatility_vs_log_user_num_posts.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")




############################ DIVERSITY VALIDITY ########################################
# misc plots, construct validity plots
point_size = 5
axis_size = 29.5
text_size = 3
pdf_width = 7
pdf_height = 7

df = data1
print_cors(df, c("log_num_unique_threads"), get_large_independent_vars(), method="spearman", 0.2)

vars = c("age_variation", "sentiment_variation")
df$age_variation = df$user_ages_std / df$user_ages_mean
df$sentiment_variation = df$rel_sentiment_score_std_sentimental_tokens / (df$rel_sentiment_score_mean_sentimental_tokens + 5)
plot(df$log_num_unique_threads, df$sentiment_variation)
plot(df$log_num_unique_threads, df$age_variation)
print_cors(df, c("log_num_unique_threads"), vars, method="spearman", 0.2)
print_cors(df, c("log_num_unique_threads"), vars, method="pearson", 0.2)

# crazy outliers
df_tmp = df[df$sentiment_variation < 0.28,]
#df_tmp = df
p = ggplot(df_tmp, aes(sentiment_variation, log_num_unique_threads)) +
  geom_point(size=point_size) +
  xlab("Sentiment Scores Variation Coef") + ylab("Log Degree of Thread") +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 5, 8, 5.5), "pt")) +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size))
p
cor.test(df_tmp$sentiment_variation, df_tmp$log_num_unique_threads)
ggsave(filename = "~/Desktop/num_unique_threads_vs_coef_var_sentiment.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")


# crazy outliers
df_tmp = df[df$age_variation < 2.1,]
#df_tmp = df
p = ggplot(df_tmp, aes(age_variation, log_num_unique_threads)) +
  geom_point(size=point_size) +
  xlab("User Ages Variation Coef") + ylab("Log Degree of Thread") +
  theme_bw() +
  #xlim(c(NA,1.61)) +
  theme(plot.margin=unit(c(2.5, 5, 8, 5.5), "pt")) +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size))
p
cor.test(df_tmp$age_variation, df_tmp$log_num_unique_threads)
ggsave(filename = "~/Desktop/num_unique_threads_vs_coef_var_age.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

############################ ENTROPY VALIDITY ########################################
# entropy vs fraction of posts by developer in design 3
point_size = 5
axis_size = 26
text_size = 3
pdf_width = 7
pdf_height = 6.2

df1 = data1
#df1 = data
#df1 = data1[data1$days_on_forum > 50,]
plot(df1$first_1_posters_fraction_posts, df1$user_announcement_posts_entropy_normalized)
cor.test(df1$user_announcement_posts_entropy_normalized, df1$first_1_posters_fraction_posts)
cor.test(df1$user_announcement_posts_entropy_normalized, df1$first_1_posters_fraction_posts, method="spearman")

plot(log(df1$first_1_posters_fraction_posts), df1$user_announcement_posts_entropy_normalized)
cor.test(df1$user_announcement_posts_entropy_normalized, log(df1$first_1_posters_fraction_posts))

plot(log(df1$first_1_posters_fraction_posts), df1$log_price_mean_volatility_fixed_100days)
cor.test(log(df1$first_1_posters_fraction_posts), df1$log_price_mean_volatility_fixed_100days)

plot(df1$user_announcement_posts_entropy_normalized, df1$log_price_mean_volatility_fixed_100days)
cor.test(df1$user_announcement_posts_entropy_normalized, df1$log_price_mean_volatility_fixed_100days)

plot(df1$rest_1_posters_posts_entropy_normalized, df1$log_price_mean_volatility_fixed_100days)
cor.test(df1$rest_1_posters_posts_entropy_normalized, df1$log_price_mean_volatility_fixed_100days)


df3 = data3
#df3 = df3[df3$log_price_mean_volatility_initial_100days > -3,]
plot(df3$first_1_posters_fraction_posts, df3$user_announcement_posts_entropy_normalized)
cor.test(df3$user_announcement_posts_entropy_normalized, df3$first_1_posters_fraction_posts)
cor.test(df3$user_announcement_posts_entropy_normalized, df3$first_1_posters_fraction_posts, method="spearman")

plot(log(df3$first_1_posters_fraction_posts), df3$user_announcement_posts_entropy_normalized)
cor.test(df3$user_announcement_posts_entropy_normalized, log(df3$first_1_posters_fraction_posts))

plot(log(df3$first_1_posters_fraction_posts), df3$log_price_mean_volatility_initial_100days)
cor.test(log(df3$first_1_posters_fraction_posts), df3$log_price_mean_volatility_initial_100days)

plot(df3$user_announcement_posts_entropy_normalized, df3$log_price_mean_volatility_initial_100days)
cor.test(df3$user_announcement_posts_entropy_normalized, df3$log_price_mean_volatility_initial_100days)

plot(df3$rest_1_posters_posts_entropy_normalized, df3$log_price_mean_volatility_initial_100days)
cor.test(df3$rest_1_posters_posts_entropy_normalized, df3$log_price_mean_volatility_initial_100days)





p = ggplot() +
  geom_point(aes(df1$first_1_posters_fraction_posts, df1$user_announcement_posts_entropy_normalized, color="Design 1", shape="Design 1", size="Design 1")) +
  geom_point(aes(df3$first_1_posters_fraction_posts, df3$user_announcement_posts_entropy_normalized, color="Design 3", shape="Design 3", size="Design 3")) +
  xlab("Fraction of Posts Made by Announcer") + ylab("Entropy of Posts") + 
  theme_bw() +
  #xlim(c(NA,1.61)) +
  scale_color_manual(name="", values=c("Design 1" = "red", "Design 3" = "blue")) +
  scale_size_manual(name="", values=c("Design 1" = point_size+1, "Design 3" = point_size)) +
                     #guide = guide_legend(keywidth = 1.5, size = point_size)) +
  scale_shape_manual(name="", values=c("Design 1" = 18, "Design 3" = 19)) +
  theme(plot.margin=unit(c(2.5, 5, 8, 5.5), "pt"),
        axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.text=element_text(colour="black", size=axis_size),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(t=2, r=2),
        legend.key.size = unit(0.9, 'cm'),
        legend.background = element_rect(fill=alpha(0.001)))
p
ggsave(filename = "~/Desktop/entropy_vs_announcer_fraction_posts.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot() +
  geom_point(aes(df3$rest_1_posters_posts_entropy_normalized, df3$log_price_mean_volatility_initial_100days), color="blue", size=point_size, shape=19) +
  xlab("Entropy of Posts Excluding Announcer") + ylab("Log Price Volatility") + 
  theme_bw() +
  ylim(c(-3,-0.5)) +
  theme(plot.margin=unit(c(2.5, 5, 8, 5.5), "pt"),
        axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.text=element_text(colour="black", size=axis_size),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(t=2, r=2),
        legend.key.size = unit(0.9, 'cm'),
        legend.background = element_rect(fill=alpha(0.001)))
p
ggsave(filename = "~/Desktop/entropy_excluded_vs_volatility_design3.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot() +
  geom_point(aes(df1$rest_1_posters_posts_entropy_normalized, df1$log_price_mean_volatility_fixed_100days), color="red", size=point_size+1, shape=18) +
  xlab("Entropy of Posts Excluding Announcer") + ylab("Log Price Volatility") + 
  theme_bw() +
  xlim(c(0.63,0.93)) +
  theme(plot.margin=unit(c(2.5, 5, 8, 5.5), "pt"),
        axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.text=element_text(colour="black", size=axis_size),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(t=2, r=2),
        legend.key.size = unit(0.9, 'cm'),
        legend.background = element_rect(fill=alpha(0.001)))
p
ggsave(filename = "~/Desktop/entropy_excluded_vs_volatility_design1.pdf", plot = p,
       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

####################################################################
# trivial results
df = data1
trivial_data = read.csv("data/trivial-coins.csv")
colnames(trivial_data)[1] = "symbol"
missing_df = df[!df$symbol %in% trivial_data$symbol,]
df = merge(df, trivial_data, by=c("symbol"))
df = df[!is.nan(df$log_volume_mean_mean_fixed_100days),]
df = df[!is.nan(df$price_mean_volatility_log_fixed_100days),]
df = df[is.finite(df$log_num_unique_threads),]
df = df[,c("log_price_mean_volatility_fixed_100days", "log_volume_mean_mean_fixed_100days",
            "user_announcement_posts_entropy_normalized",  "log_num_unique_threads",
            "log_user_ages_mean", "user_ages_mean", "age", "log_user_num_posts", "trivial.completed")]
print(paste('There are', nrow(df[df$trivial.completed == 1,]), 'non-technical coins'))
print(paste('There are', nrow(df[df$trivial.completed == 0,]), 'technical coins'))

colMeans(df[df$trivial.completed == 1,
            c("log_price_mean_volatility_fixed_100days", "log_volume_mean_mean_fixed_100days",
              "user_announcement_posts_entropy_normalized",  "log_num_unique_threads",
              "user_ages_mean", "age", "log_user_num_posts")])
colMeans(df[df$trivial.completed == 0,
            c("log_price_mean_volatility_fixed_100days", "log_volume_mean_mean_fixed_100days",
              "user_announcement_posts_entropy_normalized",  "log_num_unique_threads",
              "user_ages_mean", "age", "log_user_num_posts")])

trivial_df = df[df$trivial.completed == 1,]
nontrivial_df = df[df$trivial.completed == 0,]

tests = list()
test = t.test(nontrivial_df$log_price_mean_volatility_fixed_100days,
       trivial_df$log_price_mean_volatility_fixed_100days,
       alternative = "less", var.equal = FALSE, conf.level = 0.975)
tests[["volatility"]] = test
test

test = t.test(nontrivial_df$log_volume_mean_mean_fixed_100days,
              trivial_df$log_volume_mean_mean_fixed_100days,
              alternative = "greater", var.equal = FALSE, conf.level = 0.75)
tests[["volume"]] = test
test

test = t.test(nontrivial_df$user_announcement_posts_entropy_normalized,
              trivial_df$user_announcement_posts_entropy_normalized,
              alternative = "greater",var.equal = FALSE, conf.level = 0.975)
tests[["entropy"]] = test
test

test = t.test(nontrivial_df$log_num_unique_threads,
              trivial_df$log_num_unique_threads,
              alternative = "greater", var.equal = FALSE, conf.level = 0.975)
tests[["degree"]] = test
test

test = t.test(nontrivial_df$user_ages_mean,
              trivial_df$user_ages_mean,
              alternative = "greater", var.equal = FALSE, conf.level = 0.975)
tests[["user_ages"]] = test
test

test = t.test(nontrivial_df$age,
              trivial_df$age,
              alternative = "greater", var.equal = FALSE, conf.level = 0.975)
tests[["coin_age"]] = test
test

test = t.test(nontrivial_df$log_user_num_posts,
              trivial_df$log_user_num_posts,
              alternative = "greater", var.equal = FALSE, conf.level = 0.975)
tests[["num_posts"]] = test
test

num_sigfigs = 3
diffs = c(tests[["volatility"]]$estimate[1] - tests[["volatility"]]$estimate[2],
          tests[["volume"]]$estimate[1] - tests[["volume"]]$estimate[2],
          tests[["entropy"]]$estimate[1] - tests[["entropy"]]$estimate[2],
          tests[["degree"]]$estimate[1] - tests[["degree"]]$estimate[2],
          tests[["user_ages"]]$estimate[1] - tests[["user_ages"]]$estimate[2],
          tests[["coin_age"]]$estimate[1] - tests[["coin_age"]]$estimate[2],
          tests[["num_posts"]]$estimate[1] - tests[["num_posts"]]$estimate[2])
pvals = c(tests[["volatility"]]$p.value,
          tests[["volume"]]$p.value,
          tests[["entropy"]]$p.value,
          tests[["degree"]]$p.value,
          tests[["user_ages"]]$p.value,
          tests[["coin_age"]]$p.value,
          tests[["num_posts"]]$p.value)
ci_lows = c(tests[["volatility"]]$conf.int[1],
            tests[["volume"]]$conf.int[1],
            tests[["entropy"]]$conf.int[1],
            tests[["degree"]]$conf.int[1],
            tests[["user_ages"]]$conf.int[1],
            tests[["coin_age"]]$conf.int[1],
            tests[["num_posts"]]$conf.int[1])
ci_highs = c(tests[["volatility"]]$conf.int[2],
             tests[["volume"]]$conf.int[2],
             tests[["entropy"]]$conf.int[2],
             tests[["degree"]]$conf.int[2],
             tests[["user_ages"]]$conf.int[2],
             tests[["coin_age"]]$conf.int[2],
             tests[["num_posts"]]$conf.int[2])
cis = paste0("(", round(ci_lows, num_sigfigs), ",", round(ci_highs, num_sigfigs), ")")

labels = c("Price Volatility", "Log Daily Volume", "Posts Entropy",
           "Log Degree", "User Ages", "Coin Age", "Log Total Posts")

          
df_table = matrix(0, nrow=3, ncol=7,
                  dimnames = list(c('Means Difference', 'H0 P-Value', '97.5% CI'),
                                  c('Price Volatility', 'Log Daily Volume', 'Posts Entropy',
                                    'Log Degree', 'User Ages', 'Coin Age', 'Log Total Posts')))
df_table[1,] = round(diffs, num_sigfigs)
df_table[2,] = round(pvals, 7)
df_table[3,] = cis
df_table
print(xtable(df_table, caption="t-Tests of technical vs. non-technical coins"),
      file="~/Desktop/t_tests_table.tex")

# manova analysis of technical coins
full_triviality_df = rbind(trivial_df, nontrivial_df)
thread_vars_data = cbind(full_triviality_df$user_announcement_posts_entropy_normalized,
                         full_triviality_df$log_num_unique_threads,
                         full_triviality_df$user_ages_mean)
manova_fit = manova(thread_vars_data ~ full_triviality_df$trivial.completed)
summary(manova_fit)

################################### ROBUSTNESS OF PERIOD ###############################
summary(lm(data0$log_price_mean_volatility_fixed_200days ~ data0$user_ages_mean + data0$user_announcement_posts_entropy_normalized +
           data0$log_num_unique_threads + data0$age + data0$log_volume_mean_mean_fixed_200days + data0$log_user_num_posts))
summary(lm(data3$log_price_mean_volatility_initial_50days ~ data3$user_ages_mean + data3$user_announcement_posts_entropy_normalized +
           data3$log_num_unique_threads + data3$age + data3$log_volume_mean_mean_initial_50days + data3$log_user_num_posts))

################################### ROBUSTNESS OF DATA FILTERING ###############################

############# filtering of first design
# loose filtering
data1 = read.csv("data/unfiltered_final_prices_2016_11_100days_threads_2016_11_200days.csv")
period_type = "fixed"
prices_earliest_trade_shift = 0
threads_earliest_trade_shift = 0
currency = 'usd'
threads_period_length = 200
min_days_on_forum = -200
threads_participation_absolute_threshold = 0
threads_participation_quantile_threshold = 0
min_volume_days = 100
min_volume = 3
min_trades = 0
min_num_posts = 5
max_normalized_volume_volatility = 200
not_na_var = "log_price_mean_volatility_fixed_100days"
max_not_na_var = 200
positive_vars = c("volume_mean_mean_fixed_50days")
btc_rel = FALSE
normalization_period_length = 180
prices_enddate = "2016-11-01"
threads_enddate = as.Date(prices_enddate, format="%Y-%m-%d") %m-% months(0)
threads_enddate = as.character(threads_enddate)
data1 = filter_data(data1, min_trades, min_volume, min_volume_days,
                    min_num_posts, period_type, min_days_on_forum, max_normalized_volume_volatility,
                    not_na_var, max_not_na_var, positive_vars)
# strong filtering
data2 = read.csv("data/unfiltered_final_prices_2016_11_100days_threads_2016_11_200days.csv")
period_type = "fixed"
prices_earliest_trade_shift = 0
threads_earliest_trade_shift = 0
currency = 'usd'
threads_period_length = 200
min_days_on_forum = -200
threads_participation_absolute_threshold = 0
threads_participation_quantile_threshold = 0
min_volume_days = 100
min_volume = 75
min_trades = 0
min_num_posts = 75
max_normalized_volume_volatility = 200
not_na_var = "log_price_mean_volatility_fixed_100days"
max_not_na_var = 200
positive_vars = c("volume_mean_mean_fixed_50days")
btc_rel = FALSE
normalization_period_length = 180
prices_enddate = "2016-11-01"
threads_enddate = as.Date(prices_enddate, format="%Y-%m-%d") %m-% months(0)
threads_enddate = as.character(threads_enddate)
data2 = filter_data(data2, min_trades, min_volume, min_volume_days,
                    min_num_posts, period_type, min_days_on_forum, max_normalized_volume_volatility,
                    not_na_var, max_not_na_var, positive_vars)

# table
df1 = data1
df1$volatility  = df1$log_price_mean_volatility_fixed_100days
df1$user_age = df1$user_ages_mean
df1$entropy = df1$user_announcement_posts_entropy_normalized
df1$degree = df1$log_num_unique_threads
df1$coin_age = df1$age
df1$volume = df1$log_volume_mean_mean_fixed_100days
df1$num_posts = df1$log_user_num_posts

df2 = data2
df2$volatility  = df2$log_price_mean_volatility_fixed_100days
df2$user_age = df2$user_ages_mean
df2$entropy = df2$user_announcement_posts_entropy_normalized
df2$degree = df2$log_num_unique_threads
df2$coin_age = df2$age
df2$volume = df2$log_volume_mean_mean_fixed_100days
df2$num_posts = df2$log_user_num_posts

data_sets = list(df1, df2)
min(df1$volume_mean_mean_fixed_100days)
min(df1$user_num_posts)
min(df2$volume_mean_mean_fixed_100days)
min(df2$user_num_posts)

perform_regression(data_sets,
                   TRUE,
                   "volatility",
                   "Price Volatility over 100 Days",
                   c("user_age", "entropy", "degree"),
                   c("Average User Age", "Number of Posts Entropy", "Thread Network Degree"),
                   c("coin_age", "volume", "num_posts"),
                   c("Coin Age", "Average Daily Volume", "Number of Posts"),
                   c("Loose Filtering", "Strict Filtering"),
                    "~/Desktop/regression_table_robustness_d1.tex")




############# filtering of 3rd design
# loose filtering
data1 = read.csv("data/unfiltered_final_prices_initial_100days_threads_initial_2000days.csv")
period_type = "initial"
prices_earliest_trade_shift = 0
threads_earliest_trade_shift = 0
currency = 'usd'
threads_period_length = 2000
min_days_on_forum = 0
threads_participation_absolute_threshold = 0
threads_participation_quantile_threshold = 0
min_volume_days = 100
min_volume = 20
min_trades = 0
min_num_posts = 20
max_normalized_volume_volatility = 400
not_na_var = "log_price_mean_volatility_initial_100days"
max_not_na_var = 100
positive_vars = c("volume_mean_mean_initial_50days")
btc_rel = FALSE
normalization_period_length = 180
prices_enddate = "2016-11-01"
threads_enddate = "earliesttrade"
data1 = filter_data(data1, min_trades, min_volume, min_volume_days,
                    min_num_posts, period_type, min_days_on_forum, max_normalized_volume_volatility,
                    not_na_var, max_not_na_var, positive_vars)

# strict filtering
data2 = read.csv("data/unfiltered_final_prices_initial_100days_threads_initial_2000days.csv")
period_type = "initial"
prices_earliest_trade_shift = 0
threads_earliest_trade_shift = 0
currency = 'usd'
threads_period_length = 2000
min_days_on_forum = 0
threads_participation_absolute_threshold = 0
threads_participation_quantile_threshold = 0
min_volume_days = 100
min_volume = 150
min_trades = 0
min_num_posts = 100
max_normalized_volume_volatility = 400
not_na_var = "log_price_mean_volatility_initial_100days"
max_not_na_var = 100
positive_vars = c("volume_mean_mean_initial_50days")
btc_rel = FALSE
normalization_period_length = 180
prices_enddate = "2016-11-01"
threads_enddate = "earliesttrade"
data2 = filter_data(data2, min_trades, min_volume, min_volume_days,
                    min_num_posts, period_type, min_days_on_forum, max_normalized_volume_volatility,
                    not_na_var, max_not_na_var, positive_vars)

# table
df1 = data1
df1$volatility  = df1$log_price_mean_volatility_initial_100days
df1$user_age = df1$log_user_ages_mean
df1$entropy = df1$user_announcement_posts_entropy_normalized
df1$degree = df1$log_num_unique_threads
df1$coin_age = df1$age
df1$volume = df1$log_volume_mean_mean_initial_100days
df1$num_posts = df1$log_user_num_posts

df2 = data2
df2$volatility  = df2$log_price_mean_volatility_initial_100days
df2$user_age = df2$log_user_ages_mean
df2$entropy = df2$user_announcement_posts_entropy_normalized
df2$degree = df2$log_num_unique_threads
df2$coin_age = df2$age
df2$volume = df2$log_volume_mean_mean_initial_100days
df2$num_posts = df2$log_user_num_posts

data_sets = list(df1, df2)
min(df1$volume_mean_mean_initial_100days)
min(df1$user_num_posts)
min(df2$volume_mean_mean_initial_100days)
min(df2$user_num_posts)

perform_regression(data_sets,
                   TRUE,
                   "volatility",
                   "Price Volatility over 100 Days",
                   c("user_age", "entropy", "degree"),
                   c("Average User Age", "Number of Posts Entropy", "Thread Network Degree"),
                   c("coin_age", "volume", "num_posts"),
                   c("Coin Age", "Average Daily Volume", "Number of Posts"),
                   c("Loose Filtering", "Strict Filtering"),
                    "~/Desktop/regression_table_robustness_d3.tex")







################# older version of final plots with all points the same shape and color for all vars/designs
point_size = 5
axis_size = 25
text_size = 14
pdf_width = 7
pdf_height = 6

df = data1
df = bin_variables(df, c("age", "log_volume_mean_mean_fixed_100days", "log_user_num_posts"), 4)
p = ggplot(df, aes(user_announcement_posts_entropy_normalized, log_price_mean_volatility_fixed_100days)) +
  geom_point(aes(colour=log_volume_mean_mean_fixed_100days_binned), size=point_size) +
  scale_colour_gradient(low="blue", high="red") +
  labs(color="Volume") +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Normalized Entropy of Announcement Posts") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        #legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size),
        legend.title=element_text(size=text_size)) 
p
#ggsave(filename = "~/Desktop/log_volatility_vs_posts_entropy_d1.pdf", plot = p,
#       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(log_num_unique_threads, log_price_mean_volatility_fixed_100days)) +
  geom_point(aes(colour=log_user_num_posts_binned), size=point_size) +
  scale_colour_gradient(low="blue", high="red") +
  labs(color="Num Posts") +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Log Degree of the Announcement Thread") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size)) 
p
#ggsave(filename = "~/Desktop/log_volatility_vs_log_num_unique_threads_d1.pdf", plot = p,
#       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(user_ages_mean, log_price_mean_volatility_fixed_100days)) +
  geom_point(aes(colour=age_binned), size=point_size) +
  scale_colour_gradient(low="blue", high="red") +
  labs(color="Coin Age") +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Average Age of Users in Days") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size)) 
p
#ggsave(filename = "~/Desktop/log_volatility_vs_user_ages_mean_d1.pdf", plot = p,
#       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")





df = data3
df = bin_variables(df, c("age", "log_volume_mean_mean_initial_100days", "log_user_num_posts",
                         "user_announcement_posts_entropy_normalized", "first_1_posters_fraction_posts"), 4)
p = ggplot(df, aes(user_announcement_posts_entropy_normalized, log_price_mean_volatility_initial_100days)) +
  geom_point(aes(colour=log_volume_mean_mean_initial_100days_binned), size=point_size) +
  #geom_point(aes(colour=log_user_num_posts_binned), size=point_size) +
  scale_colour_gradient(low="blue", high="red") +
  labs(color="Volume") +
  ylim(-3.1,-0.1) +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Normalized Entropy of Announcement Posts") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        #legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size)) 
p
#ggsave(filename = "~/Desktop/log_volatility_vs_posts_entropy_d3.pdf", plot = p,
#       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(first_1_posters_fraction_posts, user_announcement_posts_entropy_normalized)) +
  geom_point(aes(colour=log_volume_mean_mean_initial_100days_binned), size=point_size) +
  #geom_point(aes(colour=log_user_num_posts_binned), size=point_size) +
  scale_colour_gradient(low="blue", high="red") +
  labs(color="Volume") +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Fraction of Posts by Developer") + ylab("Normalized Entropy of Announcement Posts") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size)) 
p
#ggsave(filename = "~/Desktop/posts_entropy_vs_first_1_poster_fraction_d3.pdf", plot = p,
#       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(log_num_unique_threads, log_price_mean_volatility_initial_100days)) +
  geom_point(aes(colour=log_user_num_posts_binned), size=point_size) +
  scale_colour_gradient(low="blue", high="red") +
  labs(color="Num Posts") +
  ylim(-3.1,-0.1) +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Log Degree of the Announcement Thread") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size)) 
p
#ggsave(filename = "~/Desktop/log_volatility_vs_log_num_unique_threads_d3.pdf", plot = p,
#       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")

p = ggplot(df, aes(log_user_ages_mean, log_price_mean_volatility_initial_100days)) +
  geom_point(aes(colour=age_binned), size=point_size) +
  scale_colour_gradient(low="blue", high="red") +
  labs(color="Coin Age") +
  ylim(-3.1,-0.3) +
  #geom_text(aes(label=symbol), size=text_size) +
  xlab("Log Average Age of Users in Days") + ylab("Log Price Volatility") +
  theme_bw() +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size),
        legend.margin=margin(l=-0.2,unit="cm"),
        legend.title.align=0.5,
        legend.text=element_text(size=text_size), 
        legend.title=element_text(size=text_size)) 
p
#ggsave(filename = "~/Desktop/log_volatility_vs_user_ages_mean_d3.pdf", plot = p,
#       device = cairo_pdf, width = pdf_width, height = pdf_height, units= "in")



#########################################
