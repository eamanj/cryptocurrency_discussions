library(lubridate)
library(ggplot2)
library(randomForest)
library(miscTools)


setwd(dir = "~/research/bitcoin/")
source("R/utils.R")

data0 = read.csv("data/final_prices_2016_11_100days_threads_2016_11_300days.csv")
data1 = read.csv("data/final_prices_2016_11_100days_threads_2016_11_200days.csv")
data2 = read.csv("data/final_prices_2016_01_100days_threads_2016_01_200days.csv")
data3 = read.csv("data/final_prices_initial_100days_threads_initial_2000days.csv")
data = data3

dep_var = "log_price_mean_volatility_initial_100days"
indep_vars = c("log_user_ages_mean", "user_announcement_posts_entropy_normalized", "log_num_unique_threads")
control_vars = c("log_user_num_posts", "log_volume_mean_mean_initial_100days", "age")
#dep_var = "log_price_mean_volatility_fixed_100days"
#indep_vars = c("user_ages_mean", "user_announcement_posts_entropy_normalized", "log_num_unique_threads")
#control_vars = c("log_user_num_posts", "log_volume_mean_mean_fixed_100days", "age")

if(btc_rel) {
  data = add_btc_normalized_dependent_vars(data, price_dependent_vars)
  price_dependent_vars = c(paste0(price_dependent_vars, "_btc_diff"), paste0(price_dependent_vars, "_btc_ratio"))
}



# divide data to train test
train_fraction = 0.7
train_size = floor(train_fraction * nrow(data))

# set the seed to make partition reproductible
#set.seed(123)
train_idx = sample(seq_len(nrow(data)), size = train_size)
train_data = data[train_idx, c(dep_var, indep_vars)]
test_data = data[-train_idx, c(dep_var, indep_vars)]
# with control vars
#train_data = data[train_idx, c(dep_var, indep_vars, control_vars)]
#test_data = data[-train_idx, c(dep_var, indep_vars, control_vars)]

rf = randomForest(as.formula(paste(dep_var, "~ .")), data=train_data, ntree=1000)
train_predict = predict(rf, train_data)
test_predict = predict(rf, test_data)

train_r2 = rSquared(train_data[,dep_var], train_data[,dep_var] - train_predict)
train_mse = mean((train_data[,dep_var] - train_predict)^2)
print(paste("Train R-squared:", round(train_r2, 3), "and MSE:", round(train_mse, 3)))

test_r2 = rSquared(test_data[,dep_var], test_data[,dep_var] - test_predict)
test_mse = mean((test_data[,dep_var] - test_predict)^2)
test_cor = cor(test_data[,dep_var], test_predict)
print(paste("Test R-squared:", round(test_r2, 3), "MSE:", round(test_mse, 3), "and pred-actual correlation:", round(test_cor, 3)))
#rf$importance
cor.test(test_data[,dep_var], predict(rf, test_data))

point_size = 5
line_size = 2
axis_size = 29.5
text_size = 3
pdf_width = 7
pdf_height = 7
p = ggplot(data=data.frame(actual=test_data[,dep_var], pred=test_predict), aes(x=actual, y=pred)) +
  geom_point(size=point_size) +
  geom_abline(color="red", size=line_size, linetype="dashed") +
  xlab("Actual Volatility") + ylab("Predicted Volatility") +
  theme_bw() +
  theme(plot.margin=unit(c(2.5, 5, 8, 5.5), "pt")) +
  theme(axis.text=element_text(colour="black",size=axis_size),
        axis.title=element_text(colour="black",size=axis_size))
p
ggsave(filename = "~/Desktop/random_forest_actual_pred_d3.pdf", plot = p,
       width = pdf_width, height = pdf_height, units= "in")
