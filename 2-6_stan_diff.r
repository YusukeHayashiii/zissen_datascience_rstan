# 平均値の差の評価とgenerated quantitiesブロック ----
# パッケージの読み込み
library(rstan)
library(bayesplot)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データ読み込み
file_beer_sales_ab <- read.csv('book-data/2-6-1-beer-sales-ab.csv')
head(file_beer_sales_ab, 3)

# ビールの種類別のヒストグラム
ggplot(file_beer_sales_ab, aes(x = sales, y = ..density..,
    color = beer_name, fill = beer_name)) + 
    geom_histogram(alpha = 0.5, position = 'identity') + 
    geom_density(alpha = 0.5, size = 0)

# ビールの種類別にデータを分ける
sales_a <- file_beer_sales_ab$sales[1:100]
sales_b <- file_beer_sales_ab$sales[101:200]

# listにまとめる
data_list_ab <- list(
  sales_a = sales_a,
  sales_b = sales_b,
  N = 100
)

# 乱数の生成
mcmc_result_diff_beer <- stan(
    file = "book-data/2-6-5-difference-mean.stan",
    data = data_list_ab,
    seed = 1
)

print(
    mcmc_result_diff_beer,
    probs = c(0.025, 0.975)
)

# 平均値の差の事後分布のグラフ
mcmc_sample <- rstan::extract(mcmc_result_diff_beer, permuted = FALSE)
mcmc_dens(mcmc_sample, pars = 'diff')
