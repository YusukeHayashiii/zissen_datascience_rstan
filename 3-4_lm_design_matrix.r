# 分析の準備 ----

# パッケージの読み込み
library(rstan)
library(bayesplot)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 分析対象のデータ
file_beer_sales_2 <- read.csv("book-data/3-2-1-beer-sales-2.csv")

# サンプルサイズ
sample_size <- nrow(file_beer_sales_2)

# formula構文を用いてデザイン行列を作成

# formula
formula_lm <- formula(sales ~ temperature)

# デザイン行列の作成
X <- model.matrix(formula_lm, file_beer_sales_2)
head(X, 5)
class(X)


# MCMCの実行----

# サンプルサイズ
N <- nrow(file_beer_sales_2)

# デザイン行列の列数
K <- 2

# 応答変数
Y <- file_beer_sales_2$sales

# list化
data_list_design <- list(N = N, K = K, Y = Y, X = X)

# MCMCの実行
mcmc_result_design <- stan(
    file = "3-4_lm_design_matrix.stan", 
    data = data_list_design, 
    seed = 1
)

# 結果
print(mcmc_result_design, probs = c(0.025, 0.5, 0.975))
