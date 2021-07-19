# 分析の準備----

# パッケージの読み込み
library(rstan)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データ読み込み----

# 分析対象のデータ
file_beer_sales_1 <- read.csv("book-data/2-4-1-beer-sales-1.csv")
head(file_beer_sales_1, 3)

# stanに渡すためにデータを整形----

# サンプルサイズ
sample_size <- nrow(file_beer_sales_1)
sample_size

# sales列をリストに
data_list <- list(sales = file_beer_sales_1$sales, N = sample_size)
data_list

# MCMCによるサンプリングの実施----

# 乱数の生成
mcmc_result <- stan(
    file = "2-4-1_calc_mean_variance.stan", # stanファイル
    data = data_list,                       # 対象データ
    seed = 1,                               # 乱数の種
    chains = 4,                             # 乱数生成のセット数
    iter = 2000,                            # 乱数生成の繰り返し数
    warmup = 1000,                          # バーンイン期間
    thin = 1                                # 間引き数(1なら間引きなし)
)

# 結果の表示
print(
  mcmc_result,                   # MCMCサンプリングの結果
  probs = c(0.025, 0.5, 0.975)   # 中央値と95%信用区間を出力
)

# 収束の確認----
 
 # トレースプロット(バーンイン期間なし)
traceplot(mcmc_result)
# トレースプロット(バーンイン期間あり)
traceplot(mcmc_result, inc_warmup = T)
