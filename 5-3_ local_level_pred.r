# 分析の準備----

library(rstan)
library(bayesplot)

# 計算の高速化
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 状態空間モデルを図示する関数の読み込み
source("plotSSM.r", encoding = "utf-8")

# データ読み込み
sales_df_all <- read.csv("book-data/5-2-1-sales-ts-1.csv")
sales_df_all$date <- as.POSIXct(sales_df_all$date)


# ローカルレベルモデルによる予測の実行 ----

# データの準備
data_list_pred <- list(
    T = nrow(sales_df_all),
    y = sales_df_all$sales,
    pred_term = 20
    )

# モデルの推定
local_level_pred <- stan(
    file = "5-3-1_local_level_pred.stan",
    data = data_list_pred,
    seed = 1
)

# 参考：収束の確認
mcmc_rhat(rhat(local_level_pred))

# 参考：結果の表示
print(local_level_pred,
      pars = c("s_w", "s_v","lp__"),
      probs = c(0.025, 0.5, 0.975))

## 図示

# 予測対象期間も含めた日付を用意
data_plot <- seq(
    from = as.POSIXct("2020-01-01"),
    by = "days",
    len =120
)
# 生成された乱数を格納
mcmc_sample_pred <- rstan::extract(local_level_pred)

# 予測結果の図示
plotSSM(mcmc_sample = mcmc_sample_pred,
        time_vec = data_plot,
        state_name = "mu_pred",
        graph_title = "予測の結果",
        y_label = "sales")


# 欠損があるデータ ----

# データの読み込み
sales_df_NA <- read.csv("book-data/5-3-1-sales-ts-1-NA.csv")

# 日付をPOSIXct形式にする
sales_df_NA$date <- as.POSIXct(sales_df_NA$date)

# 売り上げデータに一部欠損がある
head(sales_df_NA, n = 3)


# 欠損データの取り扱い ----

# 欠損データの削除
sales_df_omit_NA <- na.omit(sales_df_NA)
head(sales_df_omit_NA, n = 3)

nrow(sales_df_NA)
nrow(sales_df_omit_NA)

# NAがどこにあるかを判別
!is.na(sales_df_NA$sales)

# データがある行番号の取得
# which()はTRUEのある位置を返す
which(!is.na(sales_df_NA$sales))


# ローカルレベルモデルによる補間の実行 ----

# データの準備
data_list_interpolation <- list(
    T       = nrow(sales_df_NA),
    len_obs = nrow(sales_df_omit_NA),
    y       = sales_df_omit_NA$sales,
    obs_no  = which(!is.na(sales_df_NA$sales))
)

# モデルの推定
local_level_interpolation <- stan(
    file = "5-3-2_local_level_interpolation.stan",
    data = data_list_interpolation,
    seed = 1,
    iter = 4000
)

# 参考：収束の確認
mcmc_rhat(rhat(local_level_interpolation))

# 参考：結果の表示
print(local_level_interpolation,
      pars = c("s_w", "s_v","lp__"),
      probs = c(0.025, 0.5, 0.975))

## 図示

# 生成された乱数を格納
mcmc_sample_interpolation <- rstan::extract(
    local_level_interpolation
)

# 図示
plotSSM(mcmc_sample = mcmc_sample_interpolation, 
        time_vec = sales_df_all$date, 
        obs_vec = sales_df_all$sales,
        state_name = "mu", 
        graph_title = "補間の結果",
        y_label = "sales") 


# 参考：予測区間 -----------------------------------------------------------------


# モデルの推定
local_level_prediction_interval <- stan(
  file = "book-data/5-3-3-local-level-interpolation-prediction-interval.stan",
  data = data_list_interpolation,
  seed = 1,
  iter = 4000
)

# 参考：収束の確認
mcmc_rhat(rhat(local_level_prediction_interval))

# 参考：結果の表示
print(local_level_prediction_interval,
      pars = c("s_w", "s_v","lp__"),
      probs = c(0.025, 0.5, 0.975))


## 図示

# 生成された乱数を格納
mcmc_sample_prediction_interval <- rstan::extract(
  local_level_prediction_interval)

# 図示
plotSSM(mcmc_sample = mcmc_sample_prediction_interval, 
        time_vec = sales_df_all$date, 
        obs_vec = sales_df_all$sales,
        state_name = "y_pred", 
        graph_title = "補間の結果:予測分布",
        y_label = "sales") 
