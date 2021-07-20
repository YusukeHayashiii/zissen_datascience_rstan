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

