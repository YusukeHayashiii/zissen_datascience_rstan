# 分析の準備 ----

# パッケージの読み込み
library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 状態空間モデルの図示をする関数の読み込み
source("plotSSM.R", encoding="utf-8")


# データの読み込み
fish_ts <- read.csv("book-data/5-9-1-fish-num-ts.csv")
fish_ts$date <- as.POSIXct(fish_ts$date)
head(fish_ts)

# 図示
autoplot(ts(fish_ts[, -1]))


# モデルの推定 ----

# データの準備
data_list <- list(
    T = nrow(fish_ts),
    y = fish_ts$fish_num,
    ex = fish_ts$temperature
)

# 推定
dglm_poisson <- stan(
    file = "5-9_dglm_poisson.stan",
    data = data_list,
    seed = 1,
    iter = 8000,
    warmup = 2000,
    thin = 6,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
)

# 結果
print(dglm_poisson, 
      par =  c("s_z", "s_r", "b", "lp__"),
      probs = c(0.025, 0.5, 0.975))

# 参考：収束の確認
mcmc_rhat(rhat(dglm_poisson))
check_hmc_diagnostics(dglm_poisson)

# 参考:トレースプロット
mcmc_sample <- rstan::extract(dglm_poisson, permuted = FALSE)
mcmc_trace(mcmc_sample, pars = c("s_z", "s_r", "lp__"))

# 参考：推定結果一覧
options(max.print=100000)
print(dglm_poisson, probs = c(0.025, 0.5, 0.975))


# 推定結果の図示 ----
# MCMCサンプルの取得
mcmc_sample <- rstan::extract(dglm_poisson)

# グラフの作成
p_all <- plotSSM(mcmc_sample = mcmc_sample,
                 time_vec = fish_ts$date,
                 obs_vec = fish_ts$fish_num,
                 state_name = "lambda_exp",
                 graph_title = "状態推定値",
                 y_label = "釣獲尾数",
                 date_labels = "%Y年%m月%d日")

p_smooth <- plotSSM(mcmc_sample = mcmc_sample,
                 time_vec = fish_ts$date,
                 obs_vec = fish_ts$fish_num,
                 state_name = "lambda_smooth",
                 graph_title = "ランダム効果を除いた状態推定値",
                 y_label = "釣獲尾数",
                 date_labels = "%Y年%m月%d日")

p_fix <- plotSSM(mcmc_sample = mcmc_sample,
                 time_vec = fish_ts$date,
                 obs_vec = fish_ts$fish_num,
                 state_name = "lambda_smooth_fix",
                 graph_title = "気温を固定した状態推定値",
                 y_label = "釣獲尾数",
                 date_labels = "%Y年%m月%d日")

# 図示
grid.arrange(p_all, p_smooth, p_fix)
