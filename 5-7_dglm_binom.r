# 分析の準備 ----
install.packages(("KFAS"))

# パッケージの読み込み
library(rstan)
library(bayesplot)
library(KFAS)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 状態空間モデルの図示をする関数の読み込み
source("plotSSM.R", encoding="utf-8")

# データの読み込み
data("boat")
boat


# 二項分布を仮定したDGLMの推定 ----

# NAを除く
boat_omit_NA <- na.omit(as.numeric(boat))
boat_omit_NA

# データの準備
data_list <- list(
    T       = length(boat),
    len_obs = length(boat_omit_NA),
    y       = boat_omit_NA,
    obs_no  = which(!is.na(boat))
)

# 推定
dglm_binom <- stan(
    file = "5-8_dglm_binom.stan",
    data = data_list,
    seed = 1,
    iter = 30000,
    warmup = 10000,
    thin = 20
)

# 結果
print(dglm_binom,
      par = c("s_w", "lp__"),
      probs = c(0.025, 0.5, 0.975))

# 参考：収束の確認
mcmc_rhat(rhat(dglm_binom))
check_hmc_diagnostics(dglm_binom)

# 参考:トレースプロット
mcmc_sample <- rstan::extract(dglm_binom, permuted = FALSE)
mcmc_trace(mcmc_sample, pars = c("s_w", "lp__"))

# 参考：推定結果一覧
options(max.print=100000)
print(dglm_binom, probs = c(0.025, 0.5, 0.975))


# 推定された状態の図示 ----

years <- seq(from = as.POSIXct("1829-01-01"),
             by = "1 year",
             len = length(boat))
head(years)

# mcmcサンプルの取得
mcmc_sample <- rstan::extract(dglm_binom)

# ケンブリッジ大学の勝率の推移のグラフ
plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = years,
        obs_vec = as.numeric(boat),
        state_name = "probs", 
        graph_title = "ケンブリッジ大学の勝率の推移", 
        y_label = "勝率",
        date_labels = "%Y年") 


# ケンブリッジ大学の平均勝率
mean(boat_omit_NA)
