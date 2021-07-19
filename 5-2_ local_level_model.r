# 分析の準備 ----

# パッケージのインストール
# install.packages("ggfortify")
# install.packages("gridExtra")

# パッケージの読み込み
library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# ホワイトノイズとランダムウォーク ----

# 正規ホワイトノイズ
set.seed(1)
wn <- rnorm(n = 100, mean = 0, sd = 1)
# 累積和をとる関数cumsumの説明
cumsum(c(1,3,2))

# ランダムウォーク
rw <- cumsum(wn)

# 図示
p_wn_1 <- autoplot(ts(wn), main = "ホワイトノイズ")
p_rw_1 <- autoplot(ts(rw), main = "ランダムウォーク")
grid.arrange(p_wn_1, p_rw_1)

# 複数のホワイトノイズ・ランダムウォーク系列
# ランダムウォークの結果が上がったり下がったりするのを確認する
wn_mat <- matrix(nrow = 100, ncol = 20)
rw_mat <- matrix(nrow = 100, ncol = 20)

set.seed(1)
for(i in 1:20){
    wn <- rnorm(n = 100, mean = 0, sd = 1)
    wn_mat[, i] <- wn
    rw_mat[, i] <- cumsum(wn)
}

# 図示
p_wn_2 <- autoplot(ts(wn_mat), facets = F, main = "ホワイトノイズ") + 
    theme(legend.position = "none") # 凡例を消す
p_rw_2 <- autoplot(ts(rw_mat), facets = F, main = "ランダムウォーク") + 
    theme(legend.position = "none") # 凡例を消す
grid.arrange(p_wn_2, p_rw_2)


# データの読み込みとPOSIXctへの変換 ----

# データ読み込み
sales_df <- read.csv("book-data/5-2-1-sales-ts-1.csv")
# 日付をPOSIXct形式に
sales_df$date <- as.POSIXct(sales_df$date)
# データの先頭行を表示
head(sales_df, n = 3)
dim(sales_df)


# ローカルレベルモデルの推定 ----

# データの準備
data_list <- list(
    T = nrow(sales_df),
    y = sales_df$sales
)

# モデルの推定
local_level_stan <- stan(
    file = "5-2_local_level.stan",
    data = data_list,
    seed = 1
    )

# 収束の確認
mcmc_rhat(rhat(local_level_stan))
# 結果
print(local_level_stan,
      pars = c("s_w", "s_v", "lp__"),
      probs = c(0.025, 0.5, 0.975))


# 結果の図示 ----
# 乱数の格納
mcmc_sample <- rstan::extract(local_level_stan)
# stanにおける状態を表す変数名
state_name <- "mu"

# 1時点目の状態の95%ベイズ信用区間と中央値を得る
quantile(mcmc_sample[[state_name]][,1],
         probs = c(0.025, 0.5, 0.975))

# すべての時点の状態の、95%ベイズ信用区間と中央値
result_df <- data.frame(t(apply(   # tは転置
    X = mcmc_sample[[state_name]], # 実行対象となるデータ
    MARGIN = 2,                    # 列を対象としてループ
    FUN = quantile,                # 実行対象となる関数
    probs = c(0.025, 0.5, 0.9075)  # 上記関数に入れる引数
)))

# 列名の変更
colnames(result_df) <- c("lwr", "fit", "upr")

# 時間軸の追加
result_df$time <- sales_df$date

# 観測値の追加
result_df$obs <- sales_df$sales

# 図示のためのデータの確認
head(result_df, n = 3)

# 図示
ggplot(result_df, aes(x = time, y = obs)) +
    labs(title = "ローカルレベルモデルの推定結果") + 
    ylab("sales") +
    geom_point(alpha = 0.6, size =0.9) +
    geom_line(aes(y = fit), size =1.2) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
    scale_x_datetime(date_labels = "%Y年%m月")


# plotSSM関数を用いて図示----

# 関数の読み込み
source("plotSSM.r", encoding = "utf-8")

plotSSM(mcmc_sample = mcmc_sample, time_vec = sales_df$date, 
        obs_vec = sales_df$sales,
        state_name = "mu", graph_title = "ローカルレベルモデルの推定結果",
        y_label = "sales") 
