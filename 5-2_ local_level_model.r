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
