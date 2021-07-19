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
