# パッケージの読み込み----
library(rstan)
library(bayesplot)

library(ggfortify)


# MCMCの実行----

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 分析対象のデータ
file_beer_sales_1 <- read.csv("book-data/2-4-1-beer-sales-1.csv")

# サンプルサイズ
sample_size <- nrow(file_beer_sales_1)

# listにまとめる
data_list <- list(sales = file_beer_sales_1$sales, N = sample_size)

# 乱数の生成
mcmc_result <- stan(
  file = "2-4-1_calc_mean_variance.stan", # stanファイル
  data = data_list,                       # 対象データ
  seed = 1,                               # 乱数の種
  chains = 4,                             # チェーン数
  iter = 2000,                            # 乱数生成の繰り返し数
  warmup = 1000,                          # バーンイン期間
  thin = 1                                # 間引き数(1なら間引き無し) 
)


# MCMCサンプルの抽出----
mcmc_sample <- rstan::extract(mcmc_result, permuted = FALSE)
# 中身の確認
class(mcmc_sample)
dim(mcmc_sample)
dimnames(mcmc_sample)

mcmc_sample[1,'chain:1', 'mu']
mcmc_sample[,'chain:1', 'mu']
length(mcmc_sample[,'chain:1', 'mu'])

length(mcmc_sample[, , 'mu'])
dim(mcmc_sample[, , 'mu'])
class(mcmc_sample[, , 'mu'])


# MCMCサンプルの代表値の計算 ----
mu_mcmc_vec <- as.vector(mcmc_sample[, , 'mu'])

# 事後中央値
median(mu_mcmc_vec)
quantile(mu_mcmc_vec, 0.5)

# 事後期待値
mean(mu_mcmc_vec)

# 95%ベイズ信用区間
quantile(mu_mcmc_vec, probs = c(0.025, 0.975))

# 参考：print関数
print(
  mcmc_result,                   # MCMCサンプリングの結果
  probs = c(0.025, 0.5, 0.975)   # 中央値と95%信用区間を出力
)


# トレースプロットの描画 ----
# 参考：標準のトレースプロット
traceplot(mcmc_result, par = "mu")

# MCMCサンプルを使ったトレースプロット
library(ggfortify)
autoplot(ts(mcmc_sample[, , 'mu']),
    facets = F, # 4つのchainをまとめて一つに
    ylab = 'mu', # y軸ラベル
    main = 'トレースプロット')


# 事後分布の図示----
mu_df <- data.frame(
    mu_mcmc_sample = mu_mcmc_vec
)

ggplot(mu_df, aes(x = mu_mcmc_sample)) + 
    geom_density(size = 1.5)


# bayesplotを用いた事後分布の図示 ----
library(bayesplot)

# ヒストグラム
mcmc_hist(mcmc_sample, pars = c('mu', 'sigma'))

# カーネル密度推定
mcmc_dens(mcmc_sample, pars = c('mu', 'sigma'))

# トレースプロット
mcmc_trace(mcmc_sample, pars = c('mu', 'sigma'))

# 事後分布+トレースプロット
mcmc_combo(mcmc_sample, pars = c('mu', 'sigma'))


#	bayesplotで事後分布の範囲を比較する ----
mcmc_intervals(
    mcmc_sample, pars = c('mu','sigma'),
    prob = 0.8,         # 太い線の範囲
    prob_outer = 0.95   # 細い線の範囲
)

# 密度の情報も加える
mcmc_areas(mcmc_sample, pars = c("mu", "sigma"), 
           prob = 0.6,        # 薄い青色で塗られた範囲
           prob_outer = 0.99  # 細い線が描画される範囲
)

# bayesplotによるMCMCサンプルの評価 ----

# MCMCサンプルのコレログラム
mcmc_acf_bar(mcmc_sample, pars = c('mu', 'sigma'))


# 事後予測チェック：MCMCの実行 ----
animal_num <- read.csv('book-data/2-5-1-animal-num.csv')
head(animal_num, 3)

# サンプルサイズ
sample_size <- nrow(animal_num)

# listにまとめる
data_list <- list(animal_num = animal_num$animal_num, N = sample_size)

# MCMCの実行：正規分布の場合
mcmc_normal <- stan(
    file = "2-5-1_normal_dist.stan",
    data = data_list,
    seed = 1
)

# MCMCの実行：ポアソン分布の場合
mcmc_poisson <- stan(
    file = "2-5-1_poisson_dist.stan",
    data = data_list,
    seed = 1
)

# 参考：推定されたパラメタ
print(mcmc_normal, par = c("mu", "sigma", "lp__"))
print(mcmc_poisson, par = c("lambda", "lp__"))


# 事後予測チェックの実施 ----
y_rep_normal <- rstan::extract(mcmc_normal)$pred
y_rep_poisson <- rstan::extract(mcmc_poisson)$pred

# MCMCサンプルのサンプルサイズ
dim(y_rep_normal)

# 事後予測値の1回目のMCMCサンプルを抽出
# 正規分布を仮定したモデル
y_rep_normal[1,]
y_rep_poisson[1,]

# 参考；観測データの分布と、事後予測分布の比較
hist(animal_num$animal_num) # 観測データの分布
hist(y_rep_normal[1,])      # 正規分布を仮定した事後予測分布
hist(y_rep_poisson[1,])     # ポアソン分布を仮定した事後予測分布

# 元データのヒストグラムと、
# 1~5回分のMCMCサンプルの事後予測値のヒストグラム

# 正規分布を仮定したモデル
ppc_hist(y = animal_num$animal_num,  
         yrep = y_rep_normal[1:5, ])

# ポアソン分布を仮定したモデル
ppc_hist(y = animal_num$animal_num,  
         yrep = y_rep_poisson[1:5, ])

# カーネル密度推定

# 正規分布を仮定したモデル
ppc_dens(y = animal_num$animal_num,  
         yrep = y_rep_normal[1:10, ])

# ポアソン分布を仮定したモデル
ppc_dens(y = animal_num$animal_num,  
         yrep = y_rep_poisson[1:10, ])
