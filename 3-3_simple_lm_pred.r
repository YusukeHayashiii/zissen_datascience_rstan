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


# 予測のためのデータの整理 ----

# 気温を11度から30度まで変化させて、その時の売り上げを予測する
temperature_pred <-11:30
temperature_pred

# listにする
data_list_pred <- list(
    N = sample_size, 
    sales = file_beer_sales_2$sales,
    temperature = file_beer_sales_2$temperature, 
    N_pred = length(temperature_pred), 
    temperature_pred = temperature_pred
    )

# MCMCの実行----

# 実行
mcmc_result_pred <- stan(
    file = "3-3_simple_lm_pred.stan", 
    data = data_list_pred,
    seed = 1
)

# 結果
print(mcmc_result_pred, probs = c(0.025, 0.50, 0.975))


# 予測分布の可視化----

# MCMCサンプルの抽出
mcmc_sample_pred <- rstan::extract(mcmc_result_pred, 
                                   permuted = FALSE)

# 気温を変えたときの予測売り上げの95%信用区間の図示
mcmc_intervals(
    mcmc_sample_pred, 
    regex_pars = c('sales_pred.'), 
    prob = 0.8, 
    prob_outer = 0.95
)

# mu_predとの比較
mcmc_intervals(
    mcmc_sample_pred, 
    pars = c('mu_pred[1]', 'sales_pred[1]'), 
    prob = 0.8, 
    prob_outer = 0.95
)

# 気温が11度と30度の時の、売り上げの予測分布
mcmc_areas(
    mcmc_sample_pred, 
    pars = c('sales_pred[1]', 'sales_pred[20]'), 
    prob = 0.6, 
    prob_outer = 0.99
)
