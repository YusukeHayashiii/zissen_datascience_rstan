# 分析の準備----
# パッケージの読み込み
library(rstan)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# データの読み込みと図示----

sales_weather <- read.csv("book-data/3-6-1-beer-sales-3.csv")
head(sales_weather, 3)

summary(sales_weather)

ggplot(sales_weather, aes(x = weather, y = sales)) + 
    geom_violin() + 
    geom_point(aes(color = weather)) + 
    labs(title = "ビールの売上と天気の関係")

# brmsによるモデルの推定----

# 分散分析モデル
anova_brms <- brm(
    formula = sales ~ weather,
    family = gaussian(),
    data = sales_weather,
    seed = 1,
    prior = c(set_prior("", class = "Intercept"),
              set_prior("", class = "sigma"))
)

anova_brms

# 可視化
eff <- marginal_effects(anova_brms)
plot(eff, points = FALSE)


# 補足：分散分析モデルのデザイン行列 ----

# デザイン行列作成
formula_anova <- formula(sales ~ weather)
design_mat <- model.matrix(formula_anova, sales_weather)

# stanに渡すlistの作成
data_list <- list(
    N = nrow(sales_weather),
    K = 3,
    Y = sales_weather$sales, 
    X = design_mat
)
data_list


# 補足：brmsを使わない分散分析モデルの推定 ----

# rstanで分散分析モデルを実行
anova_stam <- stan(
    file = "3-4_lm_design_matrix.stan", 
    data = data_list, 
    seed = 1
)

# 結果
print(anova_stam, probs = c(0.025, 0.5, 0.975))
