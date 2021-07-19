data {
    int N;                  // サンプルサイズ
    vector[N] sales;        // 売上
    vector[N] temperature;  // 気温
}

parameters {
    real Intercept;         // 切片
    real beta;              // 傾き
    real<lower=0> sigma;    // 標準偏差
}

model {
    // 平均が線形変化する正規分布
    sales ~ normal(Intercept + beta*temperature, sigma);
}
