data {
    int N;           // サンプルサイズ
    int K;           //デザイン行列の列数
    vector[N] Y;
    matrix[N, K] X;
}

parameters {
    vector[K] b;
    real<lower=0> sigma;
}

model {
   vector[N] mu = X * b;
   Y ~ normal(mu, sigma);
}
