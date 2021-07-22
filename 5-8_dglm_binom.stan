data {
    int T;
    int len_obs;
    int y[len_obs];
    int obs_no[len_obs];
}

parameters {
    vector[T] mu;
    real<lower=0> s_w;
}

model {
    // 弱情報事前分布
    s_w ~ student_t(3, 0, 10);
    
    // 状態方程式
    for(i in 2:T) {
        mu[i] ~ normal(mu[i-1], s_w);
    }

    // 観測方程式
    for(i in 1:len_obs) {
        y[i] ~ bernoulli_logit(mu[obs_no[i]]);
    }
}

generated quantities {
   vector[T] probs;
   probs = inv_logit(mu);
}
