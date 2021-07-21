data {
   int T;
   vector[T] y;
}

parameters {
    real<lower=0> s_w;
    real b_ar;
    real Intercept;
}

model {
    for(i in 2:T) {
        y[i] ~ normal(Intercept + b_ar * y[i-1], s_w);
    }
}
