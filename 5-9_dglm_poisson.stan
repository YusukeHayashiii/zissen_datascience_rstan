data {
    int T;
    vector[T] ex;
    int y[T];
}

parameters {
    real<lower=0> s_z;
    real<lower=0> s_r;
    vector[T] r;
    vector[T] mu;
    real b;
}

transformed parameters {
   vector[T] lambda;

   for(i in 1:T) {
       lambda[i] = mu[i] + b * ex[i] + r[i];
   }
}

model {
    r ~ normal(0, s_r);

    for(i in 3:T) {
        mu[i] ~normal(2*mu[i-1] - mu[i-2], s_z);
    }

    for(i in 1:T) {
        y[i] ~ poisson_log(lambda[i]);
    }
}

generated quantities {
   vector[T] lambda_exp;
   vector[T] lambda_smooth;
   vector[T] lambda_smooth_fix;

   lambda_exp = exp(lambda);
   lambda_smooth = exp(mu + b * ex);
   lambda_smooth_fix = exp(mu + b * mean(ex));
}
