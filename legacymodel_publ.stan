data{
  int N; // sample size: number of partitions here 
  
  int<lower=0> t2_AOU[N]; // resp variable. count data
  // vector<lower=0>[N] t2_AOU; // trying another set up for the resp data --> does not work together with poisson_log()!
  
  // Explanatory variables
  
  // Static-equilibrium vars
  vector[N] urban_t2;  
  vector[N] urban_t1; 
  vector[N] forest_t2;  
  vector[N] forest_t1;
  vector[N] grass_t2;  
  vector[N] grass_t1;
  vector[N] crop_t2;  
  vector[N] crop_t1;
  vector[N] wet_t2;  
  vector[N] wet_t1;
  
  // quadratic terms vars
  vector[N] urban_squared_t2;
  vector[N] forest_squared_t2;
  vector[N] crop_squared_t2;
  vector[N] grass_squared_t2;
  vector[N] wet_squared_t2;
  
  vector[N] urban_squared_t1;
  vector[N] forest_squared_t1;
  vector[N] crop_squared_t1;
  vector[N] grass_squared_t1;
  vector[N] wet_squared_t1;
  // 
  //   // additional vars
  vector[N] time; // survey starting time
  vector[N] temp_t2; // temp
  vector[N] temp_squared_t2; // quadratic temp
  // // //   
  int<lower=1> O; // number of observer levels for random effect
  int<lower=1, upper=O> observerID[N];
  // // 
  // int<lower=1> R; // number of route levels for random effect
  // int<lower=1, upper=R> routeID[N];

}
parameters{
  
  real intercept;
  
  real<lower=0, upper=1> w;
  
  // linear land cover parameters
  real<lower=0> b_urban;
  real<lower=0> b_forest;
  real<lower=0> b_grass;
  real<lower=0> b_crop;
  real<lower=0> b_wet;

  // quadratic land cover parameters
  real<upper=0> b2_urban;
  real<upper=0> b2_forest;
  real<upper=0> b2_grass;
  real<upper=0> b2_crop;
  real<upper=0> b2_wet;
  // 
  //   // additional params
  real b_time;
  real<lower=0> b_temp;
  real<upper=0> b2_temp;
  // 
  // // // random effects variance
  real<lower=0> sigma_observer;
  // // 
  vector[O] random_observer; // random effect vectors

}

transformed parameters {
  
  vector[N] lambda;
  
    
    lambda = intercept +
    

    // equilibrium model t2
    // linear and quadratic terms
    (b_urban * urban_t2 + b2_urban * urban_squared_t2 +
    b_forest * forest_t2 + b2_forest * forest_squared_t2 +
    b_grass * grass_t2 + b2_grass * grass_squared_t2 +
    b_crop * crop_t2 + b2_crop * crop_squared_t2 +
    b_wet * wet_t2  + b2_wet * wet_squared_t2) *
    // delay model t2

    w +

    // equilibrium model t1
    // linear and quadratic terms
    (b_urban * urban_t1 + b2_urban * urban_squared_t1 +
    b_forest * forest_t1 + b2_forest * forest_squared_t1 +
    b_grass * grass_t1 + b2_grass * grass_squared_t1 +
    b_crop * crop_t1 + b2_crop * crop_squared_t1 +
    b_wet * wet_t1 + b2_wet * wet_squared_t1) *
    // delay model t1
    (1 - w) +
    
    
  // additional terms
                  b_time * time +
                  b_temp * temp_t2 + 
                  b2_temp * temp_squared_t2 +
                  random_observer[observerID];
 
}

model{
  // main model

  
  // delay parameter
  // w ~ uniform(0,1);
  // w ~ beta(1,1); // 1,1 makes the a straight line, basically identicle to uniform but without boundaries

  // linear params
  b_urban ~ normal(0,1);
  b_forest ~ normal(0,1);
  b_grass ~ normal(0,1);
  b_crop ~ normal(0,1);
  b_wet ~ normal(0,1);

  // quadratic params
  b2_urban ~ normal(0,10);
  b2_forest ~ normal(0,10);
  b2_grass ~ normal(0,10);
  b2_crop ~ normal(0,10);
  b2_wet ~ normal(0,10);

  //   // additional parameters
  b_time ~ normal(0,10);
  b_temp ~ normal(0,1);
  b2_temp ~ normal(0,1);

  sigma_observer ~ gamma(0.001, 0.001); // sd of random effect

  random_observer ~ normal(0, sigma_observer);

  
  t2_AOU ~ poisson_log(lambda);
  
}
