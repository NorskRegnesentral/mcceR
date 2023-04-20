

library(MASS)
library(data.table)
library(mcceR)

### Full setup ###

filename <- "inst/paper_experiment/table_sims_3.csv"
set.seed(123)

p_all <- 100
n_all <- 1.1*10^6
mu_all <- rep(0,p_all)
rho_all <- 0.5
Sigma_all <- matrix(rho_all,nrow = p_all,ncol=p_all)
diag(Sigma_all) <- 1

x_all <- MASS::mvrnorm(n_all,mu = mu_all,Sigma = Sigma_all)
#beta_all <- rep(1,p_all)
#epsilon_all <- rnorm(n_all,sd = 0.1)

y_all <- rep(1,n_all)#as.vector(x_all%*%beta_all+epsilon_all)

#### Experiment setup ####

c_int <- c(0,Inf)

n_explain_vec <- c(1,50)
n_train_vec <- c(10^3,10^4)
p_vec <- c(5,30)
generate.K_vec <- c(10^4,10^5)
reps <- 10

n_sims <- length(generate.K_vec)*length(p_vec)*length(n_train_vec)*length(n_explain_vec)*reps
counter <- 1
method == "cart_new_our_param"

for(i0 in seq_len(reps)){
  set.seed(123+i0)
  for(i1 in seq_along(n_explain_vec)){
    these_explain_init <- sample.int(n_all,size = n_explain_vec[i1]*100)
    x_explain0_init <- x_all[these_explain_init,]

    for(i2 in seq_along(n_train_vec)){
      these_train <- sample.int(n_all,size = n_train_vec[i2])
      x_train0 <- x_all[these_train,]
      y_train <- y_all[these_train]

      for(i3 in seq_along(p_vec)){
        these_dims <- sample.int(p_all,size=p_vec[i3])
        x_train <- as.data.frame(x_train0[,these_dims])
        x_explain_init <- as.data.frame(x_explain0_init[,these_dims])

        xy_train <- cbind(y=y_train,x_train)
        form <- as.formula(paste0("y~-1+",paste0("V",seq_len(p_vec[i3]),collapse = "+")))
        model <- lm(formula = form,data = xy_train)
        model$coefficients <- rep(1,length(model$coefficients)) # set all coefficients to 1

        pred_explain_init <- predict(model,x_explain_init)
        these_explain_nonorg <- head(which(pred_explain_init<c_int[1]),n_explain_vec[i1]) # Note that these inidicies are not the inidicies in the original data
        x_explain <- x_explain_init[these_explain_nonorg,]

        these_explain <- these_explain_init[these_explain_nonorg] # These are the inidicies in the original data (in case I want them)

        for(i4 in seq_along(generate.K_vec)){


          set.seed(123)
          if(method=="ctree_org"){
            explained <- explain_mcce(model = model,
                                      x_explain = x_explain,
                                      x_train = x_train,
                                      c_int = c_int,
                                      predict_model=NULL,
                                      fixed_features = NULL,
                                      generate.K = generate.K_vec[i4],store_model_list = T)
          }
          if(method=="ctree_synthpop_param"){
            explained <- explain_mcce(model = model,
                                      x_explain = x_explain,
                                      x_train = x_train,
                                      c_int = c_int,
                                      predict_model=NULL,
                                      fixed_features = NULL,
                                      generate.K = generate.K_vec[i4],
                                      control = party::ctree_control(minbucket = 5, mincriterion = 0.9),store_model_list = T)
          }
          if(method=="cart_org"){
            explained <- explain_mcce(model = model,
                                      x_explain = x_explain,
                                      x_train = x_train,
                                      c_int = c_int,
                                      predict_model=NULL,
                                      fixed_features = NULL,
                                      generate.K = generate.K_vec[i4],
                                      fit.autoregressive_model = "rpart",store_model_list = T)
          }
          if(method=="cart_synthpop_param"){
            explained <- explain_mcce(model = model,
                                      x_explain = x_explain,
                                      x_train = x_train,
                                      c_int = c_int,
                                      predict_model=NULL,
                                      fixed_features = NULL,
                                      generate.K = generate.K_vec[i4],
                                      fit.autoregressive_model = "rpart",
                                      control = rpart::rpart.control(minbucket = 5, cp = 1e-08),store_model_list = T)
          }

          if(method=="cart_our_param"){
            explained <- explain_mcce(model = model,
                                       x_explain = x_explain,
                                       x_train = x_train,
                                       c_int = c_int,
                                       predict_model=NULL,
                                       fixed_features = NULL,
                                       generate.K = generate.K_vec[i4],
                                       fit.autoregressive_model = "rpart",
                                       control = rpart::rpart.control(minbucket = 5, cp = 5e-04),store_model_list = T)
          }


          if(method=="cart_new_synthpop_param"){
            explained <- explain_mcce(model = model,
                                       x_explain = x_explain,
                                       x_train = x_train,
                                       c_int = c_int,
                                       predict_model=NULL,
                                       fixed_features = NULL,
                                       generate.K = generate.K_vec[i4],
                                       fit.autoregressive_model = "rpart_new",
                                       control = rpart::rpart.control(minbucket = 5, cp = 1e-08),store_model_list = T)
          }
          if(method=="cart_new_our_param"){
            explained <- explain_mcce(model = model,
                                       x_explain = x_explain,
                                       x_train = x_train,
                                       c_int = c_int,
                                       predict_model=NULL,
                                       fixed_features = NULL,
                                       generate.K = generate.K_vec[i4],
                                       fit.autoregressive_model = "rpart_new",
                                       control = rpart::rpart.control(minbucket = 5, cp = 5e-04),store_model_list = T)
          }




          print(paste0(counter/n_sims*100," % done"))

          # Storing timing results

          dt <- data.table(reps=i0,
                           n_explain=n_explain_vec[i1],
                           n_train=n_train_vec[i2],
                           p = p_vec[i3],
                           generate.K = generate.K_vec[i4],
                           fit.time = explained$time[1],
                           generate.time = explained$time[2],
                           process.time = explained$time[3])
          fwrite(dt,file=filename,append = T)

          counter <- counter + 1

          gc()

        }

      }

    }

  }
}
