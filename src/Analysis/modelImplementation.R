# Function to apply both adjustments
#
# @params:
#     - model:
#     - obs:
#     - n.quantile: . Default: 99
#     - extrapolation:
# @return:
#     Data.frame
#
# @author Jaimedgp

qq.predict <- function(model, x.obs, y.obs, date, n.quantile=99, extrapolation="qwerty") {

    if (is.na(x.obs)) {
        return(data.frame(date=date, obs=NaN, pred=NaN, pred.qq=NaN))
    }

    if ("lm" %in% class(model)) {
        train.outcome <- model$model[,1]

    } else if ("train" %in% class(model)) {
        train.outcome <- model$trainingData$.outcome

    } else if ("randomForest" %in% class(model)) {
        train.outcome <- model$y

    } else {
        print(paste("Please pass as model a class of type",
                    "'randomForest', 'lm' or a caret 'train' class"))
        return()
    }

    pred <- predict(model, newdata=x.obs)

    pred.qq <- downscaleR:::eqm(train.outcome,
                                predict(model),
                                pred,
                                n.quantile=n.quantile,
                                precip=FALSE, pr.threshold=0,
                                extrapolation=extrapolation
                               )

    data.frame(date=date, obs=y.obs, pred, pred.qq)
}


# Calculate metrics
#
# @params:
#     - obs:
#     - pred:
#     - obs.ds:
#     - pred.ds:
#     - obs.qq:
#     - pred.qq:
#     - obs.qq.ds:
#     - pred.qq.ds:
# @return:
#     Data.frame
#
# @author Jaimedgp

comp.metrics <- function(obs, obs.ds=NA,
                         pred, pred.ds=NA,
                         pred.qq=NA, pred.qq.ds=NA) {

    metrics <- data.frame("bias"=mean(pred) / mean(obs),
                          "var.ratio"=var(pred) / var(obs),
                          "cor1"=cor(pred, obs),
                          "cor2"=NaN,
                          "RMSE"=sqrt(mean((pred - obs)^2))
                          )
    if (!is.na(pred.ds)) {
        metrics$cor2 <- cor(pred.ds, obs.ds)
    }

    if (sum(!is.na(pred.qq)) != 0) {
        metrics <- rbind(cbind(data.frame(qq.Mapping="No"),
                               metrics),
                         data.frame(
                            qq.Mapping="Yes",
                            "bias"=mean(pred.qq) / mean(obs),
                            "var.ratio"=var(pred.qq) / var(obs),
                            "cor1"=cor(pred.qq, obs),
                            "cor2"=NaN,
                            "RMSE"=sqrt(mean((pred.qq - obs)^2))
                            )
                         )
        if (!is.na(pred.qq.ds)) {
            metrics[metrics$qq.Mapping == "Yes", "cor2"] <- cor(pred.qq.ds,
                                                                obs.ds)
        }
    }

    return(data.frame(metrics))
}


#
#
# @author Jaimedgp

cbind.cv <- function(cv.list, value.list) {

    for (nm in names(value.list)){
        vl <- value.list[[nm]]

        for (i in 1:length(cv.list)) {
            cv.list[[i]][, nm] <- vl
            cv.list[[i]] <- cv.list[[i]] %>% relocate(all_of(nm))
        }
    }

    cv.list
}


#
#
# @author Jaimedgp

rbind.cv <- function(...) {

    cv.list <- enexprs(...)

    end <- list(metrics=data.frame(),
                predictions=data.frame(),
                predictions.ds=data.frame())

    for (i in 1:length(cv.list)) {
        end$metrics <- rbind(end$metrics,
                             cv.list[[i]]$metrics)
        end$predictions <- rbind(end$predictions,
                                 cv.list[[i]]$predictions)
        end$predictions.ds <- rbind(end$predictions.ds,
                                    cv.list[[i]]$predictions.ds)
    }

    end
}


#
#
# @author Jaimedgp

leave.one.year.out <- function(date) {
    years <- as.factor(lubridate::year(date))

    if (length(levels(years)) < 2) {
        print("Not enough years")
        return()
    }

    yr.fold <- list()
    for (yr in levels(years)) {
        yr.fold[[yr]]$train <- which(lubridate::year(date) != as.numeric(yr))
        yr.fold[[yr]]$test <- which(lubridate::year(date) == as.numeric(yr))
    }
    yr.fold
}
