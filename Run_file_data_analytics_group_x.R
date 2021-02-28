
library(data.table)
library(ggplot2)
library(tuneRanger)
library(mlr)
library(OpenML)

pacman::p_load(rpart, rpart.plot, vip, pROC, randomForest, gbm)

DT <- fread("https://www.dropbox.com/s/5rr2ysw6tjcnbpj/train.csv?dl=1")
summary(DT)


DT$preferred_foot <- invisible(sapply(DT$preferred_foot,switch,'Right'=1,'Left'=2,'undefined'=""))
DT$work_rate <- invisible(sapply(DT$work_rate,switch,'Low/Medium'=1,'High/Medium'=2,'Medium/Medium'=3,'Medium/Low'=4,'Medium/High'=5,'High/High'=6,'High/Low'=7,'Low/Low'=8,'Low/High'=9,'undefined'=""))
DT$team_position <- invisible(sapply(DT$team_position,switch,'LCB'=1,'SUB'=2,'RES'=3,'LS'=4,'RB'=5,'ST'=6,'RM'=7,'LCM'=8,'LB'=9,'LM'=10,'RCB'=11,'RW'=12,'CAM'=13,'RDM'=14,'RWB'=15,'LF'=16,'CDM'=17,'LW'=18,'CB'=19,'LDM'=20,'CM'=21,'RCM'=22,'LAM'=23,'CF'=24,'RS'=25,'RF'=26,'RAM'=27,'LWB'=28,'undefined'=""))


test_submission <- fread("https://www.dropbox.com/s/395thqjfxf7k4wi/test.csv?dl=1")

test_submission$preferred_foot <- invisible(sapply(test_submission$preferred_foot,switch,'Right'=1,'Left'=2,'undefined'=""))
test_submission$work_rate <- invisible(sapply(test_submission$work_rate,switch,'Low/Medium'=1,'High/Medium'=2,'Medium/Medium'=3,'Medium/Low'=4,'Medium/High'=5,'High/High'=6,'High/Low'=7,'Low/Low'=8,'Low/High'=9,'undefined'=""))
test_submission$team_position <- invisible(sapply(test_submission$team_position,switch,'LCB'=1,'SUB'=2,'RES'=3,'LS'=4,'RB'=5,'ST'=6,'RM'=7,'LCM'=8,'LB'=9,'LM'=10,'RCB'=11,'RW'=12,'CAM'=13,'RDM'=14,'RWB'=15,'LF'=16,'CDM'=17,'LW'=18,'CB'=19,'LDM'=20,'CM'=21,'RCM'=22,'LAM'=23,'CF'=24,'RS'=25,'RF'=26,'RAM'=27,'LWB'=28,'undefined'=""))



regr.task = makeRegrTask(id = "player_ID", data = DT, target = "wage_eur")


#runtime should be around 21 mins (ran on a dell ultrabook with i7-8550U)
estimateTimeTuneRanger(regr.task)

#to ensure reproducibility
set.seed(123)

# Tuning and model generation
# note that this process cannot be uncoupled as the results with the best submitted estimate will change. The tuning process is intigrated in the production of the model
res = tuneRanger(regr.task, measure = list(mse), num.trees = 1000, iters = 70, iters.warmup = 30)

#show model
res

# Ranger Model with the new tuned hyperparameters
res$model




# our_prediction
our_prediction <- predict(res$model, newdata = test_submission)
View(our_prediction$data)

submit <- data.table(Id = test_submission$player_ID, Predicted = our_prediction$data)
fwrite(submit, "./2020_02_27_submit_tree_tuned_by_tuneRanger.csv")

submit