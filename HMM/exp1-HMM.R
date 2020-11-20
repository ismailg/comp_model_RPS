library(dplyr)
library(depmixS4)
source("HMM/Qlearn-HMM.R")
source("HMM/ToM-HMM.R")
source("HMM/dummy-HMM.R")

dat <- read.csv("data20180719.csv")

dat <- as_tibble(dat) %>% group_by(human_id,game)
dat <- dat %>%
  mutate(ai_action_prev = lag(ai_action,1), human_action_prev = lag(human_action,1)) %>%
    mutate(state = interaction(ai_action_prev,human_action_prev))
dat <- dat %>% mutate(pred_a1_level0 = case_when(
  game == "rps" & ai_action_prev == "rock" ~ 1,
  game == "fwg" & ai_action_prev == "fire" ~ 1,
  game == "numbers" & ai_action_prev == "one" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a2_level0 = case_when(
  game == "rps" & ai_action_prev == "paper" ~ 1,
  game == "fwg" & ai_action_prev == "water" ~ 1,
  game == "numbers" & ai_action_prev == "two" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a3_level0 = case_when(
  game == "rps" & ai_action_prev == "scissors" ~ 1,
  game == "fwg" & ai_action_prev == "grass" ~ 1,
  game == "numbers" & ai_action_prev == "three" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a4_level0 = case_when(
  game == "numbers" & ai_action_prev == "four" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  TRUE ~ 0))
dat <- dat %>% mutate(pred_a5_level0 = case_when(
  game == "numbers" & ai_action_prev == "five" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  TRUE ~ 0))

## level 1 predictions
dat <- dat %>% mutate(pred_a1_level1 = case_when(
  game == "rps" & human_action_prev == "scissors" ~ 1,
  game == "fwg" & human_action_prev == "grass" ~ 1,
  game == "numbers" & human_action_prev == "five" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a2_level1 = case_when(
  game == "rps" & human_action_prev == "rock" ~ 1,
  game == "fwg" & human_action_prev == "fire" ~ 1,
  game == "numbers" & human_action_prev == "one" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a3_level1 = case_when(
  game == "rps" & human_action_prev == "paper" ~ 1,
  game == "fwg" & human_action_prev == "water" ~ 1,
  game == "numbers" & ai_action_prev == "two" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a4_level1 = case_when(
  game == "numbers" & ai_action_prev == "three" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  TRUE ~ 0))
dat <- dat %>% mutate(pred_a5_level1 = case_when(
  game == "numbers" & ai_action_prev == "four" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  TRUE ~ 0))

## level 2 predictions
dat <- dat %>% mutate(pred_a1_level2 = case_when(
  game == "rps" & ai_action_prev == "paper" ~ 1,
  game == "fwg" & ai_action_prev == "water" ~ 1,
  game == "numbers" & ai_action_prev == "four" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a2_level2 = case_when(
  game == "rps" & ai_action_prev == "scissors" ~ 1,
  game == "fwg" & ai_action_prev == "grass" ~ 1,
  game == "numbers" & ai_action_prev == "five" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a3_level2 = case_when(
  game == "rps" & ai_action_prev == "rock" ~ 1,
  game == "fwg" & ai_action_prev == "fire" ~ 1,
  game == "numbers" & ai_action_prev == "one" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a4_level2 = case_when(
  game == "numbers" & ai_action_prev == "two" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  TRUE ~ 0))
dat <- dat %>% mutate(pred_a5_level2 = case_when(
  game == "numbers" & ai_action_prev == "three" ~ 1,
  game == "numbers" & is.na(ai_action_prev) ~ 1/5,
  TRUE ~ 0))

# vars for Qlearn
reward <- as.numeric(dat$winner == "human")
reward[dat$winner == "ai"] <- -1
state <- as.numeric(dat$state)
act <- as.numeric(factor(dat$human_action,levels=c("rock","paper","scissors","fire","water","grass","one","two","three","four","five")))
act_mask <- matrix(0,nrow=nrow(dat),ncol=3+3+5)
act_mask[dat$game == "rps",1:3] <- 1
act_mask[dat$game == "fwg",4:6] <- 1
act_mask[dat$game == "numbers",7:11] <- 1

# vars to ToM
dat$ai_action_num <- recode(dat$ai_action,"rock" = 1, "paper" = 2, "scissors" = 3, "fire" = 1, "water" = 2, "grass" = 3, "one" = 1, "two" = 2, "three" = 3, "four" = 4 , "five" = 5)
dat$human_action_num <- recode(dat$human_action,"rock" = 1, "paper" = 2, "scissors" = 3, "fire" = 1, "water" = 2, "grass" = 3, "one" = 1, "two" = 2, "three" = 3, "four" = 4 , "five" = 5)
act_num <- dat$human_action_num
opp_act_num <- dat$ai_action_num
pred <- array(0.0,dim=c(nrow(dat),5,3))
pred[,1,1] <- dat$pred_a1_level0
pred[,2,1] <- dat$pred_a2_level0
pred[,3,1] <- dat$pred_a3_level0
pred[,4,1] <- dat$pred_a4_level0
pred[,5,1] <- dat$pred_a5_level0
pred[,1,2] <- dat$pred_a1_level1
pred[,2,2] <- dat$pred_a2_level1
pred[,3,2] <- dat$pred_a3_level1
pred[,4,2] <- dat$pred_a4_level1
pred[,5,2] <- dat$pred_a5_level1
pred[,1,3] <- dat$pred_a1_level2
pred[,2,3] <- dat$pred_a2_level2
pred[,3,3] <- dat$pred_a3_level2
pred[,4,3] <- dat$pred_a4_level2
pred[,5,3] <- dat$pred_a5_level2
n_act <- 3*(dat$game != "numbers") + 5*(dat$game == "numbers")


nsubject <- length(unique(dat$human_id)) # number of participants
ngame <- 3 # number of games
ntrial <- c(50,50,50) # numer of trials in each game


rModels <- list(
  list(
    dummyResponse(1/n_act)
  ),
  list(
    ToM(act = act_num, opp_act = opp_act_num, pred = pred, n_act = n_act, ntimes=rep(sum(ntrial),nsubject))
  ),
  list(
    Qlearn(reward = reward, state = state, act = act, act_mask = act_mask, ntimes=rep(sum(ntrial),nsubject))
  )
)

trstart <- matrix(c(0.8,0.1,0.1,0.1,0.8,0.1,.1,.1,.8),ncol=3)
transition <- list()
transition[[1]] <- transInit(~1,nstates=3,data=data.frame(1),pstart=trstart[1,],family=multinomial("identity"))
transition[[2]] <- transInit(~1,nstates=3,data=data.frame(1),pstart=trstart[2,],family=multinomial("identity"))
transition[[3]] <- transInit(~1,nstates=3,data=data.frame(1),pstart=trstart[3,],family=multinomial("identity"))

instart <- c(1/3,1/3,1/3)
inMod <- transInit(~1,nstates=3,pstart=instart,family=multinomial("identity"),data=data.frame(rep(1,nsubject*ngame)))

mod <- makeDepmix(response=rModels,transition=transition,prior=inMod,ntimes=rep(ntrial,nsubject))

fmod <- fit(mod, emcontrol=em.control(random.start=FALSE))
