#Original data
sesame = read.csv("sesame.csv", header = T)

#Creating difference columns
sesame$diffbody = sesame$postbody - sesame$prebody
sesame$difflet = sesame$postlet - sesame$prelet
sesame$diffnumb = sesame$postnumb - sesame$prenumb

#Advantaged vs. disadvantaged
sesamedisadv = sesame[sesame$site == 1 | sesame$site == 4 | sesame$site == 5,]
sesameadv = sesame[sesame$site == 2 | sesame$site == 3,]

#Exploratory Data Analysis
par(mfrow = c(1, 3))

boxplot(sesame$diffbody ~ sesame$viewcat, horizontal = FALSE, main = "Overall Change in Body Scores",
        xlab = "Viewing Categories", ylab = "Body Pretest - Body Posttest", ylim = c(-25, 25))

boxplot(sesame$difflet ~ sesame$viewcat, horizontal = FALSE, main = "Overall Change in Letters Scores",
        xlab = "Viewing Categories", ylab = "Letters Pretest - Letters Posttest", ylim = c(-45,45))

boxplot(sesame$diffnumb ~ sesame$viewcat, horizontal = FALSE, main = "Overall Change in Numbers Scores",
        xlab = "Viewing Categories", ylab = "Numbers Pretest - Numbers Posttest", ylim = c(-45,45))

par(mfrow = c(3, 2))

boxplot(sesamedisadv$diffbody ~ sesamedisadv$viewcat, horizontal = FALSE,
        main = "Change in Body Scores for Disadvantaged Children", xlab = "Viewing Categories",
        ylab = "Body Pretest - Body Posttest", ylim = c(-25, 25))

boxplot(sesameadv$diffbody ~ sesameadv$viewcat, horizontal = FALSE,
        main = "Change in Body Scores for Advantaged Children", xlab = "Viewing Categories",
        ylab = "Body Pretest - Body Posttest", ylim = c(-25, 25))

boxplot(sesamedisadv$difflet ~ sesamedisadv$viewcat, horizontal = FALSE,
        main = "Change in Letters Scores for Disadvantaged Children", xlab = "Viewing Categories",
        ylab = "Letters Pretest - Letters Posttest", ylim = c(-45, 45))

boxplot(sesameadv$difflet ~ sesameadv$viewcat, horizontal = FALSE,
        main = "Change in Letters Scores for Advantaged Children", xlab = "Viewing Categories",
        ylab = "Letters Pretest - Letters Posttest", ylim = c(-45, 45))

boxplot(sesamedisadv$diffnumb ~ sesamedisadv$viewcat, horizontal = FALSE,
        main = "Change in Numbers Scores for Disadvantaged Children", xlab = "Viewing Categories",
        ylab = "Numbers Pretest - Numbers Posttest", ylim = c(-45, 45))

boxplot(sesameadv$diffnumb ~ sesameadv$viewcat, horizontal = FALSE,
        main = "Change in Numbers Scores for Advantaged Children", xlab = "Viewing Categories",
        ylab = "Numbers Pretest - Numbers Posttest", ylim = c(-45, 45))

#Regression
par(mfrow = c(2, 2))

body=lm(diffbody ~ factor(viewcat), data = sesame)
summary(body)
anova(body)
plot(body)

let=lm(difflet ~ factor(viewcat), data = sesame)
summary(let)
anova(let)
plot(let)

numb=lm(diffnumb ~ factor(viewcat), data = sesame)
summary(numb)
anova(numb)
plot(numb)


#adv vs. disadv
bodydisadv=lm(diffbody ~ factor(viewcat), data = sesamedisadv)
summary(bodydisadv)
anova(bodydisadv)
plot(bodydisadv, main = "bodydisadv")

bodyadv=lm(diffbody ~ factor(viewcat), data = sesameadv)
summary(bodyadv)
anova(bodyadv)
plot(bodyadv, main = "bodyadv")

letdisadv=lm(difflet ~ factor(viewcat), data = sesamedisadv)
summary(letdisadv)
anova(letdisadv)
plot(letdisadv, main = "letdisadv")

letadv=lm(difflet ~ factor(viewcat), data = sesameadv)
summary(letadv)
anova(letadv)
plot(letadv, main = "letadv")

numbdisadv=lm(diffnumb ~ factor(viewcat), data = sesamedisadv)
summary(numbdisadv)
anova(numbdisadv)
plot(numbdisadv, main = "numbdisadv")

numbadv=lm(diffnumb ~ factor(viewcat), data = sesameadv)
summary(numbadv)
anova(numbadv)
plot(numbadv, main = "numbadv")

#Looking for confounders
bodycon1=lm(diffbody ~ factor(viewcat) + factor(viewenc) + sex + age + factor(setting) + peabody, data = sesame)
bodycon2=lm(diffbody ~ factor(viewenc) + sex + age + factor(setting) + peabody, data = sesame)
anova(bodycon2, bodycon1)

letcon1=lm(difflet ~ factor(viewcat) + factor(viewenc) + sex + age + factor(setting) + peabody, data = sesame)
letcon2=lm(difflet ~ factor(viewenc) + sex + age + factor(setting) + peabody, data = sesame)
anova(letcon2, letcon1)

numbcon1=lm(diffnumb ~ factor(viewcat) + factor(viewenc) + sex + age + factor(setting) + peabody, data = sesame)
numbcon2=lm(diffnumb ~ factor(viewenc) + sex + age + factor(setting) + peabody, data = sesame)
anova(numbcon2, numbcon1)

##EXTRA##

#Comparing viewcat = 1 (rarely watch) to viewcat = 2, 3, 4

#combine viewcat 2-4
sesame$viewcatcombined = 1
for (i in 1:240){
  if (sesame$viewcat[i] == 1)
    sesame$viewcatcombined[i] = 0
}

par(mfrow = c(2, 2))

bodyc=lm(diffbody ~ factor(viewcatcombined), data = sesame)
summary(bodyc)
plot(bodyc)

letc=lm(difflet ~ factor(viewcatcombined), data = sesame)
summary(letc)
plot(letc)

numbc=lm(diffnumb ~ factor(viewcatcombined), data = sesame)
summary(numbc)
plot(numbc)

bodydisadvc=lm(diffbody ~ factor(viewcatcombined), data = sesamedisadv)
summary(bodydisadvc)
plot(bodydisadvc)

bodyadvc=lm(diffbody ~ factor(viewcatcombined), data = sesameadv)
summary(bodyadvc)
plot(bodyadvc)

letdisadvc=lm(difflet ~ factor(viewcatcombined), data = sesamedisadv)
summary(letdisadvc)
plot(letdisadvc)

letadvc=lm(difflet ~ factor(viewcatcombined), data = sesameadv)
summary(letadvc)
plot(letadvc)

numbdisadvc=lm(diffnumb ~ factor(viewcatcombined), data = sesamedisadv)
summary(numbdisadvc)
plot(numbdisadvc)

numbadvc=lm(diffnumb ~ factor(viewcatcombined), data = sesameadv)
summary(numbadvc)
plot(numbadvc)


#By site

#Splitting data by site
sesame1 = sesame[sesame$site == 1,]
sesame2 = sesame[sesame$site == 2,]
sesame3 = sesame[sesame$site == 3,]
sesame4 = sesame[sesame$site == 4,]
sesame5 = sesame[sesame$site == 5,]


body1=lm(diffbody ~ factor(viewcat), data = sesame1)
summary(body1)
boxplot(sesame1$diffbody ~ sesame1$viewcat)

let1=lm(difflet ~ factor(viewcat), data = sesame1)
summary(let1)
boxplot(sesame1$difflet ~ sesame1$viewcat)

numb1=lm(diffnumb ~ factor(viewcat), data = sesame1)
summary(numb1)
boxplot(sesame1$diffnumb ~ sesame1$viewcat)


body2=lm(diffbody ~ factor(viewcat), data = sesame2)
summary(body2)
boxplot(sesame2$diffbody ~ sesame2$viewcat)

let2=lm(difflet ~ factor(viewcat), data = sesame2)
summary(let2)
boxplot(sesame2$difflet ~ sesame2$viewcat)

numb2=lm(diffnumb ~ factor(viewcat), data = sesame2)
summary(numb2)
boxplot(sesame2$diffnumb ~ sesame2$viewcat)


body3=lm(diffbody ~ factor(viewcat), data = sesame3)
summary(body3)
boxplot(sesame3$diffbody ~ sesame3$viewcat)

let3=lm(difflet ~ factor(viewcat), data = sesame3)
summary(let3)
boxplot(sesame3$difflet ~ sesame3$viewcat)

numb3=lm(diffnumb ~ factor(viewcat), data = sesame3)
summary(numb3)
boxplot(sesame3$diffnumb ~ sesame3$viewcat)


body4=lm(diffbody ~ factor(viewcat), data = sesame4)
summary(body4)
boxplot(sesame4$diffbody ~ sesame4$viewcat)

let4=lm(difflet ~ factor(viewcat), data = sesame4)
summary(let4)
boxplot(sesame4$difflet ~ sesame4$viewcat)

numb4=lm(diffnumb ~ factor(viewcat), data = sesame4)
summary(numb4)
boxplot(sesame4$diffnumb ~ sesame4$viewcat)


body5=lm(diffbody ~ factor(viewcat), data = sesame5)
summary(body5)
boxplot(sesame5$diffbody ~ sesame5$viewcat)

let5=lm(difflet ~ factor(viewcat), data = sesame5)
summary(let5)
boxplot(sesame5$difflet ~ sesame5$viewcat)

numb5=lm(diffnumb ~ factor(viewcat), data = sesame5)
summary(numb5)
boxplot(sesame5$diffnumb ~ sesame5$viewcat)







boxplot(sesame$diffbody ~ sesame$viewcatcombined)

boxplot(sesame$difflet ~ sesame$viewcatcombined)

boxplot(sesame$diffnumb ~ sesame$viewcatcombined)



#By site

body1c=lm(diffbody ~ factor(viewcatcombined), data = sesame1)
summary(body1c)
boxplot(sesame1$diffbody ~ sesame1$viewcatcombined)

let1c=lm(difflet ~ factor(viewcatcombined), data = sesame1)
summary(let1c)
boxplot(sesame1$difflet ~ sesame1$viewcatcombined)

numb1c=lm(diffnumb ~ factor(viewcatcombined), data = sesame1)
summary(numb1c)
boxplot(sesame1$diffnumb ~ sesame1$viewcatcombined)


body2c=lm(diffbody ~ factor(viewcatcombined), data = sesame2)
summary(body2c)
boxplot(sesame2$diffbody ~ sesame2$viewcatcombined)

let2c=lm(difflet ~ factor(viewcatcombined), data = sesame2)
summary(let2c)
boxplot(sesame2$difflet ~ sesame2$viewcatcombined)

numb2c=lm(diffnumb ~ factor(viewcatcombined), data = sesame2)
summary(numb2c)
boxplot(sesame2$diffnumb ~ sesame2$viewcatcombined)


body3c=lm(diffbody ~ factor(viewcatcombined), data = sesame3)
summary(body3c)
boxplot(sesame3$diffbody ~ sesame3$viewcatcombined)

let3c=lm(difflet ~ factor(viewcatcombined), data = sesame3)
summary(let3c)
boxplot(sesame3$difflet ~ sesame3$viewcatcombined)

numb3c=lm(diffnumb ~ factor(viewcatcombined), data = sesame3)
summary(numb3c)
boxplot(sesame3$diffnumb ~ sesame3$viewcatcombined)


body4c=lm(diffbody ~ factor(viewcatcombined), data = sesame4)
summary(body4c)
boxplot(sesame4$diffbody ~ sesame4$viewcatcombined)

let4c=lm(difflet ~ factor(viewcatcombined), data = sesame4)
summary(let4c)
boxplot(sesame4$difflet ~ sesame4$viewcatcombined)

numb4c=lm(diffnumb ~ factor(viewcatcombined), data = sesame4)
summary(numb4c)
boxplot(sesame4$diffnumb ~ sesame4$viewcatcombined)


body5c=lm(diffbody ~ factor(viewcatcombined), data = sesame5)
summary(body5c)
boxplot(sesame5$diffbody ~ sesame5$viewcatcombined)

let5c=lm(difflet ~ factor(viewcatcombined), data = sesame5)
summary(let5c)
boxplot(sesame5$difflet ~ sesame5$viewcatcombined)

numb5c=lm(diffnumb ~ factor(viewcatcombined), data = sesame5)
summary(numb5c)
boxplot(sesame5$diffnumb ~ sesame5$viewcatcombined)

boxplot(sesamedisadv$diffbody ~ sesamedisadv$viewcatcombined)

boxplot(sesamedisadv$difflet ~ sesamedisadv$viewcatcombined)

boxplot(sesamedisadv$diffnumb ~ sesamedisadv$viewcatcombined)

boxplot(sesameadv$diffbody ~ sesameadv$viewcatcombined)

boxplot(sesameadv$difflet ~ sesameadv$viewcatcombined)

boxplot(sesameadv$diffnumb ~ sesameadv$viewcatcombined)
