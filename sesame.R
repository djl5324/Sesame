#Original data
sesame = read.csv("sesame.csv", header = T)

#Creating difference columns
sesame$diffbody = sesame$postbody - sesame$prebody
sesame$difflet = sesame$postlet - sesame$prelet
sesame$diffnumb = sesame$postnumb - sesame$prenumb

par(mfrow = c(1, 3))
#Comparing viewcat = 1 (rarely watch) to viewcat = 2, 3, 4
body=lm(diffbody ~ factor(viewcat), data = sesame)
summary(body)
boxplot(sesame$diffbody ~ sesame$viewcat, horizontal = FALSE, main = "Overall Change in Body Scores",
        xlab = "Viewing Categories", ylab = "Body Pretest - Body Posttest", ylim = c(-25, 25))

let=lm(difflet ~ factor(viewcat), data = sesame)
summary(let)
boxplot(sesame$difflet ~ sesame$viewcat, horizontal = FALSE, main = "Overall Change in Letters Scores",
        xlab = "Viewing Categories", ylab = "Letters Pretest - Letters Posttest", ylim = c(-45,45))

numb=lm(diffnumb ~ factor(viewcat), data = sesame)
summary(numb)
boxplot(sesame$diffnumb ~ sesame$viewcat, horizontal = FALSE, main = "Overall Change in Numbers Scores",
        xlab = "Viewing Categories", ylab = "Numbers Pretest - Numbers Posttest", ylim = c(-45,45))

#Splitting data by site
sesame1 = sesame[sesame$site == 1,]
sesame2 = sesame[sesame$site == 2,]
sesame3 = sesame[sesame$site == 3,]
sesame4 = sesame[sesame$site == 4,]
sesame5 = sesame[sesame$site == 5,]

#By site

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

#combine viewcat 2-4
sesame$viewcatcombined = 1
for (i in 1:240){
  if (sesame$viewcat[i] == 1)
    sesame$viewcatcombined[i] = 0
}

bodyc=lm(diffbody ~ factor(viewcatcombined), data = sesame)
summary(bodyc)
boxplot(sesame$diffbody ~ sesame$viewcatcombined)

letc=lm(difflet ~ factor(viewcatcombined), data = sesame)
summary(letc)
boxplot(sesame$difflet ~ sesame$viewcatcombined)

numbc=lm(diffnumb ~ factor(viewcatcombined), data = sesame)
summary(numbc)
boxplot(sesame$diffnumb ~ sesame$viewcatcombined)

#Splitting data by site
sesame1 = sesame[sesame$site == 1,]
sesame2 = sesame[sesame$site == 2,]
sesame3 = sesame[sesame$site == 3,]
sesame4 = sesame[sesame$site == 4,]
sesame5 = sesame[sesame$site == 5,]

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

#Looking for confounders
bodycon=lm(diffbody ~ factor(viewcatcombined) + factor(viewenc) + sex + age + factor(setting) + peabody, data = sesame)
summary(bodycon)

letcon=lm(difflet ~ factor(viewcatcombined) + factor(viewenc) + sex + age + factor(setting) + peabody, data = sesame)
summary(letcon)

numbcon=lm(diffnumb ~ factor(viewcatcombined) + factor(viewenc) + sex + age + factor(setting) + peabody, data = sesame)
summary(numbcon)

#Advantaged vs. disadvantaged
sesamedisadv = sesame[sesame$site == 1 | sesame$site == 4 | sesame$site == 5,]
sesameadv = sesame[sesame$site == 2 | sesame$site == 3,]

par(mfrow = c(3, 2))

bodydisadv=lm(diffbody ~ factor(viewcat), data = sesamedisadv)
summary(bodydisadv)
boxplot(sesamedisadv$diffbody ~ sesamedisadv$viewcat, horizontal = FALSE,
        main = "Change in Body Scores for Disadvantaged Children", xlab = "Viewing Categories",
        ylab = "Body Pretest - Body Posttest", ylim = c(-25, 25))

bodyadv=lm(diffbody ~ factor(viewcat), data = sesameadv)
summary(bodyadv)
boxplot(sesameadv$diffbody ~ sesameadv$viewcat, horizontal = FALSE,
        main = "Change in Body Scores for Advantaged Children", xlab = "Viewing Categories",
        ylab = "Body Pretest - Body Posttest", ylim = c(-25, 25))

letdisadv=lm(difflet ~ factor(viewcat), data = sesamedisadv)
summary(letdisadv)
boxplot(sesamedisadv$difflet ~ sesamedisadv$viewcat, horizontal = FALSE,
        main = "Change in Letters Scores for Disadvantaged Children", xlab = "Viewing Categories",
        ylab = "Letters Pretest - Letters Posttest", ylim = c(-45, 45))

letadv=lm(difflet ~ factor(viewcat), data = sesameadv)
summary(letadv)
boxplot(sesameadv$difflet ~ sesameadv$viewcat, horizontal = FALSE,
        main = "Change in Letters Scores for Advantaged Children", xlab = "Viewing Categories",
        ylab = "Letters Pretest - Letters Posttest", ylim = c(-45, 45))

numbdisadv=lm(diffnumb ~ factor(viewcat), data = sesamedisadv)
summary(numbdisadv)
boxplot(sesamedisadv$diffnumb ~ sesamedisadv$viewcat, horizontal = FALSE,
        main = "Change in Numbers Scores for Disadvantaged Children", xlab = "Viewing Categories",
        ylab = "Numbers Pretest - Numbers Posttest", ylim = c(-45, 45))

numbadv=lm(diffnumb ~ factor(viewcat), data = sesameadv)
summary(numbadv)
boxplot(sesameadv$diffnumb ~ sesameadv$viewcat, horizontal = FALSE,
        main = "Change in Numbers Scores for Advantaged Children", xlab = "Viewing Categories",
        ylab = "Numbers Pretest - Numbers Posttest", ylim = c(-45, 45))

bodydisadvc=lm(diffbody ~ factor(viewcatcombined), data = sesamedisadv)
summary(bodydisadvc)
boxplot(sesamedisadv$diffbody ~ sesamedisadv$viewcatcombined)

letdisadvc=lm(difflet ~ factor(viewcatcombined), data = sesamedisadv)
summary(letdisadvc)
boxplot(sesamedisadv$difflet ~ sesamedisadv$viewcatcombined)

numbdisadvc=lm(diffnumb ~ factor(viewcatcombined), data = sesamedisadv)
summary(numbdisadvc)
boxplot(sesamedisadv$diffnumb ~ sesamedisadv$viewcatcombined)

bodyadvc=lm(diffbody ~ factor(viewcatcombined), data = sesameadv)
summary(bodyadvc)
boxplot(sesameadv$diffbody ~ sesameadv$viewcatcombined)

letadvc=lm(difflet ~ factor(viewcatcombined), data = sesameadv)
summary(letadvc)
boxplot(sesameadv$difflet ~ sesameadv$viewcatcombined)

numbadvc=lm(diffnumb ~ factor(viewcatcombined), data = sesameadv)
summary(numbadvc)
boxplot(sesameadv$diffnumb ~ sesameadv$viewcatcombined)
