require(car)
require(ggplot2)

setwd("~/Dropbox/Vital2/MSU/Communication")

##Nop-y test
nopy_data <- read.table("data_wo_tasks/reg_data/munged_reg_evolve_nopy.dat", h=T)
nopy_pop <- read.table("data_wo_tasks/reg_data/munged_reg_evolve_nopy_pop.dat", h=T)
nopy_data <- cbind(nopy_data, pop.size = nopy_pop$pop_size)
nopy_data <- cbind(nopy_data, norm.smart.explode = nopy_data$smart.explode/nopy_data$pop.size)
nopy_data <- cbind(nopy_data, norm.quorum.sense = nopy_data$quorum.sense/nopy_data$pop.size)
nopy_data <- cbind(nopy_data, norm.nop.y = nopy_data$nop.y/nopy_data$pop.size)
nopy_data_final <- subset(nopy_data, update==30000)
wilcox.test(nopy_data_final$norm.quorum.sense, nopy_data_final$norm.nop.y, conf.int=T)
wilcox.test(nopy_data_final$norm.smart.explode, nopy_data_final$norm.nop.y, conf.int=T)
median(nopy_data_final$norm.nop.y)
median(nopy_data_final$norm.quorum.sense)
median(nopy_data_final$norm.smart.explode)

regnoping <-data.frame(update = nopy_data$update, count=nopy_data$norm.nop.y, Instruction=rep("Neutral Instruction (nop-Y)", length(nopy_data$norm.nop.y)), xlabel=rep("nop-Y (a control)", length(nopy_data$norm.nop.y)))
regexploding <- data.frame(update = nopy_data$update, count = nopy_data$norm.smart.explode, Instruction=rep("Informed Suicidal Altruism (smart-explode)", length(nopy_data$norm.smart.explode)),xlabel=rep("smart-explode", length(nopy_data$norm.smart.explode)))
regsensing <- data.frame(update = nopy_data$update, count = nopy_data$norm.quorum.sense, Instruction=rep("Quorum Sensing (quorum-sense)", length(nopy_data$norm.quorum.sense)), xlabel=rep("quorum-sense", length(nopy_data$norm.quorum.sense)))
reg_test_data <- rbind(regexploding, regsensing, regnoping)

ggplot(data=reg_test_data, aes(x=update, y=count, group=Instruction, colour=Instruction)) + ylab("Mean Number of Instruction Executions Per Organism") + xlab("Evolutionary time (in updates)") + stat_summary(aes(color=Instruction, fill=Instruction),fun.data="mean_cl_boot", geom=c("smooth")) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE)

nopy_final <- subset(reg_test_data, update==30000)

ggplot(data=nopy_final, aes(x=xlabel, y=count, group=Instruction, colour=Instruction)) + ylab("Mean Number of Instruction Executions\n Per Organism") + xlab("Instruction") + geom_boxplot(width=0.5, aes(fill=Instruction)) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE) + opts(legend.position="none") + theme(axis.text=element_text(size=25), axis.title=element_text(size=25))


## Comparing explosions
qs_explode <- read.table("data_wo_tasks/reg_data/munged_reg_qs_explosions.dat", h=T)
popsize_nt <- read.table("data_wo_tasks/reg_data/munged_reg_qs_popsize.dat", h=T)
noqs_explode <- read.table("data_wo_tasks/reg_data/munged_reg_noqs_explosions.dat", h=T)
popsize_noqs <- read.table("data_wo_tasks/reg_data/munged_reg_noqs_popsize.dat", h=T)

qs_explode <- cbind(qs_explode, norm.kabooms = qs_explode$kabooms/popsize_nt$popsize)
noqs_explode <- cbind(noqs_explode, norm.kabooms = noqs_explode$kabooms/popsize_noqs$popsize)
median(subset(qs_explode, update==30000)$norm.kabooms)
median(subset(noqs_explode, update==30000)$norm.kabooms)
wilcox.test(subset(noqs_explode, update==30000)$norm.kabooms,subset(qs_explode, update==30000)$norm.kabooms, conf.int=T)

noqs_explosions <- data.frame(update = noqs_explode$update, count = noqs_explode$norm.kabooms, Instruction=rep("Informed Suicidal Altruism Without\n Quorum Sensing Available", length(noqs_explode$norm.kabooms)),xlabel=rep("non-quorum-sensing altruists", length(noqs_explode$norm.kabooms)))
qs_explosions <- data.frame(update = qs_explode$update, count = qs_explode$norm.kabooms, Instruction=rep("Informed Suicidal Altruism With\n Quorum Sensing Available\n", length(qs_explode$norm.kabooms)),xlabel=rep("quorum-sensing altruists", length(qs_explode$norm.kabooms)))
explode_comp <- rbind(qs_explosions, noqs_explosions)
explode_comp <- subset(explode_comp, update==30000)

ggplot(data=explode_comp, aes(x=xlabel, y=count, group=Instruction, colour=Instruction)) + ylab("Mean Number of Explosions \nPer Organism") + xlab("Treatment") + geom_boxplot(width=0.5, aes(fill=Instruction)) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE) + theme(axis.text=element_text(size=24), axis.title=element_text(size=26)) +opts(legend.position="none")


### Comparing avg orgs killed
ave_killed_nt <- read.table("data_wo_tasks/reg_data/munged_reg_qs_orgkills.dat", h=T)
popsize_nt <- read.table("data_wo_tasks/reg_data/munged_reg_qs_popsize.dat", h=T)
ave_killed_noqs <-read.table("data_wo_tasks/reg_data/munged_reg_noqs_orgkills.dat", h=T)
popsize_noqs <- read.table("data_wo_tasks/reg_data/munged_reg_noqs_popsize.dat", h=T)
ave_killed_nt <- cbind(ave_killed_nt, ave_killed = ave_killed_nt$orgs_killed/qs_explode$kabooms, norm_killed = (ave_killed_nt$orgs_killed/qs_explode$kabooms)/popsize_nt$popsize)
ave_killed_noqs <- cbind(ave_killed_noqs, ave_killed = ave_killed_noqs$orgs_killed/noqs_explode$kabooms, norm_killed = (ave_killed_noqs$orgs_killed/noqs_explode$kabooms)/popsize_noqs$popsize)
ave_killed_total <- rbind(ave_killed_nt, ave_killed_noqs)
ave_killed_total$treatment <- factor(ave_killed_total$treatment, labels = c("Quorum-Sensing Altruists", "Non-Quorum-Sensing Altruists"))
popsize_total <- rbind(popsize_nt, popsize_noqs)
popsize_total$treatment <- factor(popsize_total$treatment, labels=c("Quorum-Sensing Altruists", "Non-Quorum-Sensing Altruists"))

median(subset(ave_killed_nt, update==30000)$ave_killed)
median(subset(ave_killed_noqs, update==30000)$ave_killed)
wilcox.test(subset(ave_killed_nt, update==30000)$ave_killed, subset(ave_killed_noqs, update==30000)$ave_killed, conf.int=T)


ave_killed_total$ave_killed[ave_killed_total$ave_killed == "NaN"] = 0
ggplot(data=ave_killed_total, aes(x=update, y=ave_killed, group=treatment, colour=treatment)) + ylab("Mean Number of Organisms Killed \nper Explosion per 100 Updates") + xlab("Evolutionary time (in updates)") + stat_summary(aes(color=treatment, fill=treatment),fun.data="mean_cl_boot", geom=c("smooth")) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE) + theme(axis.text=element_text(size=15), axis.title=element_text(size=23)) + opts(legend.position="none")



## Pop size
ggplot(data=popsize_total, aes(x=update, y=popsize, group=treatment, colour=treatment)) + ylab("Mean Population Size Over Time") + xlab("Evolutionary time (in updates)") + stat_summary(aes(color=treatment, fill=treatment),fun.data="mean_cl_boot", geom=c("smooth")) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE)

##normalized by popsize?
ave_killed_total$norm_killed[ave_killed_total$norm_killed =="NaN"] = 0
ggplot(data=ave_killed_total, aes(x=update, y=norm_killed, group=treatment, colour=treatment)) + ylab("Mean Number of Organisms Killed per Explosion per 100 Updates\n Normalized by Population Size") + xlab("Evolutionary time (in updates)") + stat_summary(aes(color=treatment, fill=treatment),fun.data="mean_cl_boot", geom=c("smooth")) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE) 

median(subset(ave_killed_nt, update==30000)$norm_killed)
median(subset(ave_killed_noqs, update==30000)$norm_killed)
wilcox.test(subset(ave_killed_nt, update==30000)$norm_killed, subset(ave_killed_noqs, update==30000)$norm_killed, conf.int=T)

## Noisy nop-y Inst
noisy_nt <- read.table("data_wo_tasks/reg_data/munged_reg_noisy1_nopy.dat", h=T)
noisy01 <- read.table("data_wo_tasks/reg_data/munged_reg_noisy01_nopy.dat", h=T)

median(subset(noisy01, update==30000)$quorum.sense)
median(subset(noisy01, update==30000)$nop.y)
wilcox.test(subset(noisy01, update==30000)$quorum.sense,subset(noisy01, update==30000)$nop.y)

median(subset(noisy_nt, update==30000)$quorum.sense)
median(subset(noisy_nt, update==30000)$nop.y)
wilcox.test(subset(noisy_nt, update==30000)$quorum.sense,subset(noisy_nt, update==30000)$nop.y)

noisy_noping <-data.frame(update = noisy_nt$update, count=noisy_nt$nop.y, Instruction=rep("Neutral Instruction (nop-B)", length(noisy_nt$nop.y)), xlabel=rep("nop-Y", length(noisy_nt$nop.y)))
n_exploding <- data.frame(update = noisy_nt$update, count = noisy_nt$smart.explode, Instruction=rep("Informed Suicidal Altruism With Noise SD = 0.1\n (noisy-smart-explode .1)\n", length(noisy_nt$smart.explode)),xlabel=rep("noisy-smart\n-explode .1", length(noisy_nt$smart.explode)))
n_sensing <- data.frame(update = noisy_nt$update, count = noisy_nt$quorum.sense, Instruction=rep("Quorum Sensing With Noise SD = 0.1\n (noisy-quorum-sense .1)\n", length(noisy_nt$quorum.sense)), xlabel=rep("noisy-quorum\n-sense .1", length(noisy_nt$quorum.sense)))
noisy_data <- rbind(n_exploding, control.explode, n_sensing, noisy_noping)
noisy_data_final <- noisy_data[noisy_data$update == 30000,]

## Noise .01
noisy01_noping <-data.frame(update = noisy01$update, count=noisy01$nop.y, Instruction=rep("Neutral Instruction (nop-B)", length(noisy01$nop.b)), xlabel=rep("nop-B", length(noisy01$nop.b)))
n01_exploding <- data.frame(update = noisy01$update, count = noisy01$smart.explode, Instruction=rep("Informed Suicidal Altruism With Noise SD = 0.01\n (noisy-smart-explode .01)\n", length(noisy01$smart.explode)),xlabel=rep("noisy-smart\n-explode .01", length(noisy01$smart.explode)))
n01_sensing <- data.frame(update = noisy01$update, count = noisy01$noisy.quorum, Instruction=rep("Quorum Sensing With Noise SD = 0.01 \n(noisy-quorum-sense .01)\n", length(noisy01$noisy.quorum)), xlabel=rep("noisy-quorum\n-sense .01", length(noisy01$noisy.quorum)))
noisy_data <- rbind(n_exploding, n_sensing, n01_exploding, n01_sensing, noisy_noping)
noisy_data_final <- noisy_data[noisy_data$update == 30000,]

ggplot(data=noisy_data_final, aes(x=xlabel, y=count, group=Instruction, colour=Instruction)) + ylab("Mean Number of Instruction Executions At Final Update") + xlab("Instruction") + geom_boxplot(width=0.5, aes(fill=Instruction)) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE)


####threshold data
threshold <- read.table("data_wo_tasks/reg_data/munged_reg_evolve_thresh.dat", h=T)
median(subset(threshold, update==30000)$threshold)

threshold$threshold[threshold$threshold == "NaN"] = 0
noisy1_threshold <- read.table("data_wo_tasks/reg_data/munged_reg_noisy1_threshold.dat", h=T)
noisy01_threshold <- read.table("data_wo_tasks/reg_data/munged_reg_noisy01_threshold.dat", h=T)
noisy1_threshold$threshold[noisy1_threshold$threshold == "NaN"] = 0
noisy01_threshold$threshold[noisy01_threshold$threshold == "NaN"] = 0
threshold_lastupdate = threshold[threshold$update==30000,]
threshold$treatment <- factor(threshold$treatment, labels=c("Quorum Threshold"))
noisy_thresh1_lastupdate = noisy1_threshold[noisy1_threshold$update==30000,]
noisy_thresh01_lastupdate = noisy01_threshold[noisy01_threshold$update==30000,]

wilcox.test(threshold_lastupdate$threshold, noisy_thresh01_lastupdate$threshold, conf.int=T)
wilcox.test(threshold_lastupdate$threshold, noisy_thresh1_lastupdate$threshold, conf.int=T)
median(threshold_lastupdate$threshold)
wilcox.test(threshold_lastupdate$threshold, conf.int=T)
median(noisy_thresh1_lastupdate$threshold)
median(noisy_thresh01_lastupdate$threshold)
comb_threshold <- rbind(threshold, noisy01_threshold, noisy1_threshold)
comb_threshold$treatment <- factor(comb_threshold$treatment, labels=c("Quorum Threshold", "Noisy Quorum (SD = 0.01) Threshold", "Noisy Quorum (SD = 0.1) Threshold"))

ggplot(data=threshold, aes(x=update, y=threshold, group=treatment, colour=treatment)) + ylab("Mean Quorum Threshold\n(Percent of related organisms in neighborhood)") + xlab("Evolutionary time (in updates)") + ylim(0,100)+ stat_summary(aes(color=treatment, fill=treatment),fun.data="mean_cl_boot", geom=c("smooth")) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE) + theme(axis.text=element_text(size=23), axis.title=element_text(size=20)) + opts(legend.position="none")

ggplot(data=comb_threshold, aes(x=update, y=threshold, group=treatment, colour=treatment)) + ylab("Mean Quorum Threshold") + xlab("Evolutionary time (in updates)") + ylim(0,100)+ stat_summary(aes(color=treatment, fill=treatment),fun.data="mean_cl_boot", geom=c("smooth")) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE) 

#Comp 50%
lineage2_data <- read.table("data_wo_tasks/munged_finalcomps_lineage.dat",h=T)
lineage_data <- subset(lineage2_data, treatment=="comp50")
smart_data <- cbind(lineage_data, true_size = 100 * lineage_data$lineage, Lineage = rep("Quorum-Sensing Altruists", length(lineage_data)))
dumb_data = cbind(lineage_data, true_size = 100 - smart_data$true_size, Lineage = rep("Non-Quourm-Sensing Altruists", length(lineage_data)))
size_data = rbind(smart_data, dumb_data)

ggplot(data=size_data, aes(x=update, y=true_size, colour=Lineage, group=Lineage)) + ylab("Mean Percent of Organisms of Each Lineage") + xlab("Evolutionary time (in updates)") + ylim(0,100)+ stat_summary(aes(color=Lineage, fill=Lineage),fun.data="mean_cl_boot", geom=c("smooth")) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE) + theme(axis.text=element_text(size=23), axis.title=element_text(size=20))



#Comp QS 95%

lineage_qs95 <- subset(lineage2_data, treatment=="compqs95")
smart_qs95 <- cbind(lineage_qs95, true_size=100*lineage_qs95$lineage, Lineage = rep("Quorum-Sensing Altruists", length(lineage_qs95)))
dumb_qs95 <- cbind(lineage_qs95, true_size = 100 - smart_qs95$true_size, Lineage=rep("Non-Quorum-Sensing Altruists", length(lineage_qs95)))
size_qs95_data <- rbind(smart_qs95, dumb_qs95)

ggplot(data=size_qs95_data, aes(x=update, y=true_size, group=Lineage, colour=Lineage)) + ylab("Mean Percent of Organisms of Each Lineage") + xlab("Evolutionary time (in updates)") + ylim(0,100)+ stat_summary(aes(color=Lineage, fill=Lineage),fun.data="mean_cl_boot", geom=c("smooth")) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE) + theme(axis.text=element_text(size=23), axis.title=element_text(size=20))

size_qs95_data <- subset(size_qs95_data, update==30000)

ggplot(data=size_qs95_data, aes(x=lineage, y=true_size, group=lineage, colour=lineage)) + ylab("Mean Percent of Organisms with Each Instruction") + xlab("Lineage") + geom_boxplot(width=0.5, aes(fill=lineage)) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE)

### Comp 95%
lineage_95 <- subset(lineage2_data, treatment=="comp95")
smart_95 <- cbind(lineage_95, true_size=100*lineage_95$lineage, Lineage = rep("Quorum-Sensing Altruists", length(lineage_95)))
dumb_95 <- cbind(lineage_95, true_size = 100 - smart_95$true_size, Lineage=rep("Non-Quorum-Sensing Altruists", length(lineage_95)))
size_95_data <- rbind(smart_95, dumb_95)

ggplot(data=size_95_data, aes(x=update, y=true_size, group=Lineage, colour=Lineage)) + ylab("Mean Percent of Organisms of Each Lineage") + xlab("Evolutionary time (in updates)") + ylim(0,100)+ stat_summary(aes(color=Lineage, fill=Lineage),fun.data="mean_cl_boot", geom=c("smooth")) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE) + theme(axis.text=element_text(size=23), axis.title=element_text(size=20))

ggplot(data=size_95_data, aes(x=lineage, y=true_size, group=lineage, colour=lineage)) + ylab("Mean Percent of Organisms with Each Instruction") + xlab("Lineage") + geom_boxplot(width=0.5, aes(fill=lineage)) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE)

### Noisy comp
noisy_comp1_lineage = read.table("data_wo_tasks/reg_data/munged_reg_noqs_noisy1comp.dat", header=TRUE)
noisy_comp01_lineage = read.table("data_wo_tasks/reg_data/munged_reg_noqs_noisy01comp.dat", header=TRUE)

smartnoise_data = data.frame(update = noisy_comp1_lineage$update, true_size = 100 * noisy_comp1_lineage$lineage_perc, lineage = rep("Quorum-Sensing Altruists With Noise SD = 0.1", length(noisy_comp1_lineage)))
dumbnoise_data = data.frame(update = noisy_comp1_lineage$update, true_size = 100 - smartnoise_data$true_size, lineage = rep("Non-Quorum-Sensing Altruists When Noise SD = 0.1", length(noisy_comp1_lineage)))
smart_01 = data.frame(update = noisy_comp01_lineage$update, true_size = 100*noisy_comp01_lineage$lineage_perc, lineage=rep("Quorum-Sensing Altruists With Noise SD = 0.01", length(noisy_comp01_lineage)))
dumb_01 = data.frame(update=noisy_comp01_lineage$update, true_size = 100 - smart_01$true_size, lineage = rep("Non-Quorum-Sensing Altruists When Noise SD = 0.01", length(noisy_comp01_lineage)))

totalnoise_data = rbind(smartnoise_data,dumbnoise_data, smart_01, dumb_01)
ggplot(data=totalnoise_data, aes(x=update, y=true_size, group=lineage, colour=lineage)) + ylab("Mean Percent of Organisms with Each Instruction") + xlab("Evolutionary time (in updates)") + stat_summary(aes(color=lineage, fill=lineage),fun.data="mean_cl_boot", geom=c("smooth")) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE) 


