SSM = read_csv('/Users/Jasonmoy/Desktop/Research/Vocabulary Learing Project/Final Product/Picture/Generating Code/SSM.csv')
LSM = read_csv('/Users/Jasonmoy/Desktop/Research/Vocabulary Learing Project/Final Product/Picture/Generating Code/LSM.csv')

SSM_PC_mean = SSM %>% group_by(FinalTestCondition,TrainingTask) %>% summarise(mean = mean(CorrectPercentage))
LSM_PC_mean = LSM %>% group_by(FinalTestCondition,TrainingTask) %>% summarise(mean = mean(CorrectPercentage))

StrictScoringSummaryStatCorrect = 
  Rmisc::summarySEwithin(SSM,
                         idvar = 'Subject',
                         withinvars = 'TrainingTask',
                         betweenvars = 'FinalTestCondition',
                         measurevar = 'CorrectPercentage') %>% 
  mutate(CorrectPercentage = SSM_PC_mean$mean)

LenientScoringSummaryStatCorrect = 
  Rmisc::summarySEwithin(LSM,
                         idvar = 'Subject',
                         withinvars = 'TrainingTask',
                         betweenvars = 'FinalTestCondition',
                         measurevar = 'CorrectPercentage') %>% 
  mutate(CorrectPercentage = LSM_PC_mean$mean)

# Strict scoring + productive posttest 
SS_productive_PC = StrictScoringSummaryStatCorrect %>% filter(FinalTestCondition == 'CE') %>% 
  ggplot(data =.,
         aes(x = TrainingTask,y = CorrectPercentage,fill = TrainingTask)) +
  geom_bar(stat = 'identity',width = 0.5,color = 'black') + 
  geom_errorbar(aes(ymin = CorrectPercentage - se, ymax = CorrectPercentage + se), position=position_dodge(0.9),
                width = 0.05) + 
  labs(y = 'Proportion Correct',
       x = 'Strict Scoring - Productive Final Test') + # edit title here 
  scale_fill_manual(values = c('#eff3ff','#bdd7e7','#6baed6','#2171b5'),
                    labels = c('Productive','Receptive','Picture + Producive','Restudy'),
                    name = '') + 
  scale_x_discrete(labels=c("CE" = "Productive", "EC" = "Receptive",
                            "PI" = "Productive + Picture", 'RE' = 'Restudy')) + 
  ylim(0,0.5) + 
  theme_classic() +
  theme(legend.position=c(0.5,.9),
        legend.key.size = unit(0.4,'cm'),
        legend.direction = 'horizontal',
        axis.title.x = element_text(size = 13,vjust = -.6),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        aspect.ratio = .85, 
        panel.background = element_rect(colour = "black", size=1)) + 
  geom_text(aes(label = format(CorrectPercentage,digits = 3)),
            vjust = 2.7,
            colour = c('#000000','#000000','#000000','#FFFFFF'))
  
# Strict Scoring - Receptive Final Test
SS_receptive_PC = SS_productive_graph = StrictScoringSummaryStatCorrect %>% filter(FinalTestCondition == 'EC') %>% 
  ggplot(data =.,
         aes(x = TrainingTask,y = CorrectPercentage,fill = TrainingTask)) +
  geom_bar(stat = 'identity',width = 0.5,color = 'black') + 
  geom_errorbar(aes(ymin = CorrectPercentage - se, ymax = CorrectPercentage + se), position=position_dodge(0.9),
                width = 0.05) + 
  labs(y = 'Proportion Correct',
       x = 'Strict Scoring - Receptive Final Test') + # edit title here 
  scale_fill_manual(values = c('#eff3ff','#bdd7e7','#6baed6','#2171b5'),
                    labels = c('Productive','Receptive','Picture + Producive','Restudy'),
                    name = '') + 
  scale_x_discrete(labels=c("CE" = "Productive", "EC" = "Receptive",
                            "PI" = "Productive + Picture", 'RE' = 'Restudy')) + 
  ylim(0,0.5) + 
  theme_classic() +
  theme(legend.position=c(0.5,.9),
        legend.key.size = unit(0.4,'cm'),
        legend.direction = 'horizontal',
        axis.title.x = element_text(size = 13,vjust = -.6),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        aspect.ratio = .85, 
        panel.background = element_rect(colour = "black", size=1)) + 
  geom_text(aes(label = format(CorrectPercentage,digits = 3)),
            vjust = 2.7,
            colour = c('#000000','#000000','#000000','#FFFFFF'))

# Lenient Scoring + Productive
LS_productive_PC = LenientScoringSummaryStatCorrect %>% filter(FinalTestCondition == 'CE') %>% 
  ggplot(data =.,
         aes(x = TrainingTask,y = CorrectPercentage,fill = TrainingTask)) +
  geom_bar(stat = 'identity',width = 0.5,color = 'black') + 
  geom_errorbar(aes(ymin = CorrectPercentage - se, ymax = CorrectPercentage + se), position=position_dodge(0.9),
                width = 0.05) + 
  labs(y = 'Proportion Correct',
       x = 'Lenient Scoring - Productive Final Test') + # edit title here 
  scale_fill_manual(values = c('#eff3ff','#bdd7e7','#6baed6','#2171b5'),
                    labels = c('Productive','Receptive','Picture + Producive','Restudy'),
                    name = '') + 
  scale_x_discrete(labels=c("CE" = "Productive", "EC" = "Receptive",
                            "PI" = "Productive + Picture", 'RE' = 'Restudy')) + 
  ylim(0,0.5) + 
  theme_classic() +
  theme(legend.position=c(0.5,.9),
        legend.key.size = unit(0.4,'cm'),
        legend.direction = 'horizontal',
        axis.title.x = element_text(size = 13,vjust = -.6),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        aspect.ratio = .85, 
        panel.background = element_rect(colour = "black", size=1)) + 
  geom_text(aes(label = format(CorrectPercentage,digits = 3)),
            vjust = 2.7,
            colour = c('#000000','#000000','#000000','#FFFFFF'))

# Lenient Scoring + Receptive
LS_receptive_PC = LenientScoringSummaryStatCorrect %>% filter(FinalTestCondition == 'EC') %>% 
  ggplot(data =.,
         aes(x = TrainingTask,y = CorrectPercentage,fill = TrainingTask)) +
  geom_bar(stat = 'identity',width = 0.5,color = 'black') + 
  geom_errorbar(aes(ymin = CorrectPercentage - se, ymax = CorrectPercentage + se), position=position_dodge(0.9),
                width = 0.05) + 
  labs(y = 'Proportion Correct',
       x = 'Lenient Scoring - Receptive Final Test') + # edit title here 
  scale_fill_manual(values = c('#eff3ff','#bdd7e7','#6baed6','#2171b5'),
                    labels = c('Productive','Receptive','Picture + Producive','Restudy'),
                    name = '') + 
  scale_x_discrete(labels=c("CE" = "Productive", "EC" = "Receptive",
                            "PI" = "Productive + Picture", 'RE' = 'Restudy')) + 
  ylim(0,0.5) + 
  theme_classic() +
  theme(legend.position=c(0.5,.9),
        legend.key.size = unit(0.4,'cm'),
        legend.direction = 'horizontal',
        axis.title.x = element_text(size = 13,vjust = -.6),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        aspect.ratio = .85, 
        panel.background = element_rect(colour = "black", size=1)) + 
  geom_text(aes(label = format(CorrectPercentage,digits = 3)),
            vjust = 2.7,
            colour = c('#000000','#000000','#000000','#FFFFFF'))


# Export as JPG
jpg_name = paste('SS_productive_PC','.jpg',sep = '')
jpeg(jpg_name,width = 550,height = 430,res = 100)
SS_productive_PC
dev.off()

jpg_name = paste('SS_receptive_PC','.jpg',sep = '')
jpeg(jpg_name,width = 550,height = 430,res = 100)
SS_receptive_PC
dev.off()

jpg_name = paste('LS_productive_PC','.jpg',sep = '')
jpeg(jpg_name,width = 550,height = 430,res = 100)
LS_productive_PC
dev.off()

jpg_name = paste('LS_receptive_PC','.jpg',sep = '')
jpeg(jpg_name,width = 550,height = 430,res = 100)
LS_receptive_PC
dev.off()
