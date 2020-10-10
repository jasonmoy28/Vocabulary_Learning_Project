SSM = read_csv('/Users/Jasonmoy/Desktop/Research/Vocabulary Learing Project/Final Product/Picture/Generating Code/SSM.csv')
LSM = read_csv('/Users/Jasonmoy/Desktop/Research/Vocabulary Learing Project/Final Product/Picture/Generating Code/LSM.csv')

SSM_E_mean = SSM %>% group_by(FinalTestCondition,TrainingTask) %>% summarise(mean = mean(`Efficiency(Word Learned /Minute)`))
LSM_E_mean = LSM %>% group_by(FinalTestCondition,TrainingTask) %>% summarise(mean = mean(`Efficiency (Word Learned/Minute)`))

StrictScoringSummaryStatEfficiency = 
  Rmisc::summarySEwithin(SSM,
                         idvar = 'Subject',
                         withinvars = 'TrainingTask',
                         betweenvars = 'FinalTestCondition',
                         measurevar = 'Efficiency(Word Learned /Minute)') %>% 
  mutate(`Efficiency(Word Learned /Minute)` = SSM_E_mean$mean) %>% 
  rename(Efficiency = `Efficiency(Word Learned /Minute)`)



LenientScoringSummaryStatEfficiency = 
  Rmisc::summarySEwithin(LSM,
                         idvar = 'Subject',
                         withinvars = 'TrainingTask',
                         betweenvars = 'FinalTestCondition',
                         measurevar = 'Efficiency (Word Learned/Minute)') %>% 
  mutate(`Efficiency (Word Learned/Minute)` = LSM_E_mean$mean) %>% 
  rename(Efficiency = `Efficiency (Word Learned/Minute)`)

# SS + Productive
SS_productive_E = StrictScoringSummaryStatEfficiency %>% filter(FinalTestCondition == 'CE') %>% 
  ggplot(data =.,
         aes(x = TrainingTask,y = Efficiency,fill = TrainingTask)) +
  geom_bar(stat = 'identity',width = 0.5,color = 'black') + 
  geom_errorbar(aes(ymin = Efficiency - se, ymax = Efficiency + se), 
                position=position_dodge(0.9),width = 0.05) + 
  labs(y = 'Efficiency (Word Learned/Minute)',
       x = 'Strict Scoring - Productive Final Test') + # edit title here 
  scale_fill_manual(values = c('#eff3ff','#bdd7e7','#6baed6','#2171b5'),
                    labels = c('Productive','Receptive','Picture + Producive','Restudy'),
                    name = '') + 
  scale_x_discrete(labels=c("CE" = "Productive", "EC" = "Receptive",
                            "PI" = "Productive + Picture", 'RE' = 'Restudy')) + 
  ylim(0,1.5) + 
  theme_classic() +
  theme(legend.position=c(0.5,.9),
        legend.key.size = unit(0.4,'cm'),
        legend.direction = 'horizontal',
        axis.title.x = element_text(size = 13,vjust = -.6),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        aspect.ratio = .85, 
        panel.background = element_rect(colour = "black", size=1)) + 
  geom_text(aes(label = format(Efficiency,digits = 3)),
            vjust = 2.7,
            colour = c('#000000','#000000','#000000','#FFFFFF'))

## SS + receptive 
SS_receptive_E = StrictScoringSummaryStatEfficiency %>% filter(FinalTestCondition == 'EC') %>% 
  ggplot(data =.,
         aes(x = TrainingTask,y = Efficiency,fill = TrainingTask)) +
  geom_bar(stat = 'identity',width = 0.5,color = 'black') + 
  geom_errorbar(aes(ymin = Efficiency - se, ymax = Efficiency + se), 
                position=position_dodge(0.9),width = 0.05) + 
  labs(y = 'Efficiency (Word Learned/Minute)',
       x = 'Strict Scoring - Receptive Final Test') + # edit title here 
  scale_fill_manual(values = c('#eff3ff','#bdd7e7','#6baed6','#2171b5'),
                    labels = c('Productive','Receptive','Picture + Producive','Restudy'),
                    name = '') + 
  scale_x_discrete(labels=c("CE" = "Productive", "EC" = "Receptive",
                            "PI" = "Productive + Picture", 'RE' = 'Restudy')) + 
  ylim(0,1.5) + 
  theme_classic() +
  theme(legend.position=c(0.5,.9),
        legend.key.size = unit(0.4,'cm'),
        legend.direction = 'horizontal',
        axis.title.x = element_text(size = 13,vjust = -.6),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        aspect.ratio = .85, 
        panel.background = element_rect(colour = "black", size=1)) + 
  geom_text(aes(label = format(Efficiency,digits = 3)),
            vjust = 2.7,
            colour = c('#000000','#000000','#000000','#FFFFFF'))


# LS + Productive
LS_productive_E = LenientScoringSummaryStatEfficiency %>% filter(FinalTestCondition == 'CE') %>% 
  ggplot(data =.,
         aes(x = TrainingTask,y = Efficiency,fill = TrainingTask)) +
  geom_bar(stat = 'identity',width = 0.5,color = 'black') + 
  geom_errorbar(aes(ymin = Efficiency - se, ymax = Efficiency + se), 
                position=position_dodge(0.9),width = 0.05) + 
  labs(y = 'Efficiency (Word Learned/Minute)',
       x = 'Lenient Scoring - Productive Final Test') + # edit title here 
  scale_fill_manual(values = c('#eff3ff','#bdd7e7','#6baed6','#2171b5'),
                    labels = c('Productive','Receptive','Picture + Producive','Restudy'),
                    name = '') + 
  scale_x_discrete(labels=c("CE" = "Productive", "EC" = "Receptive",
                            "PI" = "Productive + Picture", 'RE' = 'Restudy')) + 
  ylim(0,1.5) + 
  theme_classic() +
  theme(legend.position=c(0.5,.9),
        legend.key.size = unit(0.4,'cm'),
        legend.direction = 'horizontal',
        axis.title.x = element_text(size = 13,vjust = -.6),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        aspect.ratio = .85, 
        panel.background = element_rect(colour = "black", size=1)) + 
  geom_text(aes(label = format(Efficiency,digits = 3)),
            vjust = 2.7,
            colour = c('#000000','#000000','#000000','#FFFFFF'))


#LS + receptive
LS_receptive_E = LenientScoringSummaryStatEfficiency %>% filter(FinalTestCondition == 'EC') %>% 
  ggplot(data =.,
         aes(x = TrainingTask,y = Efficiency,fill = TrainingTask)) +
  geom_bar(stat = 'identity',width = 0.5,color = 'black') + 
  geom_errorbar(aes(ymin = Efficiency - se, ymax = Efficiency + se), 
                position=position_dodge(0.9),width = 0.05) + 
  labs(y = 'Efficiency (Word Learned/Minute)',
       x = 'Lenient Scoring - Receptive Final Test') + # edit title here 
  scale_fill_manual(values = c('#eff3ff','#bdd7e7','#6baed6','#2171b5'),
                    labels = c('Productive','Receptive','Picture + Producive','Restudy'),
                    name = '') + 
  scale_x_discrete(labels=c("CE" = "Productive", "EC" = "Receptive",
                            "PI" = "Productive + Picture", 'RE' = 'Restudy')) + 
  ylim(0,1.5) + 
  theme_classic() +
  theme(legend.position=c(0.5,.9),
        legend.key.size = unit(0.4,'cm'),
        legend.direction = 'horizontal',
        axis.title.x = element_text(size = 13,vjust = -.6),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        aspect.ratio = .85, 
        panel.background = element_rect(colour = "black", size=1)) + 
  geom_text(aes(label = format(Efficiency,digits = 3)),
            vjust = 2.7,
            colour = c('#000000','#000000','#000000','#FFFFFF'))

# Output picture 
jpg_name = paste('SS_productive_E','.jpg',sep = '')
jpeg(jpg_name,width = 550,height = 430,res = 100)
SS_productive_E
dev.off()

jpg_name = paste('SS_receptive_E','.jpg',sep = '')
jpeg(jpg_name,width = 550,height = 430,res = 100)
SS_receptive_E
dev.off()

jpg_name = paste('LS_productive_E','.jpg',sep = '')
jpeg(jpg_name,width = 550,height = 430,res = 100)
LS_productive_E
dev.off()

jpg_name = paste('LS_receptive_E','.jpg',sep = '')
jpeg(jpg_name,width = 550,height = 430,res = 100)
LS_receptive_E
dev.off()




