SSM = read_csv("/Users/Jasonmoy/Desktop/SSM.csv")
LSM = read_csv('/Users/Jasonmoy/Desktop/LSM.csv')

CESummaryTable = SSM %>% 
  group_by(FinalTestCondition,TrainingTask) %>% 
  summarise(MeanCorrectPercentage = mean(CorrectPercentage),
            MeanEfficiency = mean(`Efficiency(Word Learned /Minute)`)) %>% 
  filter(FinalTestCondition == "CE")
  
ECSummaryTable = SSM %>% 
  group_by(FinalTestCondition,TrainingTask) %>% 
  summarise(MeanCorrectPercentage = mean(CorrectPercentage),
            MeanEfficiency = mean(`Efficiency(Word Learned /Minute)`)) %>% 
  filter(FinalTestCondition == "EC")

ECSummaryTable = LSM %>% 
  group_by(FinalTestCondition,TrainingTask) %>% 
  summarise(MeanCorrectPercentage = mean(CorrectPercentage),
            MeanEfficiency = mean(`Efficiency (Word Learned/Minute)`)) %>% 
  filter(FinalTestCondition == "EC")
