
## Let's look at the relationship between social media use and partisan behavior
## during the 2010 congressional elections.

# Get the data (original dataset from Pew Research Center's 2010 post midterm election survey)
# Disclaimer -- I've modified this dataset from its original format. Any mistakes are my own and 
# not Pew's.
path<-file.path("C:","Users","family","Documents","R","Projects",
                "Social Media and Partisanship","pew_post_election_data_november_2010.csv")
social<-read.csv(path) # After you download the .csv file, upload it in whatever way is easiest for you.

# Install and open the "car" library so we can do some recodes:
install.packages("car") 
library(car)

# Turn variables of interest 
  # Liberal Ideology (How.liberal)
  # Ideological Intensity (Ideological.intensity)
  # whether respondents have shared political content on social media (Share)
  # discussed politics online (Discuss)
  # friended a political group, candidate, or campaign (Friend.of.candidate.or.political.group.on.SNS)
    # I apologize for how long winded that variable is...
  # joined an online political group (Post.content.related.to.a.candidate.or.campaign)
    # I'm sorry about that one, too...
  # posted online content related to politics
    # And that one...
# into factors:

social$How.liberal.factor<-Recode(social$How.liberal,
                                  "1='1. Very Conservative';
                                  2='2. Conservative';
                                  3='3. Moderate';
                                  4='4. Liberal';
                                  5='5. Very Liberal';
                                  NA='No Response'",
                                  as.factor=TRUE)

social$Ideological.intensity.factor<-Recode(social$Ideological.intensity,
                                            "1='1. Moderate';
                                            2='2. Conservative or Liberal';
                                            3='3. Very Conservative or Very Liberal';
                                            NA='No Response'",
                                            as.factor=TRUE)

social$Share.factor<-Recode(social$Share,
                            "0='1. Have Not Shared Political Content';
                            1='2. Have Shared Political Content';
                            NA='3. No Response'",
                            as.factor=TRUE)

social$Discuss.factor<-Recode(social$Discuss,
                              "0='1. Have Never Participated in an Online Political Discussion';
                              1='2. Have Participated in an Online Political Discussion';
                              NA='3. No Response'",
                              as.factor=TRUE)

social$Friend.factor<-Recode(social$Friend.of.candidate.or.political.group.on.SNS,
                             "0='1. Not an Online Friend of a Congressional Candidate and/or Political Group';
                             1='2. Friend of a Congressional Candidate and/or Political Group';
                             NA='3. No Response'",
                             as.factor=TRUE)

social$Post.factor<-Recode(social$Post.content.related.to.a.candidate.or.campaign,
                           "0='1. Have Never Posted Content Online Related to a Candidate or Campaign';
                           1='2. Have Posted Content Online Related to a Candidate or Campaign';
                           NA='3. No Response'",
                           as.factor=TRUE)

social$Join.factor<-Recode(social$Join.a.political.group.on.SNS,
                           "0='1. Have Never Joined a Political Group on Social Media';
                           1='2. Have Joined a Political Group on Social Media';
                           NA='3. No Response'",
                           as.factor=TRUE)

# Install and open the "dpylr" package so we can filter the data to make a new data frame
# where respondents who didn't report using social media are eliminated:
install.packages("dplyr")
library(dplyr)

social.SNS <- filter(social, Use.SNS == 1)
social.SNScon <- filter(social.SNS, How.liberal.factor=="1. Very Conservative"|How.liberal.factor=="2. Conservative"|How.liberal.factor=="3. Moderate"|How.liberal.factor=="No Response")
length(social.SNScon$Rvote) # 744
social.SNSlib <- filter(social.SNS, How.liberal.factor=="3. Moderate"|How.liberal.factor=="4. Liberal"|How.liberal.factor=="5. Very Liberal"|How.liberal.factor=="No Response")
length(social.SNSlib$Rvote) # 581

# Install and open the "ggplot2" library:
install.packages("ggplot2")
library(ggplot2)

length(social$How.liberal.factor) # 2,257 total observations in the "social" dataset.

# Bar graph: total # of respondents by ideological identification.
ggplot(social,aes(How.liberal.factor)) +
  geom_bar() + theme_classic() +
  xlab("Number of Observations = 2,257") + ylab("") + 
  ggtitle("Ideological Identification of All Survey Respondents",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

length(social.SNS$How.liberal.factor) # 925 total observations in the "social.SNS" dataset.

# Bar graph: total # of respondents who use social media by ideological identification.
ggplot(social.SNS,aes(How.liberal.factor)) +
  geom_bar() + theme_classic() +
  xlab("Number of Observations = 925") + ylab("") + 
  ggtitle("Ideological Identification of Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

# Make a new data frame with stacked group percentages so that we can graph percentages rather
# than count data:

# Who shares the most political content online?
social.SNS1<-data.frame(prop.table(table(social.SNS$Ideological.intensity.factor,
                                         social.SNS$Share.factor),1))

social.SNScon1<-data.frame(prop.table(table(social.SNScon$Ideological.intensity.factor,
                                            social.SNScon$Share.factor),1))

social.SNSlib1<-data.frame(prop.table(table(social.SNSlib$Ideological.intensity.factor,
                                            social.SNSlib$Share.factor),1))

ggplot(social.SNS1,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 925") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Have Not Shared Political Content",
                            "Have Shared Political Content",
                            "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Conservative or Liberal","Very Conservative or Very Liberal","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

ggplot(social.SNScon1,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 744") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Have Not Shared Political Content",
                             "Have Shared Political Content",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Conservative","Very Conservative","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Moderate to Conservative Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

ggplot(social.SNSlib1,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 581") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Have Not Shared Political Content",
                             "Have Shared Political Content",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Liberal","Very Liberal","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Moderate to Liberal Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")


# Who is more likely to engage in online discussions about politics?
social.SNS2<-data.frame(prop.table(table(social.SNS$Ideological.intensity.factor,
                                         social.SNS$Discuss.factor),1))

social.SNScon2<-data.frame(prop.table(table(social.SNScon$Ideological.intensity.factor,
                                            social.SNScon$Discuss.factor),1))

social.SNSlib2<-data.frame(prop.table(table(social.SNSlib$Ideological.intensity.factor,
                                            social.SNSlib$Discuss.factor),1))

ggplot(social.SNS2,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 925") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Have Never Engaged in Online\nDiscussion about Politics",
                             "Have Engaged in Online Discussion\nabout Politics",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Conservative or Liberal","Very Conservative or Very Liberal","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

ggplot(social.SNScon2,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 744") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Have Never Engaged in Online\nDiscussion about Politics",
                             "Have Engaged in Online Discussion\nabout Politics",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Conservative","Very Conservative","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Moderate to Conservative Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

ggplot(social.SNSlib2,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 581") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Have Never Engaged in Online\nDiscussion about Politics",
                             "Have Engaged in Online Discussion\nabout Politics",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Liberal","Very Liberal","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Moderate to Liberal Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

# Who is more likely to be a friend of an online political group or candidate?
social.SNS3<-data.frame(prop.table(table(social.SNS$Ideological.intensity.factor,
                                         social.SNS$Friend.factor),1))

social.SNScon3<-data.frame(prop.table(table(social.SNScon$Ideological.intensity.factor,
                                            social.SNScon$Friend.factor),1))

social.SNSlib3<-data.frame(prop.table(table(social.SNSlib$Ideological.intensity.factor,
                                            social.SNSlib$Friend.factor),1))

ggplot(social.SNS3,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 925") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Not an Online Friend of a Political\nGroup and/or Candidate",
                             "A Friend of an Online Political Group\n and/or Candidate",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Conservative or Liberal","Very Conservative or Very Liberal","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

ggplot(social.SNScon3,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 744") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Not an Online Friend of a Political\nGroup and/or Candidate",
                             "A Friend of an Online Political Group\n and/or Candidate",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Conservative","Very Conservative","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Moderate to Conservative Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

ggplot(social.SNSlib3,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 581") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Not an Online Friend of a Political\nGroup and/or Candidate",
                             "A Friend of an Online Political Group\n and/or Candidate",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Liberal","Very Liberal","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Moderate to Liberal Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

# Who is more likely to join an online political group?
social.SNS4<-data.frame(prop.table(table(social.SNS$Ideological.intensity.factor,
                                         social.SNS$Join.factor),1))

social.SNScon4<-data.frame(prop.table(table(social.SNScon$Ideological.intensity.factor,
                                            social.SNScon$Join.factor),1))

social.SNSlib4<-data.frame(prop.table(table(social.SNSlib$Ideological.intensity.factor,
                                            social.SNSlib$Join.factor),1))

ggplot(social.SNS4,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 925") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Have Never Joined an Online\nPolitical Group",
                             "Have Joined an Online Political\nGroup",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Conservative or Liberal","Very Conservative or Very Liberal","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

ggplot(social.SNScon4,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 744") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Have Never Joined an Online\nPolitical Group",
                             "Have Joined an Online Political\nGroup",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Conservative","Very Conservative","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Moderate to Conservative Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

ggplot(social.SNSlib4,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 581") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Have Never Joined an Online\nPolitical Group",
                             "Have Joined an Online Political\nGroup",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Liberal","Very Liberal","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Moderate to Liberal Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

# Who is more likely to post political content?
social.SNS5<-data.frame(prop.table(table(social.SNS$Ideological.intensity.factor,
                                         social.SNS$Post.factor),1))

social.SNScon5<-data.frame(prop.table(table(social.SNScon$Ideological.intensity.factor,
                                            social.SNScon$Post.factor),1))

social.SNSlib5<-data.frame(prop.table(table(social.SNSlib$Ideological.intensity.factor,
                                            social.SNSlib$Post.factor),1))

ggplot(social.SNS5,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 925") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Have Never Posted Political\nContent Online",
                             "Have Posted Political Content/nOnline",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Conservative or Liberal","Very Conservative or Very Liberal","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

ggplot(social.SNScon5,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 744") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Have Never Posted Political\nContent Online",
                             "Have Posted Political Content/nOnline",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Conservative","Very Conservative","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Moderate to Conservative Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

ggplot(social.SNSlib5,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() +
  ylab("Number of Observations = 581") + xlab("") + 
  theme(legend.title=element_blank()) + coord_flip() +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("steelblue4","goldenrod4","grey25"),
                    labels=c("Have Never Posted Political\nContent Online",
                             "Have Posted Political Content/nOnline",
                             "No Response")) +
  theme(axis.text.y=element_text(angle=45,size=8)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("Moderate","Liberal","Very Liberal","No Response")) +
  ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Moderate to Liberal Social Media Users",
          subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")
