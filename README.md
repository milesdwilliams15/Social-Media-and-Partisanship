# Social Media and Partisanship
Analysis of the relationship between online political engagement on partisan behavior.

[Back to Main Page](https://milesdwilliams15.github.io/)

Back in October of this year (2016) I had the chance to present some of my research at the 87th Annual Meeting of the Indiana Academy of the Social sciences. 
You can find me in the IASS's event program on page 9, under Political Science Panel 2 (just so you don't think I'm blowing smoke): 
[IASS Program, 2016.](https://drive.google.com/file/d/0B0WaPggKV2mkRVE0MVBsUDhmQkE/view)

My research focused on the question of whether social media has contributed to the hardening of partisan and ideological divisions.
In this post, I'll expand on some of my analysis by displaying some very simple -- yet straightforward -- analysis of the data I used for my conference presentation.
While I used methods such as logit with a quasibinomial dispersion parameter and ordinal logistic regression in my original research, I'll keep things in this post simple, at least in terms of analysis.

The code I used for the visualizations in this post, however, are slightly more complicated, but not overly so.

## Let's Break It down by Ideology
I'll start by first breaking things down by ideology for the entire dataset. Later, I'll create a new dataset of only social media users.

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

![ideological identification of all survey respondents](https://cloud.githubusercontent.com/assets/23504082/21416365/e4dc9634-c7d7-11e6-8073-320c485363fd.jpeg)

It's pretty clear that the majority of respondents identify as politically moderate (on a conservative-liberal scale). The survey does seem to lean slightly conservative, but that shouldn't be a major issue going forward.

    length(social.SNS$How.liberal.factor) # 925 total observations in the "social.SNS" dataset.

    # Install and open the "dpylr" package so we can filter the data to make a new data frame
    # where respondents who didn't report using social media are eliminated:
    install.packages("dplyr")
    library(dplyr)

    social.SNS <- filter(social, Use.SNS == 1) # Make new dataset.

    # Bar graph: total # of respondents who use social media by ideological identification.
    ggplot(social.SNS,aes(How.liberal.factor)) +
      geom_bar() + theme_classic() +
      xlab("Number of Observations = 925") + ylab("") + 
      ggtitle("Ideological Identification of Social Media Users",
              subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

![ideological identification of social media users](https://cloud.githubusercontent.com/assets/23504082/21416412/585989c8-c7d8-11e6-8f86-0f1a19d00ad2.jpeg)

Among those who reported using social media, the distribtuion of ideological self-identification seems fairly consistent with the larger dataset.

## How Does Greater Conservative and Liberal Ideology Impact Online Political Behavior?

### Sharing Political Content Online

    # Make a new data frame with stacked group percentages so that we can graph percentages rather
    # than count data:
    social.SNS1<-data.frame(prop.table(table(social.SNS$Ideological.intensity.factor,
                                         social.SNS$Share.factor),1))

    # Create bar graph
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

![effect of ideology 1](https://cloud.githubusercontent.com/assets/23504082/21417667/d665e0ce-c7e1-11e6-92ad-a10fda69a1ee.jpeg)

Interesting. 
The more liberal or more conservative an individual identifies, the more likely she is to share political content online. 
Of course, the share of those who identify as either very liberal or very conservative who report sharing political content is not massive by any stretch of the imagination. 
Nevertheless a notable pattern is apparent.

Does this pattern remain if I separate moderate to conservative from moderate to liberal respondents?

First, I'll make a dataset composed of moderate to conservative social media users (including non-respondents) and I'll make another dataset composed of moderate to liberal respondents (including non-respondents).

    social.SNScon <- filter(social.SNS, How.liberal.factor=="1. Very Conservative"|How.liberal.factor=="2. Conservative"|How.liberal.factor=="3. Moderate"|How.liberal.factor=="No Response")
    length(social.SNScon$Rvote) # 744
    social.SNSlib <- filter(social.SNS, How.liberal.factor=="3. Moderate"|How.liberal.factor=="4. Liberal"|How.liberal.factor=="5. Very Liberal"|How.liberal.factor=="No Response")
    length(social.SNSlib$Rvote) # 581

Then, I'll make additional data frames of stacked group percentages (one for the conservative and one for the liberal dataset):

    social.SNScon1<-data.frame(prop.table(table(social.SNScon$Ideological.intensity.factor,
                                                social.SNScon$Share.factor),1))
    social.SNSlib1<-data.frame(prop.table(table(social.SNSlib$Ideological.intensity.factor,
                                                social.SNSlib$Share.factor),1))                                            

And then, I'll graph the data.

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

![con 1](https://cloud.githubusercontent.com/assets/23504082/21417560/12861714-c7e1-11e6-8e43-12aeca9b35f2.jpeg)

![lib 1](https://cloud.githubusercontent.com/assets/23504082/21417569/203baafe-c7e1-11e6-96f9-e126ed3750c2.jpeg)

Further breaking down the dataset reveals fairly consistent results. 
Those who identify as ver liberal or very conservative are more likely to share political content online.

### What about Other Measures of Online Political Activity?
Who is more likely to engage in online discussions about politics?

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
                       labels=c("Have Never Shared Political Content",
                                 "Have Shared Political Content",
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
                        labels=c("Have Never Shared Political Content",
                                 "Have Shared Political Content",
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
                        labels=c("Have Never Shared Political Content",
                                 "Have Shared Political Content",
                                 "No Response")) +
      theme(axis.text.y=element_text(angle=45,size=8)) +
      scale_y_continuous(labels=scales::percent) +
      scale_x_discrete(labels=c("Moderate","Liberal","Very Liberal","No Response")) +
      ggtitle("Effect of Ideological Intensity on Online Political Engagement\namong Moderate to Liberal Social Media Users",
              subtitle = "Data: Pew Research Center, November 2010\nAnalysis by Miles D. Williams (https://milesdwilliams15.github.io/)")

![effect of ideology 2](https://cloud.githubusercontent.com/assets/23504082/21418119/a1e09bd4-c7e4-11e6-864e-72688704cd79.jpeg)

![con 2](https://cloud.githubusercontent.com/assets/23504082/21418140/ad16f976-c7e4-11e6-84b6-ae316dcfa178.jpeg)

![lib 2](https://cloud.githubusercontent.com/assets/23504082/21433506/ac7c60a4-c835-11e6-95e8-bc2259ebb868.jpeg)

Who is more likely to be a friend of an online political group or candidate?

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

![effect of ideology 3](https://cloud.githubusercontent.com/assets/23504082/21418208/27592538-c7e5-11e6-947a-5c6a1f577981.jpeg)

![con 3](https://cloud.githubusercontent.com/assets/23504082/21418210/2cab02b8-c7e5-11e6-982d-cdcabdf15c46.jpeg)

![lib 3](https://cloud.githubusercontent.com/assets/23504082/21418212/3270243a-c7e5-11e6-8d69-2d732d305117.jpeg)

Who is more likely to join an online political group?

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

![effect of ideology 4](https://cloud.githubusercontent.com/assets/23504082/21418254/81c7b476-c7e5-11e6-9b67-118d4401e9c5.jpeg)

![con 4](https://cloud.githubusercontent.com/assets/23504082/21418257/865ac230-c7e5-11e6-9884-dc89f33f415c.jpeg)

![lib 4](https://cloud.githubusercontent.com/assets/23504082/21418259/8f062b36-c7e5-11e6-88eb-af0048569b13.jpeg)

Who is more likely to post political content?

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

![effect of ideology 5](https://cloud.githubusercontent.com/assets/23504082/21418304/e1dce9e4-c7e5-11e6-816a-448d72847f1c.jpeg)

![con 5](https://cloud.githubusercontent.com/assets/23504082/21418312/e72ea70c-c7e5-11e6-9281-403f637bfd8b.jpeg)

![lib 5](https://cloud.githubusercontent.com/assets/23504082/21434163/9f3b6a5e-c838-11e6-9b5c-18da107ad05b.jpeg)

## Conclusion
Aside from a couple of exceptions (i.e., the reported activity of liberal respondents when it comes to being a friend of an online political group and joining an online political group), the general pattern suggests that, among self-reported social media users, the more conservative or liberal an individual identifies, the more likely said person is to engage in online political activity.
Given the hyperpartisan atmosphere that currently characterizes politics in the U.S., I doubt matters are helped much by the fact that those with the most extreme political views are most politically active on social media.

[Back to Main Page](https://milesdwilliams15.github.io/)
