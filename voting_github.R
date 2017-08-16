#######################
# Analysis APHA voting patterns
# Citation: Purtle J, Goldstein ND, Hand A, Edson E. Who votes for public health? U.S. senator characteristics associated with voting in concordance with public health policy recommendations (1998–2013). SSM-Population Health. 2017 Jan 3:136–140. 
# 8/15/2014 -- Neal Goldstein
#######################


### FUNCTIONS ###

library(psych) #describe, describeBy
library(gmodels) #CrossTable
#library(lme4) #mixed effects models
library(nlme) #mixed effects models (using this to choose covariance matrix)
library(gee) #compare the results using gee


### READ DATA ###

Voting = read.csv("JP Merged Voting Dataset 9.1.15 NG.csv", as.is=T, stringsAsFactors=F, na.strings="")


### RESHAPE to WIDE ###

#recast voting patterns
Voting$Average_APHA_Acordance = as.numeric(sub("%","",Voting$Average_APHA_Acordance))

#transform to wide
Voting_wide = reshape(Voting, v.names=c("Party_Affiliation","Average_APHA_Acordance"), timevar="Year", idvar="ID", direction="wide")


### RECODE ###

Voting_wide$Region = ifelse(Voting_wide$Northeast==1, "Northeast", ifelse(Voting_wide$South==1, "South", ifelse(Voting_wide$Midwest==1, "Midwest", ifelse(Voting_wide$West==1, "West", NA))))
Voting_wide$Gender = "M"

#manually set females
Voting_wide$Gender[Voting_wide$ID=="Ayotte-NH"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Boxer-CA"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Cantwell-WA"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Carnahan-MO"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Clinton-NY"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Collins-ME"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Dole-NC"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Feinstein-CA"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Gillibrand-NY"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Hagan-NC"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Hutchison-TX"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Klobuchar-MN"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Landrieu-LA"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Lincoln-AR"] = "F"
Voting_wide$Gender[Voting_wide$ID=="McCaskill-MO"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Mikulski-MD"] = "F"
Voting_wide$Gender[Voting_wide$ID=="MoseleyOBraun-IL"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Murkowski-AK"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Murray-WA"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Sessions-AL"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Shaheen-NH"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Snowe-ME"] = "F"
Voting_wide$Gender[Voting_wide$ID=="Stabenow-MI"] = "F"

#mean voting patterns
Voting_wide$Average_APHA_Acordance = NA

#changed party affiliations
Voting_wide$Party_Affiliation_Changed = NA

#create a party UI
Voting_wide$Party = NA
Voting_wide$ID_Party = NA

#records to drop
drop_list=NA

for (i in 1:nrow(Voting_wide))
{
  #mean voting patterns
  Voting_wide$Average_APHA_Acordance[i] = mean(c(Voting_wide$Average_APHA_Acordance.1998[i], Voting_wide$Average_APHA_Acordance.1999[i], Voting_wide$Average_APHA_Acordance.2000[i], Voting_wide$Average_APHA_Acordance.2001[i], Voting_wide$Average_APHA_Acordance.2002[i], Voting_wide$Average_APHA_Acordance.2003[i], Voting_wide$Average_APHA_Acordance.2004[i], Voting_wide$Average_APHA_Acordance.2005[i], Voting_wide$Average_APHA_Acordance.2006[i], Voting_wide$Average_APHA_Acordance.2007[i], Voting_wide$Average_APHA_Acordance.2008[i], Voting_wide$Average_APHA_Acordance.2009[i], Voting_wide$Average_APHA_Acordance.2010[i], Voting_wide$Average_APHA_Acordance.2011[i], Voting_wide$Average_APHA_Acordance.2012[i], Voting_wide$Average_APHA_Acordance.2013[i]), na.rm=T)
  
  
  #determine if affiliation changed
  Voting_wide$Party_Affiliation_Changed[i] = ifelse(length(unique(c(na.omit(Voting_wide$Party_Affiliation.1998[i]), na.omit(Voting_wide$Party_Affiliation.1999[i]), na.omit(Voting_wide$Party_Affiliation.2000[i]), na.omit(Voting_wide$Party_Affiliation.2001[i]), na.omit(Voting_wide$Party_Affiliation.2002[i]), na.omit(Voting_wide$Party_Affiliation.2003[i]), na.omit(Voting_wide$Party_Affiliation.2004[i]), na.omit(Voting_wide$Party_Affiliation.2005[i]), na.omit(Voting_wide$Party_Affiliation.2006[i]), na.omit(Voting_wide$Party_Affiliation.2007[i]), na.omit(Voting_wide$Party_Affiliation.2008[i]), na.omit(Voting_wide$Party_Affiliation.2009[i]), na.omit(Voting_wide$Party_Affiliation.2010[i]), na.omit(Voting_wide$Party_Affiliation.2011[i]), na.omit(Voting_wide$Party_Affiliation.2012[i]), na.omit(Voting_wide$Party_Affiliation.2013[i])))) > 1, 1, 0)

  if (Voting_wide$Party_Affiliation_Changed[i] == 0) {
    
    #create a party unique identifier
    Voting_wide$Party[i] = unique(c(na.omit(Voting_wide$Party_Affiliation.1998[i]), na.omit(Voting_wide$Party_Affiliation.1999[i]), na.omit(Voting_wide$Party_Affiliation.2000[i]), na.omit(Voting_wide$Party_Affiliation.2001[i]), na.omit(Voting_wide$Party_Affiliation.2002[i]), na.omit(Voting_wide$Party_Affiliation.2003[i]), na.omit(Voting_wide$Party_Affiliation.2004[i]), na.omit(Voting_wide$Party_Affiliation.2005[i]), na.omit(Voting_wide$Party_Affiliation.2006[i]), na.omit(Voting_wide$Party_Affiliation.2007[i]), na.omit(Voting_wide$Party_Affiliation.2008[i]), na.omit(Voting_wide$Party_Affiliation.2009[i]), na.omit(Voting_wide$Party_Affiliation.2010[i]), na.omit(Voting_wide$Party_Affiliation.2011[i]), na.omit(Voting_wide$Party_Affiliation.2012[i]), na.omit(Voting_wide$Party_Affiliation.2013[i])))
    Voting_wide$ID_Party[i] = paste(Voting_wide$ID[i], Voting_wide$Party[i], sep="-")
    
  } else {
    
    #create separate records for time in each party
    if (length(unique(c(na.omit(Voting_wide$Party_Affiliation.1998[i]), na.omit(Voting_wide$Party_Affiliation.1999[i]), na.omit(Voting_wide$Party_Affiliation.2000[i]), na.omit(Voting_wide$Party_Affiliation.2001[i]), na.omit(Voting_wide$Party_Affiliation.2002[i]), na.omit(Voting_wide$Party_Affiliation.2003[i]), na.omit(Voting_wide$Party_Affiliation.2004[i]), na.omit(Voting_wide$Party_Affiliation.2005[i]), na.omit(Voting_wide$Party_Affiliation.2006[i]), na.omit(Voting_wide$Party_Affiliation.2007[i]), na.omit(Voting_wide$Party_Affiliation.2008[i]), na.omit(Voting_wide$Party_Affiliation.2009[i]), na.omit(Voting_wide$Party_Affiliation.2010[i]), na.omit(Voting_wide$Party_Affiliation.2011[i]), na.omit(Voting_wide$Party_Affiliation.2012[i]), na.omit(Voting_wide$Party_Affiliation.2013[i])))) == 2) {
      
      #create two new records
      records = Voting_wide[i,]
      records = rbind(records, Voting_wide[i,])
      
      #find first affiliation
      year_col=8
      affil_init = records[1,year_col]
      while (is.na(affil_init))
      {
        year_col = year_col + 2
        affil_init =records[1,year_col]
      }
      rm(year_col)
      
      #strike records where party differed
      for (year_col in seq(8,38,by=2))
      {
        #get current affil
        affil_cur = records[1,year_col]
        
        #ensure there is an affiliation this year
        if (!is.na(affil_cur))
        {
          if (affil_init == affil_cur) {
            
            #party has not changed, strike from second record
            records[2,year_col] = NA
            records[2,year_col+1] = NA
            
          } else {
            
            #party has changed, strike from first record
            records[1,year_col] = NA
            records[1,year_col+1] = NA
            
          }
        }
      }
      rm(affil_cur, affil_init, year_col)
      
      #create a party unique identifier
      records$Party[1] = unique(c(na.omit(records$Party_Affiliation.1998[1]), na.omit(records$Party_Affiliation.1999[1]), na.omit(records$Party_Affiliation.2000[1]), na.omit(records$Party_Affiliation.2001[1]), na.omit(records$Party_Affiliation.2002[1]), na.omit(records$Party_Affiliation.2003[1]), na.omit(records$Party_Affiliation.2004[1]), na.omit(records$Party_Affiliation.2005[1]), na.omit(records$Party_Affiliation.2006[1]), na.omit(records$Party_Affiliation.2007[1]), na.omit(records$Party_Affiliation.2008[1]), na.omit(records$Party_Affiliation.2009[1]), na.omit(records$Party_Affiliation.2010[1]), na.omit(records$Party_Affiliation.2011[1]), na.omit(records$Party_Affiliation.2012[1]), na.omit(records$Party_Affiliation.2013[1])))
      records$ID_Party[1] = paste(records$ID[1], records$Party[1], sep="-")
      records$Party[2] = unique(c(na.omit(records$Party_Affiliation.1998[2]), na.omit(records$Party_Affiliation.1999[2]), na.omit(records$Party_Affiliation.2000[2]), na.omit(records$Party_Affiliation.2001[2]), na.omit(records$Party_Affiliation.2002[2]), na.omit(records$Party_Affiliation.2003[2]), na.omit(records$Party_Affiliation.2004[2]), na.omit(records$Party_Affiliation.2005[2]), na.omit(records$Party_Affiliation.2006[2]), na.omit(records$Party_Affiliation.2007[2]), na.omit(records$Party_Affiliation.2008[2]), na.omit(records$Party_Affiliation.2009[2]), na.omit(records$Party_Affiliation.2010[2]), na.omit(records$Party_Affiliation.2011[2]), na.omit(records$Party_Affiliation.2012[2]), na.omit(records$Party_Affiliation.2013[2])))
      records$ID_Party[2] = paste(records$ID[2], records$Party[2], sep="-")
      
      #add these records
      Voting_wide = rbind(Voting_wide, records)
      
      #track records to drop
      drop_list = c(drop_list, i*-1)
      
    } else {
      
      #can't handle 3 parties right now
      stop()
    }
    
  }
}
Voting_wide = Voting_wide[na.omit(drop_list),]
rm(i,drop_list,records)


### RESHAPE to LONG ###

Voting_long=data.frame()
for (i in 1:nrow(Voting_wide))
{
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.1998[i],year=0,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.1999[i],year=1,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2000[i],year=2,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2001[i],year=3,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2002[i],year=4,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2003[i],year=5,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2004[i],year=6,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2005[i],year=7,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2006[i],year=8,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2007[i],year=9,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2008[i],year=10,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2009[i],year=11,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2010[i],year=12,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2011[i],year=13,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2012[i],year=14,party=Voting_wide$Party[i]))
  Voting_long = rbind(Voting_long, data.frame(id=Voting_wide$ID[i],gender=Voting_wide$Gender[i],region=Voting_wide$Region[i],state=Voting_wide$State[i],voting=Voting_wide$Average_APHA_Acordance.2013[i],year=15,party=Voting_wide$Party[i]))
}
Voting_long=na.omit(Voting_long)
rm(i)

#mean center year
Voting_long$year_centered = scale(Voting_long$year, scale=F)

#exports for appendix
write.csv(Voting_wide, file="Voting_wide.csv", na="", row.names=F)
write.csv(Voting_long, file="Voting_long.csv", na="", row.names=F)


### COHORT DESCRIPTIVES ###

##by congressperson
length(unique(Voting_wide$ID))
CrossTable(Voting_wide$Party)
CrossTable(Voting_wide$Region)

##by voting year

#overall
mean(Voting_long$voting)
sd(Voting_long$voting)

#by party, all years
describeBy(Voting_long$voting, Voting_long$party)

#by party, per year
describeBy(Voting_long$voting[Voting_long$year==0], Voting_long$party[Voting_long$year==0])
describeBy(Voting_long$voting[Voting_long$year==1], Voting_long$party[Voting_long$year==1])
describeBy(Voting_long$voting[Voting_long$year==2], Voting_long$party[Voting_long$year==2])
describeBy(Voting_long$voting[Voting_long$year==3], Voting_long$party[Voting_long$year==3])
describeBy(Voting_long$voting[Voting_long$year==4], Voting_long$party[Voting_long$year==4])
describeBy(Voting_long$voting[Voting_long$year==5], Voting_long$party[Voting_long$year==5])
describeBy(Voting_long$voting[Voting_long$year==6], Voting_long$party[Voting_long$year==6])
describeBy(Voting_long$voting[Voting_long$year==7], Voting_long$party[Voting_long$year==7])
describeBy(Voting_long$voting[Voting_long$year==8], Voting_long$party[Voting_long$year==8])
describeBy(Voting_long$voting[Voting_long$year==9], Voting_long$party[Voting_long$year==9])
describeBy(Voting_long$voting[Voting_long$year==10], Voting_long$party[Voting_long$year==10])
describeBy(Voting_long$voting[Voting_long$year==11], Voting_long$party[Voting_long$year==11])
describeBy(Voting_long$voting[Voting_long$year==12], Voting_long$party[Voting_long$year==12])
describeBy(Voting_long$voting[Voting_long$year==13], Voting_long$party[Voting_long$year==13])
describeBy(Voting_long$voting[Voting_long$year==14], Voting_long$party[Voting_long$year==14])
describeBy(Voting_long$voting[Voting_long$year==15], Voting_long$party[Voting_long$year==15])

#by gender, all years
describeBy(Voting_long$voting, Voting_long$gender)

#by gender, per year
describeBy(Voting_long$voting[Voting_long$year==0], Voting_long$gender[Voting_long$year==0])
describeBy(Voting_long$voting[Voting_long$year==1], Voting_long$gender[Voting_long$year==1])
describeBy(Voting_long$voting[Voting_long$year==2], Voting_long$gender[Voting_long$year==2])
describeBy(Voting_long$voting[Voting_long$year==3], Voting_long$gender[Voting_long$year==3])
describeBy(Voting_long$voting[Voting_long$year==4], Voting_long$gender[Voting_long$year==4])
describeBy(Voting_long$voting[Voting_long$year==5], Voting_long$gender[Voting_long$year==5])
describeBy(Voting_long$voting[Voting_long$year==6], Voting_long$gender[Voting_long$year==6])
describeBy(Voting_long$voting[Voting_long$year==7], Voting_long$gender[Voting_long$year==7])
describeBy(Voting_long$voting[Voting_long$year==8], Voting_long$gender[Voting_long$year==8])
describeBy(Voting_long$voting[Voting_long$year==9], Voting_long$gender[Voting_long$year==9])
describeBy(Voting_long$voting[Voting_long$year==10], Voting_long$gender[Voting_long$year==10])
describeBy(Voting_long$voting[Voting_long$year==11], Voting_long$gender[Voting_long$year==11])
describeBy(Voting_long$voting[Voting_long$year==12], Voting_long$gender[Voting_long$year==12])
describeBy(Voting_long$voting[Voting_long$year==13], Voting_long$gender[Voting_long$year==13])
describeBy(Voting_long$voting[Voting_long$year==14], Voting_long$gender[Voting_long$year==14])
describeBy(Voting_long$voting[Voting_long$year==15], Voting_long$gender[Voting_long$year==15])

#by region, all years
describeBy(Voting_long$voting, Voting_long$region)

#by region, per year
describeBy(Voting_long$voting[Voting_long$year==0], Voting_long$region[Voting_long$year==0])
describeBy(Voting_long$voting[Voting_long$year==1], Voting_long$region[Voting_long$year==1])
describeBy(Voting_long$voting[Voting_long$year==2], Voting_long$region[Voting_long$year==2])
describeBy(Voting_long$voting[Voting_long$year==3], Voting_long$region[Voting_long$year==3])
describeBy(Voting_long$voting[Voting_long$year==4], Voting_long$region[Voting_long$year==4])
describeBy(Voting_long$voting[Voting_long$year==5], Voting_long$region[Voting_long$year==5])
describeBy(Voting_long$voting[Voting_long$year==6], Voting_long$region[Voting_long$year==6])
describeBy(Voting_long$voting[Voting_long$year==7], Voting_long$region[Voting_long$year==7])
describeBy(Voting_long$voting[Voting_long$year==8], Voting_long$region[Voting_long$year==8])
describeBy(Voting_long$voting[Voting_long$year==9], Voting_long$region[Voting_long$year==9])
describeBy(Voting_long$voting[Voting_long$year==10], Voting_long$region[Voting_long$year==10])
describeBy(Voting_long$voting[Voting_long$year==11], Voting_long$region[Voting_long$year==11])
describeBy(Voting_long$voting[Voting_long$year==12], Voting_long$region[Voting_long$year==12])
describeBy(Voting_long$voting[Voting_long$year==13], Voting_long$region[Voting_long$year==13])
describeBy(Voting_long$voting[Voting_long$year==14], Voting_long$region[Voting_long$year==14])
describeBy(Voting_long$voting[Voting_long$year==15], Voting_long$region[Voting_long$year==15])


### ANALYSIS: LONGITUDINAL PLOT ###

#check distribution / use median since not normal
hist(Voting_long$voting,breaks="fd")

#4 up plot
par(mfrow=c(2,2))

#base plot
plot(y=c(0,100),x=c(0,15),type="n",xaxt="n",xlab="",ylab="")
title(xlab="Year",ylab="% agreement with APHA",line=2)
axis(1,at=seq(0,15,by=2),labels=seq(1998,2013,by=2))
axis(4)

#add median trends
#lines(y=c(median(Voting_long$voting[Voting_long$year==0 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==1 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==2 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==3 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==4 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==5 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==6 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==7 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==8 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==9 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==10 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==11 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==12 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==13 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==14 & Voting_long$party=="D"]),median(Voting_long$voting[Voting_long$year==15 & Voting_long$party=="D"])),x=seq(0,15),lwd=2,lty=1)
#lines(y=c(median(Voting_long$voting[Voting_long$year==0 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==1 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==2 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==3 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==4 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==5 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==6 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==7 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==8 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==9 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==10 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==11 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==12 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==13 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==14 & Voting_long$party=="R"]),median(Voting_long$voting[Voting_long$year==15 & Voting_long$party=="R"])),x=seq(0,15),lwd=2,lty=2)
#lines(y=c(median(Voting_long$voting[Voting_long$year==0 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==1 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==2 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==3 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==4 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==5 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==6 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==7 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==8 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==9 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==10 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==11 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==12 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==13 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==14 & Voting_long$party=="I"]),median(Voting_long$voting[Voting_long$year==15 & Voting_long$party=="I"])),x=seq(0,15),lwd=2,lty=3)

#mean party trends
lines(y=c(mean(Voting_long$voting[Voting_long$year==0 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==1 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==2 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==3 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==4 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==5 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==6 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==7 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==8 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==9 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==10 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==11 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==12 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==13 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==14 & Voting_long$party=="D"]),mean(Voting_long$voting[Voting_long$year==15 & Voting_long$party=="D"])),x=seq(0,15),lwd=2,lty=1)
lines(y=c(mean(Voting_long$voting[Voting_long$year==0 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==1 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==2 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==3 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==4 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==5 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==6 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==7 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==8 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==9 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==10 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==11 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==12 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==13 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==14 & Voting_long$party=="R"]),mean(Voting_long$voting[Voting_long$year==15 & Voting_long$party=="R"])),x=seq(0,15),lwd=2,lty=2)
lines(y=c(mean(Voting_long$voting[Voting_long$year==0 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==1 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==2 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==3 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==4 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==5 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==6 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==7 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==8 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==9 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==10 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==11 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==12 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==13 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==14 & Voting_long$party=="I"]),mean(Voting_long$voting[Voting_long$year==15 & Voting_long$party=="I"])),x=seq(0,15),lwd=2,lty=3)

#add legend
legend(0.5,130,lty=c(1,2,3),c("Democrat","Republican","Independent"), horiz=T, xpd=T, cex=0.8, x.intersp=0.2, y.intersp=0, text.width = 2)

#base plot
plot(y=c(0,100),x=c(0,15),type="n",xaxt="n",xlab="",ylab="")
title(xlab="Year",ylab="% agreement with APHA",line=2)
axis(1,at=seq(0,15,by=2),labels=seq(1998,2013,by=2))
axis(4)

#mean gender trends
lines(y=c(mean(Voting_long$voting[Voting_long$year==0 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==1 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==2 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==3 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==4 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==5 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==6 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==7 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==8 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==9 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==10 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==11 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==12 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==13 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==14 & Voting_long$gender=="M"]),mean(Voting_long$voting[Voting_long$year==15 & Voting_long$gender=="M"])),x=seq(0,15),lwd=2,lty=1)
lines(y=c(mean(Voting_long$voting[Voting_long$year==0 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==1 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==2 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==3 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==4 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==5 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==6 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==7 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==8 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==9 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==10 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==11 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==12 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==13 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==14 & Voting_long$gender=="F"]),mean(Voting_long$voting[Voting_long$year==15 & Voting_long$gender=="F"])),x=seq(0,15),lwd=2,lty=2)

#add legend
legend(0.5,130,lty=c(1,2),c("Male","Female"), horiz=T, xpd=T, cex=0.8, x.intersp=0.2, y.intersp=0, text.width = 2)

#base plot
plot(y=c(0,100),x=c(0,15),type="n",xaxt="n",xlab="",ylab="")
title(xlab="Year",ylab="% agreement with APHA",line=2)
axis(1,at=seq(0,15,by=2),labels=seq(1998,2013,by=2))
axis(4)

#mean region trends
lines(y=c(mean(Voting_long$voting[Voting_long$year==0 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==1 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==2 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==3 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==4 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==5 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==6 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==7 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==8 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==9 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==10 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==11 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==12 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==13 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==14 & Voting_long$region=="Northeast"]),mean(Voting_long$voting[Voting_long$year==15 & Voting_long$region=="Northeast"])),x=seq(0,15),lwd=2,lty=1)
lines(y=c(mean(Voting_long$voting[Voting_long$year==0 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==1 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==2 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==3 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==4 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==5 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==6 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==7 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==8 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==9 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==10 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==11 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==12 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==13 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==14 & Voting_long$region=="South"]),mean(Voting_long$voting[Voting_long$year==15 & Voting_long$region=="South"])),x=seq(0,15),lwd=2,lty=2)
lines(y=c(mean(Voting_long$voting[Voting_long$year==0 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==1 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==2 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==3 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==4 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==5 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==6 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==7 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==8 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==9 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==10 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==11 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==12 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==13 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==14 & Voting_long$region=="Midwest"]),mean(Voting_long$voting[Voting_long$year==15 & Voting_long$region=="Midwest"])),x=seq(0,15),lwd=2,lty=3)
lines(y=c(mean(Voting_long$voting[Voting_long$year==0 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==1 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==2 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==3 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==4 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==5 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==6 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==7 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==8 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==9 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==10 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==11 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==12 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==13 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==14 & Voting_long$region=="West"]),mean(Voting_long$voting[Voting_long$year==15 & Voting_long$region=="West"])),x=seq(0,15),lwd=2,lty=4)

#add legend
legend(0.5,130,lty=c(1,2,3,4),c("Northeast","South","Midwest","West"), horiz=T, xpd=T, cex=0.8, x.intersp=0.2, y.intersp=0, text.width = 2)


### ANALYSIS: LONGITUDINAL REGRESSION ###

#summary(lmer(voting ~ (1 | id) + year_centered*as.factor(party), data=Voting_long, REML=F))

#need to take into account a 3-level model for spatial correlation: multiple voting years occur per senators who are clustered in states

#choose covariance structure (lowest AIC)
summary(lme(voting ~ year_centered*as.factor(party), random=~1|state/id,data=Voting_long,method="REML")) #unstructured
#summary(lme(voting ~ year_centered*as.factor(party), random=~1|state/id,correlation=corSymm(),data=Voting_long,method="REML")) #symmetric
summary(lme(voting ~ year_centered*as.factor(party), random=~1|state/id,correlation=corAR1(),data=Voting_long,method="REML")) #autoregressive
summary(lme(voting ~ year_centered*as.factor(party), random=~1|state/id,correlation=corCompSymm(),data=Voting_long,method="REML")) #compound symmetry

#add potential confounders
summary(lme(voting ~ year*as.factor(party)+as.factor(gender)+relevel(region, ref="South"), random=~1|state/id,correlation=corAR1(),data=Voting_long,method="ML"))

#relevel for republican interpretation in interaction
#party_relevel = relevel(as.factor(Voting_long$party), ref="D")
#summary(lme(voting ~ year_centered*as.factor(party_relevel)+as.factor(gender)+relevel(region, ref="South"), random=~1|id,correlation=corAR1(),data=Voting_long,method="ML"))

#check for interaction
modelInteraction = lme(voting ~ year_centered*as.factor(party)+as.factor(gender)+relevel(region, ref="South"), random=~1|state/id,correlation=corAR1(),data=Voting_long,method="ML")
modelNoInteraction = lme(voting ~ year_centered+as.factor(party)+as.factor(gender)+relevel(region, ref="South"), random=~1|state/id,correlation=corAR1(),data=Voting_long,method="ML")
anova(modelInteraction,modelNoInteraction)

#summary for paper
summary(modelInteraction)
intervals(modelInteraction)

#check for normal residuals (since the outcome is skewed); see https://www.researchgate.net/post/Is_linear_regression_valid_when_the_outcome_dependant_variable_not_normally_distributed
hist(residuals(modelInteraction,type="pearson"),breaks="fd")

#comparison between mixed effects and gee
summary(lme(voting ~ year_centered*as.factor(party)+as.factor(gender)+relevel(region, ref="South"), random=~1|id,correlation=corAR1(),data=Voting_long,method="ML"))
summary(gee(voting ~ year_centered*as.factor(party)+as.factor(gender)+relevel(region, ref="South"), id=id, corstr="independence", data=Voting_long))

