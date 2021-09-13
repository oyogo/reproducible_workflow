
mydata <- airquality

write.csv(mydata,"../data/raw_data.csv")

names(mydata)[2] <- "solar_R"

str(mydata)

myreg <- lm(Ozone ~ Wind + Temp, mydata)
confint(myreg)

ss <- readr::read_csv("data/secondary_schools_stabstract.csv")
names(ss)
