
#########################################################################
#                                                                       #
#                   Introduction to the R environment                   #
#                                                                       #
#########################################################################



# There are lots of GUIs for R.  Bluesky, jamovi, RKWard, etc.  However, these are mostly 
# graphical interfaces like SPSS.  For more integrated development environments (IDE), look
# at R Studio, jupyter notebooks, and Visual Studio/ VS code.



# Understanding the R Studio Environment ----------------------------------

      # Scripts vs console

      # base R and installing/loading packages
        # CRAN and github

      # R projects (working directory)

      # Functions


# R object types ----------------------------------------------------------

str()    # structure of an object
# vector
# matrix
# array
# list
# data frame


# R classes ---------------------------------------------------------------

class()  # class or type of an object
# character
# numeric 
# integer
# complex
# logical (True/False)


# R built-in data ---------------------------------------------------------

# R comes with a lot of data built in
# Some packages will also come with their own built-in data for examples

data()


#########################################################################
#                                                                       #
#                          Data and Packages                            #
#                                                                       #
#########################################################################



# Loading data into memory ------------------------------------------------

# A note about reading and writing data...
  # R never alters the original data, unlike SPSS and other programs.
  # Everything is held in R's working memory until you tell it to write.

# A few rules...
  # R is case sensitive
  # R does not like spaces in variable names
  # R does not like variables to start with numbers

read.csv()
read.table()
read_excel()


# Load the iris data ------------------------------------------------------

# beacaue this is local data, we don't need to specify a filepath
iris <- iris

# if it were external data, we'd use a function and filepath
  # read.csv(file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", ...)
  # iris <- read.csv("C:/Users/michael_peabody/Desktop/R Workshop/iris.csv")



# Installing and loading packages -----------------------------------------


# Only install packages once (occasional updates)
  # There are tidyverse, data.table, and base solutions to nearly every problem.
  # About the tidyverse (https://www.tidyverse.org/)

install.packages("tidyverse") 

# Loading packages 
library(plyr)
library(reshape2)
library(ggplot2)

# Packages sometime conflict with each other,so you may want to detach one.
detach("package:ggplot2", unload = T)


# Seeing inside a package/function
  # The "::" calls the package without loading it into the workspace
psych::fisherz



#########################################################################
#                                                                       #
#                          Data Manipulation                            #
#                                                                       #
#########################################################################



# Subsetting --------------------------------------------------------------

# Select only one species
setosa.only<-subset(iris, Species=="setosa")
View(setosa.only)

# Select certain columns
width.only<-subset(iris, select = c(Sepal.Width, Petal.Width))
View(width.only)

# Select on criteria
  # The "&" means "and"...the "|" means "or.
long.length <-subset(iris, Sepal.Length > 6 | Petal.Length > 6)
View(long.length)

# Only certain columns that meet some criteria
long.length.species<- subset(iris, Sepal.Length > 6, select = c(Species))
View(long.length.species)


# Rename variables --------------------------------------------------------

# The base R solution
names(iris)[names(iris)=="Species"] <- "type"
View(iris)

# Using the plyr package
  # rename(df, old_name = new_name)
library(plyr)  # we've already loaded the package, so we really don't need to do it again.
iris<-rename(iris, c("Sepal.Length"="sep.len", "Sepal.Width" = "sep.wid"))
View(iris)

# Using dplyr
  # rename(df, new_name = old_name)
library(dplyr)
iris <- rename(iris, pet.len = Petal.Length, pet.wid = Petal.Width) #For renaming dataframe column
View(iris)

# Recode values -----------------------------------------------------------

# plyr does this for all variable at once.
iris$type <- plyr::revalue(iris$type, c("setosa"="SET", "versicolor"="VER", "virginica" = "VIR"))


# Create new variables
  # calculations
iris$sepals <- iris$sep.len * iris$sep.wid
View(iris)

  # Pasting text together
iris$sepal.text <- paste(iris$type, iris$sepals, sep = "_")
View(iris)

  # Logic from other variables
iris$long.length[iris$sep.len > 6 | iris$pet.len > 6 ] <- "long"
View(iris)
iris$short.length <- ifelse(iris$sep.len <= 1 | iris$pet.len <= 1.5, "short", "standard")
View(iris)


# Converting to Factors
iris$id.code<-seq(1:nrow(iris))
View(iris)
iris$id.code<-as.factor(iris$id.code)
View(iris)


# Merging data ------------------------------------------------------------


# cbind and rbind
set.seed(123)
normal<-rnorm(1:nrow(iris), mean = 0, sd = 1)
normal<-as.data.frame(normal)
iris<-cbind(iris, normal)

iris.first.half<- iris[1:75,]
iris.second.half <- iris[76:150,]
back.together <- rbind(iris.first.half, iris.second.half)

# base R merge vs plyr::join
id.code <- seq(1:nrow(iris))
some.variable <- rnorm(1:nrow(iris), mean = 0, sd = 1)
new.df <- data.frame(id.code, some.variable)

iris.merge <- merge(iris, new.df, by="id.code", all=TRUE)
iris.join <- plyr::join(iris, new.df, by="id.code", type="full")


#########################################################################
#                                                                       #
#                             Analysis                                  #
#                                                                       #
#########################################################################



# Summary Statistics ------------------------------------------------------


mean(cars$disp)
sd(cars$disp)
table(cars$cyl)
table(cars$cyl, cars$vs)

#importance of "na.rm"
cars$na<-ifelse(cars$qsec > 20, cars$qsec, NA)
mean(cars$na)
mean(cars$na, na.rm = TRUE)





# Basic Statistics --------------------------------------------------------


#Basic stats
cor.test(cars$disp, cars$mpg)
t.test(cars$disp, cars$mpg)

my.model <- lm(disp ~ mpg, data=cars)
summary(my.model)

#need to add a logistic regression

#Anova
crop.data <- read.csv("path/to/your/file/crop.data.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric"))
one.way <- aov(yield ~ fertilizer, data = crop.data)
summary(one.way)

# Plotting and data visualization -----------------------------------------



#Base R plots
plot(cars$mpg)
plot(cars$cyl)
plot(cars$disp, cars$mpg)
plot(cars$cyl, cars$mpg)


#ggplot
#https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
my.plot<-ggplot(cars, aes(disp, mpg))+
  geom_point()+
  #geom_point(aes(colour=cars$cyl))+
  geom_smooth(model=lm)+
  theme_classic()+
  #theme_linedraw()+
  ggtitle("MY PLOT")

ggsave("my.plot.jpg",my.plot, height=4, width=4, dpi=500)


#facet plot
my.plot2<-ggplot(cars, aes(disp, mpg, colour=cyl))+
  geom_point()+
  facet_wrap(~cyl)



#########################################################################
#                                                                       #
#                             Programming                               #
#                                                                       #
#########################################################################



#Writing functions
square.it <- function(x) {
  square <- x * x
  return(square)
}

square.it(5)


#Loops (try not to use loops)
for(i in 1:nrow(cars)){
  cars$names[i]<-(paste("A", i, sep = ""))
}
#use a vectorized method
cars$names2 <- paste("A", 1:nrow(cars), sep = "")



#APPLY family of functions
#(df, 1=row 2=col, function)
cars$apply<-apply(cars[9:12], 1, sum)


#########################################################################
#                                                                       #
#                          Advanced Topics                              #
#                                                                       #
#########################################################################


# Creating packages -------------------------------------------------------
  # http://r-pkgs.had.co.nz/


# Documents with R Markdown -----------------------------------------------
  # https://rmarkdown.rstudio.com/


# Shiny apps --------------------------------------------------------------
  # https://shiny.rstudio.com/


# GIS and maps ------------------------------------------------------------
install.packages("ggmap")
install.packages("maps")
library(ggmap)

# Using ggmap's internal crime dataset from the Houston police dept Jan-Aug 2010.
murder <- subset(crime, offense == "murder")
qmplot(lon, lat, data = murder, colour = I('red'), size = I(3), darken = .3)


