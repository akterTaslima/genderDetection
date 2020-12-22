
pairs(mtcars);


boxplot(mpg~cyl,data=mtcars, main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon")  


# mpg~cyl means produce a different boxplot for each number of cylinders


boxplot(mpg~am,data=mtcars, main="Car Milage Data", xlab="Transmission", ylab="Miles Per Gallon")  # am=(0,1) = (automatic,manual)


boxplot(mpg~gear,data=mtcars, main="Car Milage Data", xlab="Forward Gears", ylab="Miles Per Gallon")  # am=(0,1) = (automatic,manual)

stopp;

