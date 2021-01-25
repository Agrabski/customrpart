library(rpart)
library(keras)


makeRows <- function(rows, probs) {
	someValue <- 100 # punk zaczebny wartości
	avgDifferance <- c(20,20) # srednia różnica klasy 1 i 2 od wartości początkowej
	avgValue <- c(someValue-avgDifferance[1], someValue+avgDifferance[2]) # srednia wartość klas 1 i 2
	variance <- c(2,2) #ilość srednich różnić pomiedzy srednią wartościa a wartościa max/min. większa od 1 tak aby wartosci na siebie nachodzily


	colNames <- c("class")
	for (i in 1:length(probs))
	{
		colNames <- c(colNames, paste("arg", i,sep = ""));
	}

	data<-data.frame()


	for (i in 1:rows)
	{
		#wybierz klase
		newClass <- sample(c(1,2), 1)
		newRow <- c(newClass)
		for (i in 1:length(probs))
		{
			#wybier czy arg powinien miec wartosc zgodna z klasa do ktorej nalezy
			if(newClass == 1)
			{
				argClass <- sample(c(1,2),size = 1, replace = TRUE, prob = c(probs[i], 1 - probs[i]));
			}
			else
			{
				argClass <- sample(c(1,2),size = 1, replace = TRUE, prob = c(1 - probs[i], probs[i]));
			}

			#wybierz jedna z wartosci ktora nalezy do zakresu danej klasy
			argValue <- sample((avgValue[argClass] - avgDifferance[argClass]*variance[argClass]):(avgValue[argClass] + avgDifferance[argClass]*variance[argClass]), size=1)
			newRow <- c(newRow, argValue)
		}
		data <- rbind(data, newRow)
	}
	names(data) <- colNames
	return(data);
}

#liczby użyte w warunkach wyboru innego arg powinny zostac poprawione na podstawie wyników
FuncArg2MaxPreffered <- function(x,y)
{
	if(is.element("arg2", x))
	{
		if(y[match("arg2", x)]/max(y) > 0.05)
		{
			result = x[match("arg2", x)]
		}
		else
		{
			result = x[0];
		}
	}
	result = x[1];
}

FuncArg2MuchPreffered <- function(x,y)
{
	if(is.element("arg2", x))
	{
		if(y[match("arg2", x)]/max(y) > 0.3)
		{
			result = x[match("arg2", x)]
		}
		else
		{
			result = x[0];
		}
	}
	result = x[1];
}

FuncArg2Preffered <- function(x,y)
{

	if(is.element("arg2", x))
	{
		if(y[match("arg2", x)]/max(y) > 0.7)
		{
			result = x[match("arg2", x)]
		}
		else
		{
			result = x[0];
		}
	}
	result = x[1];
}

FuncArg1MaxPreffered <- function(x,y)
{

	if(is.element("arg1", x))
	{
		if(y[match("arg1", x)]/max(y) > 0.05)
		{
			result = x[match("arg1", x)]
		}
		else
		{
			result = x[0];
		}
	}
	result = x[1];
}

FuncArg1MuchPreffered <- function(x,y)
{

	if(is.element("arg1", x))
	{
		if(y[match("arg1", x)]/max(y) > 0.3)
		{
			result = x[match("arg1", x)]
		}
		else
		{
			result = x[0];
		}
	}
	result = x[1];
}

FuncArg1Preffered <- function(x,y)
{

	if(is.element("arg1", x))
	{
		if(y[match("arg1", x)]/max(y) > 0.7)
		{
			result = x[match("arg1", x)]
		}
		else
		{
			result = x[0];
		}
	}
	result = x[1];
}

MakeTestRun <- function(usedFunction, correctProbs, incorrectProbs)
{
	runs = 10
	rpartAcc = 0
	ourAcc = 0
	#Srednia dla kilku podobnych zestawów danych
	for (i in 1:runs)
	{
		#stwórz drzewo
		data <- makeRows(500, incorrectProbs)
		rpartResult <- rpart::rpart(class ~ arg1 + arg2 + arg3, data = data, method="class")
		#
		ourResult <- rpart::rpart(class ~ arg1 + arg2 + arg3, data = data, method="class", split_pick_function=usedFunction)

		#dopasuj nowe dane
		newData <- makeRows(1000, correctProbs)
		rpartPredict <- predict(rpartResult,newData,type="vector")
		ourPredict <- predict(ourResult,newData,type="class")

		plot(rpartResult)
		plot(ourResult)

		#policz precyzje predykcji
		rpartConfMat <- table(newData$class,rpartPredict)
		ourConfMat <- table(newData$class,ourPredict)
		rpartAcc = rpartAcc + sum(diag(rpartConfMat))/sum(rpartConfMat)
		ourAcc = ourAcc + sum(diag(ourConfMat))/sum(ourConfMat)

	}
	print(paste("Precyzja domyślego podziału: ", rpartAcc / runs))
	print(paste("Precyzja podziału użytej funkcji: ", ourAcc / runs))
}


print("Wiedza domenowa")
print("Drugi z parametrów jest istotniejszy od 3. Z danych winika że jest mniej użyteczny")
correctProbs <- c(0.5, 0.95, 0.7)
incorrectProbs <- c(0.5, 0.7, 0.95)
print("Użyj Arg2 jako istotniejszego z bardzo dużą pewnością")
MakeTestRun(FuncArg2MaxPreffered, correctProbs, incorrectProbs)
print("Użyj Arg2 jako istotniejszego z dużą pewnością")
MakeTestRun(FuncArg2MuchPreffered, correctProbs, incorrectProbs)
print("Użyj Arg2 jako istotniejszego z pewną pewnością")
MakeTestRun(FuncArg2Preffered, correctProbs, incorrectProbs)

print("Drugi z parametrów jest istotniejszy od 3. Z danych winika że są równie użyteczne")
correctProbs <- c(0.5, 0.8, 0.6)
incorrectProbs <- c(0.5, 0.6, 0.6)
print("Użyj Arg2 jako istotniejszego z bardzo dużą pewnością")
MakeTestRun(FuncArg2MaxPreffered, correctProbs, incorrectProbs)
print("Użyj Arg2 jako istotniejszego z dużą pewnością")
MakeTestRun(FuncArg2MuchPreffered, correctProbs, incorrectProbs)
print("Użyj Arg2 jako istotniejszego z pewną pewnością")
MakeTestRun(FuncArg2Preffered, correctProbs, incorrectProbs)

pprint("Drugi z parametrów jest istotniejszy od 3 bardziej niż winika to z danych")
correctProbs <- c(0.5, 0.8, 0.6)
incorrectProbs <- c(0.5, 0.7, 0.6)
print("Użyj Arg2 jako istotniejszego z bardzo dużą pewnością")
MakeTestRun(FuncArg2MaxPreffered, correctProbs, incorrectProbs)
print("Użyj Arg2 jako istotniejszego z dużą pewnością")
MakeTestRun(FuncArg2MuchPreffered, correctProbs, incorrectProbs)
print("Użyj Arg2 jako istotniejszego z pewną pewnością")
MakeTestRun(FuncArg2Preffered, correctProbs, incorrectProbs)

print("Drugi i trzeci z parametrów jest istotniejszy niż wynika to z danych, ale wiemy tylko o 2. 2 jest istotniejszy")
correctProbs <- c(0.5, 0.8, 0.7)
incorrectProbs <- c(0.5, 0.6, 0.6)
print("Użyj Arg2 jako istotniejszego z bardzo dużą pewnością")
MakeTestRun(FuncArg2MaxPreffered, correctProbs, incorrectProbs)
print("Użyj Arg2 jako istotniejszego z dużą pewnością")
MakeTestRun(FuncArg2MuchPreffered, correctProbs, incorrectProbs)
print("Użyj Arg2 jako istotniejszego z pewną pewnością")
MakeTestRun(FuncArg2Preffered, correctProbs, incorrectProbs)

print("Drugi i trzeci z parametrów jest istotniejszy niż wynika to z danych, ale wiemy tylko o 2. 3 jest istotniejszy")
correctProbs <- c(0.5, 0.7, 0.8)
incorrectProbs <- c(0.5, 0.6, 0.6)
print("Użyj Arg2 jako istotniejszego z bardzo dużą pewnością")
MakeTestRun(FuncArg2MaxPreffered, correctProbs, incorrectProbs)
print("Użyj Arg2 jako istotniejszego z dużą pewnością")
MakeTestRun(FuncArg2MuchPreffered, correctProbs, incorrectProbs)
print("Użyj Arg2 jako istotniejszego z pewną pewnością")
MakeTestRun(FuncArg2Preffered, correctProbs, incorrectProbs)



print("Brak wiedzy domenowej")
print("Wybór parametru który nie wpływa na klase")
correctProbs <- c(0.5, 0.7, 0.7)
incorrectProbs <- c(0.5, 0.7, 0.7)
print("Użyj Arg1 jako istotniejszego z bardzo dużą pewnością")
MakeTestRun(FuncArg1MaxPreffered, correctProbs, incorrectProbs)
print("Użyj Arg1 jako istotniejszego z dużą pewnością")
MakeTestRun(FuncArg1MuchPreffered, correctProbs, incorrectProbs)
print("Użyj Arg1 jako istotniejszego z pewną pewnością")
MakeTestRun(FuncArg1Preffered, correctProbs, incorrectProbs)

print("Wybór parametru ktory minimalnie wpływa na klase")
correctProbs <- c(0.55, 0.7, 0.7)
incorrectProbs <- c(0.55, 0.7, 0.7)
print("Użyj Arg1 jako istotniejszego z bardzo dużą pewnością")
MakeTestRun(FuncArg1MaxPreffered, correctProbs, incorrectProbs)
print("Użyj Arg1 jako istotniejszego z dużą pewnością")
MakeTestRun(FuncArg1MuchPreffered, correctProbs, incorrectProbs)
print("Użyj Arg1 jako istotniejszego z pewną pewnością")
MakeTestRun(FuncArg1Preffered, correctProbs, incorrectProbs)

print("Wybór parametru ktory wydaje sie wpływać na klase, ale jednak na nią nie wpływa")
correctProbs <- c(0.5, 0.7, 0.7)
incorrectProbs <- c(0.6, 0.7, 0.7)
print("Użyj Arg1 jako istotniejszego z bardzo dużą pewnością")
MakeTestRun(FuncArg1MaxPreffered, correctProbs, incorrectProbs)
print("Użyj Arg1 jako istotniejszego z dużą pewnością")
MakeTestRun(FuncArg1MuchPreffered, correctProbs, incorrectProbs)
print("Użyj Arg1 jako istotniejszego z pewną pewnością")
MakeTestRun(FuncArg1Preffered, correctProbs, incorrectProbs)




