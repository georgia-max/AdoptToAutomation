#Function 
#2. Test for exogeneity  Huasman Test 
######
#fixed<- formula("DATE_EVENT+RSE_ID_ANONYM")
Hasusman_Test<- function(formula1, formula2, formula3, df){
  Reg_1st = feols(formula1, data = df, vcov="twoway")
  
  df['residuals']<- Reg_1st$residuals
  
  Hausman_reg<- fenegbin(formula3,vcov = "twoway", data = df)
  
  HausWutest<- waldtest(Hausman_reg, .~.-residuals)
  print(HausWutest)
  #If The result p-value of smaller than 0.05,  So at an a = 0.05 we just fail to reject the null of x is  exogenous.Hence, we need to use IVs. 
  
  Reg_2nd <- fenegbin(formula2,vcov = "twoway", data = df)
  return(Reg_2nd)
}
