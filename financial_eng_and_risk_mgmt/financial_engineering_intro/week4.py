
'''
Suppose the current term structure of interest rates, assuming annual compounding, is as follows:
    s1	s2	 s3	 s4	s5	 s6
    7.0%	7.3%	7.7%	8.1%	8.4%	8.8%
What is the discount rate d(0,4)? (Recall that interest rates are always quoted on an annual basis unless stated otherwise.)
Please submit your answer rounded to three decimal places. For example, if your answer is 0.4567 then you should submit an answer of 0.457.
'''
def ex1():
    s=[0.07, 0.073, 0.077, 0.081, 0.084, 0.088]
    # result = numpy.prod([1/(1+u) for u in s[0:4]])
    result = 1/((1+s[3])**4)
    print("Q1: result = ", result) # 0.748


'''
    Term structure of interest rates and swap valuation
    Suppose the current term structure of interest rates, assuming annual compounding, is as follows:
    s1	s2	 s3	 s4	s5	 s6
    7.0%	7.3%	7.7%	8.1%	8.4%	8.8%
    
    Recall that interest rates are always quoted on an annual basis unless stated otherwise.
    Suppose a 6-year swap with a notional principal of $10 million is being configured. What is the fixed rate of interest that will make the value of the swap equal to zero? Round your answer to 3 decimal points (in decimal form, not in percentage).
'''
def ex2():
    s=[0.07, 0.073, 0.077, 0.081, 0.084, 0.088]
    d = [1/((1+u)**(i+1)) for i,u in enumerate(s)]
    result = (1-d[-1])/sum(d)
    print("Q2: result = ", result*100) # 8.62

def ex3():
    F0= 118.65
    print("Q3: result = ", F0)

'''
Call Options
Consider a 11-period binomial model with R=1.02, S_0 = 100, u=1/d= 1.05. 
Compute the value of a European call option on the stock with strike K=102. The stock does not pay dividends.

Please submit your answer rounded to two decimal places. For example, if your answer is 3.4567 then you should submit an answer of 3.46.
'''
def ex4():
    R=1.02
    S0=100
    u=1.05
    d=1/u
    K=102

    q=(R-d)/(u-d)
    Cu=u*S0-K
    Cd=0
    C0 = (1/R)*(q*Cu+(1-q)*Cd)
    print("Q4: result = ", C0)

'''
When you construct the replicating portfolio for the option in the previous question, how many dollars do you need to invest in the cash account?

Please submit your answer rounded to three decimal places. For example, if your answer is -43.4567 then you should submit an answer of -43.457.'''
def ex5():
    R=1.02
    S0=100
    u=1.05
    d=1/u
    K=102

    Su=u*S0
    Sd=d*S0
    Cu=u*S0-K
    Cd=0
    # solve the equations for y
    # u*S0*x +K*y = Cu
    # d*S0*x +K*y = Cd
    y = (1/R)*(Cd-Sd*Cu/Su)/(1-Sd/Su)
    print("Q5: result = ", y)

if __name__=="__main__":
    ex1()
    ex2()
    ex3()
    ex4()
    ex5()

