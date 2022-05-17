import sys
import git

repo = git.Repo('.', search_parent_directories=True)
sys.path.append(repo.working_tree_dir)
print(repo.working_tree_dir)
from financial_eng_and_risk_mgmt.fin_eng.utils import onePeriodPrice, risk_neutral_prob, \
    forward_price_of_fixed_value_array, forward_price_array

#################################################################
# question 1
"""Pricing a forward contract with dividends
The current price of silver is $206 per ounce. The storage cost is $1 ounce per year, payable quarterly in advance. 
Assuming a constant annual interest rate of 9% compounded quarterly, what is the theoretical forward price of silver for delivery in 9 months?"""
print("\n##############################################################################")
price = 206
r = 0.09
cost = 1
fp = forward_price_array(price, r / 4, 3)
cost = forward_price_of_fixed_value_array(cost / 4, r / 4, 3)

print("foward price:", fp)
print("cost arrays", cost)
price_f = fp[-1] + cost[-1]
print("Question 1:", price_f)

#################################################################
# question 2
"""Pricing Call Options
Consider a 11-period binomial model with R=1.05, S_0 = 50, u=1/d= 1.08. 
What is the value of a European call option on the stock with strike K=52, assuming that the stock does not pay dividends?"""
print("\n##############################################################################")
R = 1.05
price = 50
strike_price = 52
u = 1.08
d = 1 / u
q = risk_neutral_prob(R, u, d, 0)
print("q=", q)
q2 = onePeriodPrice(u * price - strike_price, 0, q, R - 1)
print("Question 2: {}".format(q2))

#################################################################
# question 3
"""Minimum-variance Hedging
A farmer has a crop of grapefruit juice that will be ready for harvest and sale as 150000 pounds of grapefruit juice in 
33 months. He is worried about possible price changes, so he is considering hedging - a financial engineering technique 
that minimizes future uncertainties in the cash flow. Typically, hedging is carried out using futures contract. 
However, unfortunately, there is no futures contract for grapefruit juice, but there is a futures contract for orange 
juice. Still, the farmer might consider using the futures contract for orange juice as a replacement for futures contract 
for grapefruit juice, in the hope that these two contracts are highly correlated due to the similarity of the underlying 
products.

Currently, the spot prices are $1.20 per pound for orange juice and $1.50 per pound for grapefruit juice. 
The standard deviation of the prices of orange juice and grapefruit juice is about 20% per year, 
and the correlation coefficient between their prices is about 0.7 (highly correlated). 
What is the minimum variance hedge for farmer, i.e. how many orange juice futures contracts does the farmer need 
to purchase in total?"""
print("\n##############################################################################")

production = 150000
sigma_orange = 0.2
sigma_apple = sigma_orange
corr = 0.7
cov = corr * sigma_orange * sigma_apple
var_orange = sigma_orange * sigma_orange
c = -2 * cov / (2 * var_orange)
print("Question 3: y={}".format(c))
print("            r={}".format(c * production))

#################################################################
# question 5
"""Replicating strategy for pricing
To price a derivative, we can use risk-neutral pricing. As an alternative yet equivalent method, we can construct price 
the derivative using replicating strategies.
Consider a 1-period binomial model, where we have three nodes N_{0,0},N_{1,0},N_{1,1}. 
We have a cash account with fixed interest rate r = 0.01 - that is, if we have at N_{0,0} cash account with D dollars, 
we expect (1+r)D at either N_{1,0} or N_{1,1}. 
We also have a stock with price S_{0,0} = 10 at N_{0,0}, S_{1,0} = 8 at N_{1,0} and S_{1,1} = 15 at N_{1,1}.
Now consider an European call option at time t=1 with strike K=12. Can you price the option at N_{0,0} 
using a replicating strategy, and what is the price?"""
print("\n##############################################################################")
# r= 0.01
# print(-10+12/(1+r))
# print(3/(1+r))
# print("question 5: {}".format(0))

# by risk neutral...
r = 0.01
R = 1 + r
u = 15 / 10
d = 8 / 10
q = risk_neutral_prob(R, u, d, 0)
q5 = onePeriodPrice(15 - 12, 0, q, r)
print("Question 5: ", q5)

# 15 x + R* y = 3
# 8 x + R*y = 0 -> x = -R*y/8
# (-15*R/8)y+R*y = 3 -> y = 3/(R*(1-15/8)
y = 3 / (R * (1 - 15 / 8))
x = -R * y / 8
price = x * 10 + y
print("y=", y)
print("x=", x)
print("Question 5: price = ", price)
