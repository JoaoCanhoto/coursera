import sys

import git

repo = git.Repo(".", search_parent_directories=True)
sys.path.append(repo.working_tree_dir)

from financial_eng_and_risk_mgmt.fin_eng.binomial import Binomial


#################################################################
# question 1
"""You should build a 15-period binomial model whose parameters are calibrated to a Black-Scholes geometric Brownian 
motion model with: T = 0.25 years, S_{0} = 100, r = 2%, \sigma = 30\% and a dividend yield of c = 1%. 
Your binomial model should use a value of u = 1.0395... 
(This has been rounded to four decimal places but you should not do any rounding in your spreadsheet calculations.)
Compute the price of an American call option with strike K=110 and maturity T=.25 years."""
# ---- question 1: Compute the price of an American call option
bnm = Binomial()
bnm.set_binomial_from_black_scholes(
    maturity_T=0.25,
    r=0.02,
    n_period=15,
    c_dividend=0.01,
    sigma=0.3,
    price=100,
    s_k_price=110,
    type_amer_eur="american",
    type_call_put="call",
)

print(
    "\n##############################################################################"
)
q1 = bnm.calc_option_price()
bnm.display_diagram()
print(">>>>>>>>>>>>>>>>>>>>>>>>>")
print("Question 1: r={:.2f}".format(q1))


print(
    "\n##############################################################################"
)
bnm.set_type("put", "american")
q2 = bnm.calc_option_price()
bnm.display_diagram(True)
print(">>>>>>>>>>>>>>>>>>>>>>>>>")
print("Question 2: r={:.2f}".format(q2))


print(
    "\n##############################################################################"
)
bnm.set_type("call", "american")
bnm.n = 10
q6 = bnm.calc_option_price()
bnm.display_diagram(True)
print(">>>>>>>>>>>>>>>>>>>>>>>>>")
print("Question 6: r={:.2f}".format(q6))


bnm.set_type("put", "american")
bnm.display_diagram(True)

print("testest")
print(bnm.calc_gain_array_at_t(0, "call"))
print(bnm.calc_gain_array_at_t(0, "put"))
