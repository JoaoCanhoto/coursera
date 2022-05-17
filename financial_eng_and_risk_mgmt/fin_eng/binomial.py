import numpy as np
import math as m
import scipy.special
from scipy.special import comb

import sys
import git

repo = git.Repo('.', search_parent_directories=True)
sys.path.append(repo.working_tree_dir)
print(repo.working_tree_dir)

from financial_eng_and_risk_mgmt.fin_eng.utils import onePeriodPrice


class Binomial:
    '''
    Binomial model for option pricing
    '''

    def __init__(self):  # create default model
        self.setup(100, 3, 1.07, 0.01, 100., 'european', 'call', 0)

    def setup(self, price, n_period, u, r, s_k, ame_eur, call_put, coupon_rate):
        ''' set up the basic information for binomial model
        :param price: current price
        :param n_period: number of period of the binomial model
        :param u: price for up / current price
        :param r: cash interest rate
        :param s_k: strike k price
        :param ame_eur: option type "american" / "european"
        :param call_put: option type "call" / "put"
        :param coupon_rate: coupon rate
        '''
        self.u = u
        self.d = 1. / u
        self.price = price
        self.n = n_period
        self.R = 1 + r
        self.strike_k = s_k
        self.coupon_rate = coupon_rate  # coupon rate / dividend
        self.type = [call_put, ame_eur]
        self.check_arbitrage()
        self.set_risk_neutral_prob()

    def set_binomial_from_black_scholes(self, maturity_T, r, n_period, c_dividend, sigma, price, s_k_price,
                                        type_amer_eur,
                                        type_call_put):
        ''' set parameters by Black Scholes Model
         :param T: maturity T
         :param r: continuously-compounded interest rate at maturity T
         :param n_period: number of period of the binomial model
         :param c_dividends: dividend yield of c
        '''
        self.u = m.exp(sigma * m.sqrt(maturity_T / n_period))
        self.d = 1. / self.u
        self.price = price
        self.R = m.exp(r * maturity_T / n_period)
        self.n = n_period
        self.coupon_rate = self.R - m.exp((r - c_dividend) * maturity_T / n_period)
        self.strike_k = s_k_price
        self.type = [type_call_put, type_amer_eur]
        self.check_arbitrage()
        self.set_risk_neutral_prob()

    def set_type(self, type_call_put, type_amer_eur):
        self.type = [type_call_put, type_amer_eur]

    def display_setup(self):
        print("\n-----------------------------------------")
        print("--> Binomial model setup:")
        print("    price:          \t{:.1f}".format(self.price))
        print("    strike k price: \t{:.1f}".format(self.strike_k))
        print("    numb periods:   \t{}".format(self.n))
        print("    cash interest rate:\t{:.5f}".format(self.R))
        print("    coupon rate:    \t{:.5f}".format(self.coupon_rate))
        print("    u price:        \t{:.3f}".format(self.u))
        print("    d price:        \t{:.3f}".format(self.d))
        print("    option type:    \t", self.type)
        print("    q:            \t\t{:.3f}".format(self.q))
        print("-----------------------------------------")

    def check_arbitrage(self):
        ''' check up price and interest rate prevent arbitrage oppotunity
        '''
        if self.u < 1.:
            print("!! no price up is wrong!!", self.u)
        if self.u < self.R:
            print("!! arbitrage by short sell stock !!", self.u, self.R)
        if self.d > self.R:
            print("!! arbitrage by borrow cash to buy !!", self.d, self.R)

    def set_risk_neutral_prob(self):
        ''' return risk neutral probabilities
        '''
        self.check_arbitrage()
        self.q = (self.R - self.d - self.coupon_rate) / (self.u - self.d)

    #################################################
    #################################################
    #################################################
    def calc_option_price(self, display=True):
        if display: self.display_setup()

        if "american" in self.type:
            self.calc_option_diagram(self.type[0], self.type[1])
            return self.diagram[0][0]
        if "european" in self.type:
            return self.calc_european_option()

    def calc_european_option(self):
        ''' calculate call option price
            the prices for american and european are same
        '''
        gain = self.calc_gain_array_at_t(self.n, self.type[0])
        qv = [(self.q) ** i * (1 - self.q) ** (self.n - i) * comb(self.n, i) for i in range(self.n, -1, -1)]
        return np.dot(gain, qv) / self.R ** self.n

    def calc_option_diagram(self, option_type, amer_eur):
        list_is_early_stop = []
        list_periods = []
        for i in range(self.n, -1, -1):
            # print("\n-------------t=",i)
            if i == self.n:
                gain_i = self.calc_gain_array_at_t(i, option_type)
                list_periods.append(gain_i)
                list_is_early_stop.append(np.zeros_like(gain_i))
                continue
            option_price_tp1 = list_periods[-1]
            option_price = [onePeriodPrice(option_price_tp1[j], option_price_tp1[j + 1], self.q, (self.R - 1)) for j in
                            range(0, i + 1)]
            # option_stop = np.zeros_like(option_price)

            gain_i = self.calc_gain_array_at_t(i, option_type)
            if amer_eur == "american":
                option_price = np.maximum(gain_i, option_price)
            option_stop = gain_i > option_price
            list_periods.append(option_price)
            list_is_early_stop.append(option_stop)
        # print("option_price_f",len(option_price_f), option_price_f)
        self.diagram = list_periods[::-1]
        self.diagram_stop = list_is_early_stop[::-1]
        return self.diagram

    def calc_gain_array_at_t(self, t, option_type):
        if option_type == "call":
            gain = self.get_stock_price_array_at_t(t) - self.strike_k
        elif option_type == "put":
            gain = self.strike_k - self.get_stock_price_array_at_t(t)
        gain = np.maximum(gain, np.zeros_like(gain))
        return gain

    def get_stock_price_array_at_t(self, t):
        ''' return stock price at time t
        :param t: time step t
        '''
        return np.array([(self.u) ** i * (self.d) ** (t - i) \
                         for i in range(t, -1, -1)]) * self.price

    def display_diagram(self, option_stop=False):
        self.calc_option_diagram(self.type[0], self.type[1])
        if option_stop:
            self.print_diagram(self.diagram_stop, "gain>price")
        self.print_diagram(self.diagram)

    def display_diagram_stop(self):
        self.print_diagram(self.diagram_stop)

    def print_diagram(self, diagram, text="prices"):
        print("\n-------- binomial model for {}, for {} periods --------".format(text, self.n))
        for row in range(0, len(diagram[-1])):
            string = ""
            for col in range(0, len(diagram)):
                if row < len(diagram[col]):
                    string += "\t{:.2f}".format(diagram[col][row])
                else:
                    string += "\t     "
            print(string)
        print("-----------------------------------------")
