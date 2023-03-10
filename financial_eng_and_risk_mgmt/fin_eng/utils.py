def onePeriodPrice(Cu, Cd, q, r):
    """return the one period option price for binomial model

    :param Cu: up price
    :param Cd: down price
    :param q: neutral risk probability
    :param r: cash interest
    :returns: price for previous period
    """
    if r > 1:
        print("ATTENTINO! onePeriodPrice, r usually be lower than 1 r=", r)
    if q <= 0 or q >= 1:
        print("!! q value is wrong !!")
    return (q * Cu + (1 - q) * Cd) / (1 + r)


def risk_neutral_prob(R, u, d, c):
    """
    :param R:
    :param u:
    :param d:
    :param c: coupon_rate
    """
    if R < 0.5:
        print("risk_neutral_prob, R usually around 1! R=", R)
    return (R - d - c) / (u - d)


def forward_price_array(price, r_per_period, n_periods):
    """
    :param price:
    :param r_per_period: interest rate per period
    :param n_periods:
    :return: array of future price per period
    """
    return [price * (1 + r_per_period) ** i for i in range(0, n_periods + 1)]


def forward_price_of_fixed_value_array(fixed_value_per_period, r_per_period, n_periods):
    """
    :param price:
    :param r_per_period:
    :param n_periods:
    :return:
    """
    values_array = [
        [
            (fixed_value_per_period) * (1 + r_per_period) ** (i - j) if i >= j else 0
            for i in range(0, n_periods + 1)
        ]
        for j in range(0, n_periods)
    ]
    return [sum(x) for x in zip(*values_array)]
