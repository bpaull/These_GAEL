#!/usr/bin/python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov 30 11:38:55 2020

@author: paul
"""

# %% import package
import numpy as np
import pandas as pd
import pymc as pm

# %% function definition

def poisson_2_param_MCMC(X1, X2):
    """
    This function compute estimation using pymc.
    """
    total_data = pd.concat([X1, X2])
    
    alpha = 1/total_data.mean()

    l1 = pm.Exponential("l1", alpha)
    l2 = pm.Exponential("l2", alpha)

    nb_x1 = len(X1)
    nb_to = len(total_data)

    @pm.deterministic
    def lambda_(l1 = l1, l2 = l2):
        out = np.zeros(nb_to) # number of data points
        out[:nb_x1] = l1 # lambda before tau is lambda_1
        out[nb_x1:] = l2 # lambda after (and including) tau is lambda_2
        return out
    
    @pm.deterministic
    def delta(l1 = l1, l2 = l2):
        return l2 - l1
    
    observation = pm.Poisson("obs", lambda_, value = total_data, observed=True)
    model = pm.Model([observation, l1, l2, delta])
    
    mcmc = pm.MCMC(model)
    mcmc.sample(40000, 10000)
    
    return {"lambda_1": mcmc.trace('l1')[:],
            "lambda_2": mcmc.trace('l2')[:],
            "delta": mcmc.trace('delta')[:]}



# %% test zone
# data_set = pd.read_csv("prepared_data.csv")
# data_set

# tx1 = data_set[(data_set["round_number"] < 6) & (data_set["in_baseline"])]["pumps"]
# tx2 = data_set[(data_set["round_number"] > 5) & (data_set["in_baseline"])]["pumps"]
