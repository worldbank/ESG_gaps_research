
'''
This module defines functions to compute cofficients of variation (CV) scores
at the indicator and country level, and return them as a data frame with cetsid
for the index and iso3 codes as columns. Since this is very time intensive, the
result is typically saved to a feather file or similar.

NB: this file is now deprecated. On the python side cvs are calculated at runtime
much more efficiently than done here, so it is no longer time intensive.

Typical use:

import pandas as pd
from esg_cv import esg_cv
data = pd.read_feather('data/ESG_wdi.feather')
cv = esg_cv(data)
cv.reset_index().to_feather('data/ESG_cv.feather')

Then to read the saved file:

cv = pd.read_feather('data/ESG_cv.feather').set_index('cetsid')
'''

import pandas as pd
import numpy as np
import warnings

def esg_cv(data):
    '''Builds a dataframe of CV scores per indicator and country

    Note this does not use stats.variation but instead looks for 
    0-length arrays, 0 means, etc in an attempt to provide a useful return value:

    cv = σ / µ

    empty array: cv=nan
    σ=0: cv=0 (regardless of µ)
    µ=0: cv=nan
    '''

    def cv(s):
        if len(s) == 0:
            
        return np.std(s, ddof=1) / np.mean(s)

    cvs = pd.DataFrame()
    cvs.index.name = 'cetsid'
    with warnings.catch_warnings():
        warnings.simplefilter('error')

        for ind in data.indicatorID.unique():
            for c in data.iso3c.unique():
                s = data[(data.indicatorID==ind)&(data.iso3c==c)].value.dropna()

                if len(s) == 0:
                    cv = np.nan
                else:
                    std = np.std(s, ddof=1)
                    mean = np.mean(s)
                    if std == 0:
                        cv = 0
                    elif mean == 0:
                        cv = np.nan
                    else:
                        cv = std / mean

                cvs.loc[ind, c] = cv
    
    return cvs
