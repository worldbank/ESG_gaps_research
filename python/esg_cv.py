
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

    cvs = pd.DataFrame()
    pd.index.name = 'cetsid'
    with warnings.catch_warnings():
        warnings.simplefilter('error')

        for ind in data.indicatorID.unique():
            for c in data.iso3c.unique():
                s = data[(data.indicatorID==ind)&(data.iso3c==c)].value.dropna()

                if len(s) == 0:
                    cv = np.nan
                else:
                    std = np.std(s)
                    mean = np.mean(s)
                    if std == 0:
                        cv = 0
                    elif mean == 0:
                        cv = np.nan
                    else:
                        cv = std / mean

                cvs.loc[ind, c] = cv
    
    return cvs
