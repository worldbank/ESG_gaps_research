
import pandas as pd
import os

def esg_mrv(data_dir, year=2018, cv_max=0.5, years_to_impute=1):
    '''Returns a data frame of mrv analysis data suitable for plotting

    Arguments:
        data_dir:  path to the data directory

        year:      mrv year to analyze

        cv_max:    maximum cv threshold: cvs above this will not impute. Set to None to perform
                   no calculation

        years_to_impute: number of years to impute beyond the actual mrv if cv is within range
    '''

    data = pd.read_feather(os.path.join(data_dir, 'ESG_wdi.feather'))
    cv   = pd.read_feather(os.path.join(data_dir, 'ESG_cv.feather')).set_index('cetsid')

    mrv = data.pivot_table(index='indicatorID', columns='iso3c', values='date', aggfunc='max')

    nCountries = len(mrv.columns)
    nIndicators = len(mrv.index)
    z = data.set_index('indicatorID')[['indicator']].drop_duplicates()

    m = pd.DataFrame(index=mrv.index)
    m['indicator'] = m.index.map(lambda x: z.loc[x].indicator)
    m['baseline'] = mrv.apply(lambda x: x>=year).sum(axis=1)/nCountries

    if cv_max is not None:
        test_mrv = mrv + cv.applymap(lambda x: years_to_impute if x<=cv_max else 0)
        m['imputed'] = (test_mrv.apply(lambda x: x>=year).sum(axis=1)/nCountries)
        m['gain'] = m['imputed'] - m['baseline']

        m.sort_values('imputed', ascending=False, inplace=True)
    else:
        m.sort_values('baseline', ascending=False, inplace=True)

    return m
