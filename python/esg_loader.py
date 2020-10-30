
'''Utility functions for building the ESG data file from API data

Typical use:
>import esg_loader as loader
>loader.extract_esg_data('data/esg_metadata.csv', 'data/ESG_wdi.csv')
>loader.write_feather_file('data/ESG_wdi.csv', 'data/ESG_wdi.feather')
>meta = loader.load_metadata('data/esg_metadata.csv', 'data/ESG_wdi.feather')
'''
import csv
import wbgapi as wb
import pandas as pd
import numpy as np
import sys
import os
import warnings

def extract_esg_data(metafile_path, csv_path, progress=True):
    '''Extract ESG data from the API and save to CSV file. set progress=False
    to suppress progress messages being written to stderr

    '''

    # for now, we limit countries to the WDI set. Could also limit to the ESG set which is smaller
    economies = {row['id']:row['value'] for row in wb.economy.list(skipAggs=True)}

    # limit to 1990 on
    time_range = range(1990, 2051)



    with open(metafile_path, 'r') as meta_fd, open(csv_path, 'w') as csv_fd:
        reader = csv.reader(meta_fd)
        writer = csv.writer(csv_fd)

        writer.writerow(['db', 'iso3c', 'date', 'value', 'indicatorID', 'indicator', 'iso2c', 'country'])

        next(reader)  # toss the header row
        for row in reader:
            (cets,db) = row[0:2]
            if db == '11' or db == '57':
                continue    # archived databases

            if progress:
                print('Fetching {} ({})'.format(cets, db), file=sys.stderr)

            localDataPath = os.path.sep.join([os.path.dirname(os.path.realpath(__file__)), 'local', '{}.csv'.format(cets)])
            if os.path.isfile(localDataPath):
                wb.db = 2 # Assume WDI countries and codes
                with open(localDataPath) as localFile:
                    localReader = csv.reader(localFile)
                    next(localReader) # read and toss the header
                    for elem in localReader:
                        if elem[1] in economies:
                            writer.writerow([0, elem[1], elem[2], elem[3], cets, elem[0], wb.economy.iso2(elem[1]), economies[elem[1]]])
            else:
                try:
                    wb.db = db
                    for elem in wb.data.fetch(cets, time=time_range, skipAggs=True, skipBlanks=True, labels=True, numericTimeKeys=True):
                        cets2 = '-'.join([cets, elem['economy']['id']])
                        tv = elem['time']['id']

                        if elem['economy']['id'] in economies:
                            # writer.writerow([db, elem['series'], elem['economy'], elem['time'], elem['value']])
                            writer.writerow([db, elem['economy']['id'],
                                elem['time']['value'],
                                elem['value'],
                                elem['series']['id'],
                                elem['series']['value'],
                                wb.economy.iso2(elem['economy']['id']),
                                elem['economy']['value']])
                except wb.APIError as err:
                    warnings.warn('ERROR {} ({})\n{}'.format(cets, db, err), RuntimeWarning)


def extract_esg_data2(meta_path, csv_path, progress=True):
    '''Extracts ESG data from the API, but only from the ESG database (db=75).
    The metadata table is only used to include the sector (ENV, SOC or GOV)
    which replace the database ID in the output
    '''

    meta = pd.read_csv(meta_path).query('wbgv1==1').set_index('cetsid')
    time_range = range(1990, 2051)
    with open(csv_path, 'w') as csv_fd:
        writer = csv.writer(csv_fd)
        
        writer.writerow(['sector', 'iso3c', 'date', 'value', 'indicatorID', 'indicator', 'iso2c', 'country'])
        esg_db = 75
        indicators = [row['id'] for row in wb.series.list(db=esg_db)]
        n = 0
        for cetsid in indicators:
            if progress:
                n += 1
                print('Fetching {} ({} of {})'.format(cetsid, n, len(indicators)), file=sys.stderr)

            for elem in wb.data.fetch(cetsid, time=time_range, skipAggs=True, skipBlanks=True, labels=True, numericTimeKeys=True, db=esg_db):
                writer.writerow([meta.loc[cetsid]['sector'], elem['economy']['id'],
                    elem['time']['value'],
                    elem['value'],
                    elem['series']['id'], elem['series']['value'],
                    wb.economy.iso2(elem['economy']['id']),
                    elem['economy']['value']])
                

def write_feather_file(csv_path, feather_path):
    '''Convert CSV file on disk to feather
    '''
    esg = pd.read_csv(csv_path, keep_default_na=False, na_values='')
    # these next 2 conversions suppress some notices when R reads the feather file
    if esg.get('db'):
        esg['db'] = esg['db'].astype('float64')

    esg['date'] = esg['date'].astype('float64')
    esg.to_feather(feather_path)


def load_metadata(metafile_path, datafile_path):
    '''Loads the metadata file and dynamically calculates the expl_ variables
    based on business logic and the contents of the database
    '''

    # some helpful lists of columns
    expl_a_g = list(map(lambda x: 'expl_'+x, list('abcdefg')))
    expl_c_g = list(map(lambda x: 'expl_'+x, list('cdefg')))

    meta = pd.read_csv(metafile_path).set_index('cetsid')
    data = pd.read_feather(datafile_path)
    data = data[data.date <= 2018]
    
    # calculate no_pop, defined as any indicator with a value for 2018 or later for 90%+ of economies
    min_economies = len(data.iso3c.unique()) * 0.9

    # dataframe of indicators with counts of countries with at least one MRV
    mrv = data[data.date>=2018].groupby(['indicatorID', 'iso3c']).count().groupby('indicatorID').count()
    meta['no_gap'] = mrv.value>=min_economies
    meta['no_gap'] = meta['no_gap'].fillna(False) # to fix indicators not found in mrv

    # expl_a is defined as archived. That's databases 11 (Africa Development Indicators) or 57 (WRI archives)
    meta['expl_a'] = (meta.database_id == '11') | (meta.database_id == '57')

    # expl_b is defined as very stale: no values past 2014
    tmp = data.groupby('indicatorID').max()['date']  # max year for each indicator
    meta['expl_b'] = tmp <= 2014
    meta['expl_b'] = meta['expl_b'].fillna(False)    # fix indicators not in database (e.g., archives)

    # ... but not any archived variables
    meta.loc[meta.expl_a, 'expl_b'] = False

    # apply additional business logic. convert 1:0 to booleans
    for row in expl_c_g:
        meta[row] = meta[row].map(lambda x: x==1)
    
    # if no_gap then all expls are false
    meta.loc[meta.no_gap, expl_a_g] = False

    # if a or b then c-g must be false
    meta.loc[meta.expl_a | meta.expl_b, expl_c_g] = False

    return meta.reset_index()
