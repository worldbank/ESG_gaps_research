
'''Utility functions for building the ESG data file from API data

Typical use:
>from extract_esg import *
>extract_esg_data('data/esg_metadata.csv', 'data/ESG_wdi.csv')
>write_feather_file('data/ESG_wdi.csv', 'data/ESG_wdi.feather')
'''
import csv
import wbgapi as wb
import pandas as pd
import sys
import warnings

def extract_esg_data(metafile_path, csv_path, progress=True):
    '''Extract ESG data from the API and save to CSV file. set progress=False
    to suppress progress messages being written to stderr

    '''

    # for now, we limit countries to the WDI set. Could also limit to the ESG set which is smaller
    economy_filter = [row['id'] for row in wb.economy.list(skipAggs=True)]

    # limit to 1990 on
    time_range = range(1990, 2051)

    with open(metafile_path, 'r') as meta_fd, open(csv_path, 'w') as csv_fd:
        reader = csv.reader(meta_fd)
        writer = csv.writer(csv_fd)

        writer.writerow(['db', 'iso3c', 'date', 'value', 'indicatorID', 'indicator', 'iso2c', 'country'])

        for row in reader:
            (cets,db) = row[0:2]
            if not db.isnumeric():
                continue # could be header or something not in the API

            if int(db) == 11 or int(db) == 57:
                continue    # archived databases

            wb.db = db
            try:
                if progress:
                    print('Fetching {} ({})'.format(cets, db), file=sys.stderr)

                for elem in wb.data.fetch(cets, time=time_range, skipAggs=True, skipBlanks=True, labels=True):
                    if elem['economy']['id'] in economy_filter:
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


def write_feather_file(csv_path, feather_path):
    '''Convert CSV file on disk to feather
    '''
    esg = pd.read_csv(csv_path, keep_default_na=False, na_values='')
    # these next 2 conversions suppress some notices when R reads the feather file
    esg['db'] = esg['db'].astype('float64')
    esg['date'] = esg['date'].astype('float64')
    esg.to_feather(feather_path)

