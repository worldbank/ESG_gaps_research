
# this script uses the WB API to extract all observations for the study period
# Usage: python extract_esg.py FILE

import csv
import wbgapi as wb
import sys

reader = csv.reader(open(sys.argv[1], 'r'))
writer = csv.writer(sys.stdout)

# for now, we limit countries to the WDI set. Could also limit to the ESG set which is smaller
economy_filter = [row['id'] for row in wb.economy.list(skipAggs=True)]

# limit
time_range = range(1990, 2051)

# writer.writerow(['db', 'series', 'economy', 'time', 'value'])
writer.writerow(['db', 'iso3c', 'date', 'value', 'indicatorID', 'indicator', 'country'])

for row in reader:
    (cets,db) = row[0:2]
    if not db.isnumeric():
        continue # could be header or something not in the API

    if int(db) == 11 or int(db) == 57:
        continue    # archived databases

    wb.db = db
    try:
        print('Fetching {} ({})'.format(cets, db), file=sys.stderr)

        for elem in wb.data.fetch(cets, time=time_range, skipAggs=True, labels=True):
            if elem['economy']['id'] in economy_filter:
                # writer.writerow([db, elem['series'], elem['economy'], elem['time'], elem['value']])
                writer.writerow([db, elem['economy']['id'], elem['time']['value'], elem['value'], elem['series']['id'], elem['series']['value'], elem['economy']['value']])
    except wb.APIError as err:
        print('ERROR {} ({})'.format(cets, db), file=sys.stderr)
        print(err, file=sys.stderr)
