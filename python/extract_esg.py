
# this script uses the WB API to extract all observations for the study period
# Usage: python extract_esg.py FILE

import csv
import wbgapi as wb
import sys

reader = csv.reader(open(sys.argv[1], 'r'))
writer = csv.writer(sys.stdout)

wdi_countries = [row['id'] for row in wb.economy.list(skipAggs=True)]

writer.writerow(['db', 'series', 'economy', 'time', 'value'])

for row in reader:
    (cets,db) = row[0:2]
    if not db.isnumeric():
        continue # could be header or something not in the API

    if int(db) == 11 or int(db) == 57:
        continue    # archived databases

    wb.db = db
    try:
        print('Fetching {} ({})'.format(cets, db), file=sys.stderr)

        for elem in wb.data.fetch(cets, time=range(1990, 2020), skipAggs=True):
            if elem['economy'] in wdi_countries:
                writer.writerow([db, elem['series'], elem['economy'], elem['time'], elem['value']])
    except wb.APIError as err:
        print('ERROR {} ({})'.format(cets, db), file=sys.stderr)
