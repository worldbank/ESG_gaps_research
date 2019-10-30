#!/usr/bin/python -u

#wbgapi module available here: https://github.com/tgherzog/wbgapi

"""
coverage.py produces quick density coverage stats for an indicator

Usage:
  coverage.py [--verbose --sources --prefixes --bssi --gaps] [--start YEAR] [--income INC] [--region RGN] [--since YEARS] [--archived-dbs IDS] INDICATOR...

Options:
  --verbose, -v       detailed output
  --prefixes, -p      add a prefix column to output
  --start, -s YEAR    start year [default: 2000]
  --since YEARS       comma-separated list of years for coverage analysis [default: 2010,2014,2016,2018]
  --archived-dbs IDS  comma-separated list of database IDs to treat as archived: don't analyze these [default: 11,57]
  --income INC        only this income group (~ to exclude)
  --region RGN        only this region (~ to exclude)
  --bssi              only BSSI countries
  --gaps              include gaps analysis
  --sources           include indicator sources

INDICATOR can be in the form CETS or SOURCE:CETS. If omitted, SOURCE defaults to 2

"""

import requests
import datetime
import sys
import os
import re
import csv
import copy
import numpy
import wbgapi
from docopt import docopt

reload(sys)
sys.setdefaultencoding('utf-8')

bssi_countries = ['ARG', 'AUS', 'AUT', 'BEL', 'BRA', 'CAN', 'CHL', 'CHN', 'COL', 'HRV',
                  'CZE', 'DNK', 'DOM', 'ECU', 'EGY', 'FIN', 'FRA', 'DEU', 'GRC', 'HUN',
                  'IND', 'IDN', 'IRL', 'ISR', 'ITA', 'JPN', 'KAZ', 'LBN', 'LTU', 'MYS',
                  'MEX', 'NLD', 'NZL', 'NGA', 'NOR', 'PAN', 'PER', 'PHL', 'POL', 'PRT',
                  'ROU', 'RUS', 'SRB', 'SGP', 'SVK', 'SVN', 'ZAF', 'KOR', 'ESP', 'LKA',
                  'SWE', 'CHE', 'THA', 'TUR', 'USA', 'UKR', 'GBR', 'URY', 'VEN', 'TWN']

config = docopt(__doc__)

minYear = int(config['--start'])
maxYear = datetime.datetime.now().year - 1
actualMaxYear = None

incomeFlag = regionFlag = True
if config['--income'] and config['--income'][0] == '~':
    config['--income'] = config['--income'][1:]
    incomeFlag = False

if config['--region'] and config['--region'][0] == '~':
    config['--region'] = config['--region'][1:]
    regionFlag = False

archiveDBs = [int(i) for i in config['--archived-dbs'].split(',')]

yearKeys = [int(i) for i in config['--since'].split(',')]
_yearBreaks = {}
for i in yearKeys:
    _yearBreaks[i] = 0
    
  
# sanity checks
if len(config['INDICATOR']) > 1:
    config['--verbose'] = False

# get populations
_pops = {}
for row in wbgapi.fetch('https://api.worldbank.org/v2/en/country/all/indicator/SP.POP.TOTL', {'MRNEV': 1}):
    if row['countryiso3code']:
        _pops[row['countryiso3code']] = row['value']

# Then fetch the the country list
_countries = {}
countOfSmallCountries = 0
countOfRichCountries = 0
country_meta = {}
for elem in wbgapi.fetch('https://api.worldbank.org/v2/en/country'):
    if config['--bssi'] and elem['id'] not in bssi_countries:
        continue

    if config['--income'] and (elem['incomeLevel']['id'] == config['--income']) != incomeFlag:
        continue
        
    if config['--region'] and (elem['region']['id'] == config['--region']) != incomeFlag:
        continue
        
    if elem['region']['id'] != 'NA' and elem['id'] != 'TWN':
        _countries[elem['id']] = [0] * (maxYear-minYear+1)
        pop = _pops.get(elem['id'])
        meta = {
          'pop': pop,
          'income': elem['incomeLevel']['id'],
          'region': elem['region']['id'],
          'smallCountry': pop and pop < 100000,
          'richCountry': elem['incomeLevel']['id'] == 'HIC',
        }

        if meta['smallCountry']: countOfSmallCountries += 1
        if meta['richCountry']: countOfRichCountries += 1

        country_meta[elem['id']] = meta

writer = csv.writer(sys.stdout, quoting=csv.QUOTE_MINIMAL)
output = ['DB', 'CETS', 'NAME', 'MINMRV', 'AVGMRV', 'MEDMRV', 'MAXMRV', 'COUNTRIES', 'TOTAL_COUNTRIES', 'MINCOV', 'MAXCOV', 'AVGCOV','COVSCORE']

if config['--sources']:
    output.insert(3, 'SOURCE')

if config['--prefixes']:
    output.insert(0, 'PREFIX')

for i in yearKeys:
    output.append('SINCE{}'.format(i))

if config['--gaps']:
    output.extend(['GAPS_TOTAL', 'GAPS_SMALL n={}'.format(countOfSmallCountries), 'GAPS_SMALLPERC', 'GAPS_RICH n={}'.format(countOfRichCountries), 'GAPS_RICHPERC', 'GAPS_OTHER'])

writer.writerow(output)

for id in config['INDICATOR']:
    minYear = int(config['--start'])
    maxYear = datetime.datetime.now().year - 1
    actualMaxYear = None
    minMRV = None
    countries = copy.deepcopy(_countries)
    yearBreaks = copy.deepcopy(_yearBreaks)

    parts = id.split(',', 1)
    if len(parts) > 1:
        (prefix,parts) = (parts[0], parts[1])
    else:
        prefix = ''
        parts = parts[0]

    parts = parts.split(':')
    if len(parts) > 1:
        (src,cets) = (parts[0],parts[1])
    else:
        (src,cets) = (2, parts[0])

    if config['--sources']:
        source = 'n/a'
        try:
            url = 'https://api.worldbank.org/v2/en/indicator/{}?source={}&format=json'.format(cets, src)
            response = requests.get(url)
            data = response.json()
            source = data[1][0]['sourceOrganization']
        except:
            pass

    localDataPath = os.path.sep.join([os.path.dirname(os.path.realpath(sys.argv[0])), 'local', '{}.csv'.format(cets)])
    data = [{}] # minimal viable object

    if os.path.isfile(localDataPath):
        with open(localDataPath) as localFile:
            localReader = csv.reader(localFile)
            localReader.next() # read and toss the header: we assume SERIES_NAME,COUNTRY_ISO,DATE,VALUE
            data = [{}, []]    # new minimal viable object we can iterate over
            src = ''
            for row in localReader:
                if len(row) >= 4 and row[0] and row[1] and row[2]:
                    data[1].append({
                        'indicator': {'id': cets, 'value': row[0]},
                        'countryiso3code': row[1],
                        'date': row[2],
                        'value': row[3] # no type conversion, but that's okay
                    })
    else:
        # sanity check: API calls for WGI data fail if minYear<1996
        minYearApi = minYear if (minYear >= 1996 or int(src) != 3) else 1996
        url = 'https://api.worldbank.org/v2/en/country/all/indicator/{}?source={}&format=json&per_page=20000&date={}:{}'.format(cets, src, minYearApi, maxYear)
        # print url

        response = requests.get(url)
        data = response.json()

    if len(data) < 2 or src in archiveDBs:
        output = [src, cets]
        if config['--prefixes']:
            output.insert(0, prefix)

        writer.writerow(output)
        continue

    data = data[1]
    for elem in data:
        cets_name = elem['indicator']['value']
        date = int(elem['date'])
        iso3 = elem['countryiso3code']
        if len(iso3) == 0 and len(elem['country']['id']) == 3:
            iso3 = elem['country']['id']

        if countries.get(iso3) and date >= minYear and date <= maxYear and elem['value'] is not None:
            actualMaxYear = date if actualMaxYear is None else max(actualMaxYear, date)
            offset = date - minYear
            countries[iso3][offset] = date

    allCoverage = [] # a country array of 'coverage' values: % of possible values in the study range (0 means no values, 100 means complete coverage)
    countriesWithData = 0
    coverageScore = 0.0
    missingCountries = 0
    missingSmallCountries = 0
    missingRichCountries = 0
    missingOtherCountries = 0
    mrvYears = []
    if actualMaxYear:
        for k,elem in countries.iteritems():
            coverage = sum([1 if i else 0 for i in elem]) * 100 / (actualMaxYear-minYear+1)
            coverageScore += max(max(elem)-minYear, 0)
            if sum(elem) > 0:
                countriesWithData += 1
                mrvYears.append(max(elem))
                minMRV = min(minMRV,max(elem)) if minMRV else max(elem)
                for i in yearKeys:
                    if len([x for x in elem if x >= i]) > 0:
                        yearBreaks[i] += 1
            else:
                missingCountries += 1
                meta = country_meta[k]
                isClassified = False
                # if k in ['ASM', 'GUM', 'VIR', 'PRI', 'CHI', 'INM']:
                #    missingTerritories += 1
                if meta['smallCountry']:
                    missingSmallCountries += 1
                    isClassified = True

                if meta['richCountry']:
                    missingRichCountries += 1
                    isClassified = True

                if not isClassified:
                    missingOtherCountries += 1

            allCoverage.append(coverage)


    coverageScore /= len(countries) * (maxYear-minYear)
    coverageScore = round(coverageScore*100, 1)
    if len(mrvYears) == 0:
        mrvYears = [0]    # sanity check: shouldn't happen

    if len(allCoverage) > 0:
        output = [src, cets, cets_name, min(mrvYears), int(round(numpy.average(mrvYears))), int(numpy.median(mrvYears)), max(mrvYears), countriesWithData, len(countries),
            int(round(min(allCoverage))),
            int(round(max(allCoverage))),
            int(round(sum(allCoverage)/len(allCoverage))), coverageScore
            ]

        for i in yearKeys:
            output.append(yearBreaks[i])

        if config['--sources']:
            output.insert(3, source)

        if config['--prefixes']:
            output.insert(0, prefix)

        if config['--gaps']:
            output.extend([len(countries)-countriesWithData, missingSmallCountries, round(float(missingSmallCountries)/countOfSmallCountries, 2), missingRichCountries, round(float(missingRichCountries)/countOfRichCountries, 2), missingOtherCountries])

        writer.writerow(output)
    else:
        output = [src, cets, cets_name]
        if config['--prefixes']:
            output.insert(0, prefix)

        writer.writerow(output)

    if config['--verbose']:
        for k,elem in countries.iteritems():
            coverage = sum([1 if i else 0 for i in elem]) * 100 / (actualMaxYear-minYear+1)
            if sum(elem) > 0:
                minMRV = min([i for i in elem if i > 0])
                maxMRV = max(elem)
            else:
                minMRV = maxMRV = ''

            writer.writerow([k, minMRV, maxMRV, coverage])
