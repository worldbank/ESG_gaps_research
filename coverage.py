#!/usr/bin/python -u

"""
coverage.py produces quick density coverage stats for an indicator

Usage:
  coverage.py [--verbose --sources --prefixes --bssi] [--start YEAR] [--income INC] [--region RGN] [--since YEARS] INDICATOR...

Options:
  --verbose, -v       detailed output
  --prefixes, -p      add a prefix column to output
  --start, -s YEAR    start year [default: 2000]
  --since YEARS       comma-separated list of years for coverage analysis [default: 2010,2014,2016,2018]
  --income INC        only this income group (~ to exclude)
  --region RGN        only this region (~ to exclude)
  --bssi              only BSSI countries
  --sources           include indicator sources

INDICATOR can be in the form CETS or SOURCE:CETS. If omitted, SOURCE defaults to 2

"""

import requests
import datetime
import sys
import csv
import copy
import numpy
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

yearKeys = [int(i) for i in config['--since'].split(',')]
_yearBreaks = {}
for i in yearKeys:
    _yearBreaks[i] = 0
    
  
# sanity checks
if len(config['INDICATOR']) > 1:
    config['--verbose'] = False

# start by fetching the country list
url = 'https://api.worldbank.org/v2/en/country?format=json&per_page=20000'
response = requests.get(url)
data = response.json()
data = data[1]
_countries = {}
for elem in data:
    if config['--bssi'] and elem['id'] not in bssi_countries:
        continue

    if config['--income'] and (elem['incomeLevel']['id'] == config['--income']) != incomeFlag:
        continue
        
    if config['--region'] and (elem['region']['id'] == config['--region']) != incomeFlag:
        continue
        
    if elem['region']['id'] != 'NA' and elem['id'] != 'TWN':
        _countries[elem['id']] = [0] * (maxYear-minYear+1)

writer = csv.writer(sys.stdout, quoting=csv.QUOTE_MINIMAL)
output = ['CETS', 'NAME', 'MINMRV', 'AVGMRV', 'MEDMRV', 'MAXMRV', 'COUNTRIES', 'TOTAL_COUNTRIES', 'MINCOV', 'MAXCOV', 'AVGCOV','COVSCORE']

if config['--sources']:
    output.insert(2, 'SOURCE')

if config['--prefixes']:
    output.insert(0, 'PREFIX')

for i in yearKeys:
    output.append('SINCE{}'.format(i))

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

    # sanity check: API calls for WGI data fail if minYear<1996
    minYearApi = minYear if (minYear >= 1996 or int(src) != 3) else 1996
    url = 'https://api.worldbank.org/v2/en/country/all/indicator/{}?source={}&format=json&per_page=20000&date={}:{}'.format(cets, src, minYearApi, maxYear)
    # print url

    response = requests.get(url)
    data = response.json()

    if len(data) < 2:
        print ""
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

            allCoverage.append(coverage)


    coverageScore /= len(countries) * (maxYear-minYear)
    coverageScore = round(coverageScore*100, 1)
    if len(mrvYears) == 0:
        mrvYears = [0]    # sanity check: shouldn't happen

    if len(allCoverage) > 0:
        output = [cets, cets_name, min(mrvYears), int(round(numpy.average(mrvYears))), int(numpy.median(mrvYears)), max(mrvYears), countriesWithData, len(countries),
            int(round(min(allCoverage))),
            int(round(max(allCoverage))),
            int(round(sum(allCoverage)/len(allCoverage))), coverageScore
            ]

        for i in yearKeys:
            output.append(yearBreaks[i])

        if config['--sources']:
            output.insert(2, source)

        if config['--prefixes']:
            output.insert(0, prefix)

        writer.writerow(output)
    else:
        output = [cets, cets_name]
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
