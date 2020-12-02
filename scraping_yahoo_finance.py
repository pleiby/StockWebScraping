# How to scrape Yahoo Finance and extract stock market data using Python & LXML
# Yahoo Finance is a good source for extracting financial data, be it – stock market data, trading prices or business related news.

# In this tutorial, we will extract the trading summary for a public company from Yahoo Finance ( like http://finance.yahoo.com/quote/AAPL?p=AAPL ). We’ll be extracting the following fields for this tutorial.

# Previous Close
# Open
# Bid
# Ask
# Day’s Range
# 52 Week Range
# Volume
# Average Volume
# Market Cap
# Beta
# PE Ratio
# EPS
# Earning’s Date
# Dividend & Yield
# Ex-Dividend Date
# 1yr Target EST

# Source: https://www.scrapehero.com/scrape-yahoo-finance-stock-market-data/

# Scraping Logic

# 1. Construct the URL of the search results page from Yahoo Finance. For example, here is the one for Apple-http://finance.yahoo.com/quote/AAPL?p=AAPL
# 2. Download HTML of the search result page using Python Requests
# 3. Parse the page using LXML – LXML lets you navigate the HTML Tree Structure using Xpaths. We have predefined the XPaths for the details we need in the code.
# 4. Save the data to a JSON file.
# 
# Requirements
# For this web scraping tutorial using Python, we will need spackages for downloading and parsing the HTML.
# Python 2.7 ( https://www.python.org/downloads/ )
# PIP to install the  following packages in Python ( https://pip.pypa.io/en/stable/installing/)
# Python Requests, to make requests and download the HTML content of the pages
#   ( http://docs.python-requests.org/en/master/user/install/).
# Python LXML, for parsing the HTML Tree Structure using Xpaths
#   ( Learn how to install that here – http://lxml.de/installation.html )

from lxml import html  
import requests
from exceptions import ValueError
from time import sleep
import json
import argparse
from collections import OrderedDict
from time import sleep
def parse(ticker):
	url = "http://finance.yahoo.com/quote/%s?p=%s"%(ticker,ticker)
	response = requests.get(url)
	print "Parsing %s"%(url)
	sleep(4)
	parser = html.fromstring(response.text)
	summary_table = parser.xpath('//div[contains(@data-test,"summary-table")]//tr')
	summary_data = OrderedDict()
	other_details_json_link = "https://query2.finance.yahoo.com/v10/finance/quoteSummary/{0}?formatted=true&lang=en-US&region=US&modules=summaryProfile%2CfinancialData%2CrecommendationTrend%2CupgradeDowngradeHistory%2Cearnings%2CdefaultKeyStatistics%2CcalendarEvents&corsDomain=finance.yahoo.com".format(ticker)
	summary_json_response = requests.get(other_details_json_link)
	json_loaded_summary =  json.loads(summary_json_response.text)
	y_Target_Est = json_loaded_summary["quoteSummary"]["result"][0]["financialData"]["targetMeanPrice"]['raw']
	earnings_list = json_loaded_summary["quoteSummary"]["result"][0]["calendarEvents"]['earnings']
	eps = json_loaded_summary["quoteSummary"]["result"][0]["defaultKeyStatistics"]["trailingEps"]['raw']
	datelist = []
	for i in earnings_list['earningsDate']:
		datelist.append(i['fmt'])
	earnings_date = ' to '.join(datelist)
	for table_data in summary_table:
		raw_table_key = table_data.xpath('.//td[@class="C(black)"]//text()')
		raw_table_value = table_data.xpath('.//td[contains(@class,"Ta(end)")]//text()')
		table_key = ''.join(raw_table_key).strip()
		table_value = ''.join(raw_table_value).strip()
		summary_data.update({table_key:table_value})
	summary_data.update({'1y Target Est':y_Target_Est,'EPS (TTM)':eps,'Earnings Date':earnings_date,'ticker':ticker,'url':url})
	return summary_data

if __name__=="__main__":
	argparser = argparse.ArgumentParser()
	argparser.add_argument('ticker',help = '')
	args = argparser.parse_args()
	ticker = args.ticker
	print "Fetching data for %s"%(ticker)
	scraped_data = parse(ticker)
	print "Writing data to output file"
	with open('%s-summary.json'%(ticker),'w') as fp:
	 	json.dump(scraped_data,fp,indent = 4)