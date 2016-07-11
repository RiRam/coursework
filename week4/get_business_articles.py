#
# file: get_business_articles.py
#
# description: fetches 1000 Business and World articles from NYTimes
#
# usage: get_business_articles.py <api_key>
#
# requirements: a NYTimes API key
#   available at https://developer.nytimes.com/signup
#

import requests
import json
import sys

ARTICLE_SEARCH_URL = 'https://api.nytimes.com/svc/search/v2/articlesearch.json'

if __name__=='__main__':
    if len(sys.argv) != 3:
        sys.stderr.write('usage: %s <api_key> <section_name>\n' % sys.argv[0])
        sys.exit(1)
    
    api_key = sys.argv[1]
    section_name = sys.argv[2]
    
    for p in range(0,100):
        params = {'api-key': api_key,
            'fq': section_name,
            'sort': "newest",
            'fl': "section_name,web_url,snippet,pub_date",
            'page': p
        }

        r = requests.get(ARTICLE_SEARCH_URL, params)        
        
        if(sys.version_info > 3):
            data = r.json()
        
            for doc in data['response']['docs']:
                print (doc['web_url'] + doc['section_name'] + doc['pub_date'] + doc['snippet'].replace('\n', ""))
        else:
            data = json.loads(r.content)
        
            for doc in data['response']['docs']:
                print doc['web_url'] + doc['section_name'] + doc['pub_date'] + doc['snippet'].replace('\n', "")