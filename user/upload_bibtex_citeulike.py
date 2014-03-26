
#!python
import pickle, requests, sys

# reload cookies
with open('c:/Users/jkitchin/Dropbox/blogofile-jkitchin.github.com/_blog/cookies.pckl', 'rb') as f:
    cookies = pickle.load(f)

url = 'http://www.citeulike.org/profile/jkitchin/import_do'

bibtex = sys.stdin.read()

data = {'pasted':bibtex,
        'to_read':2,
        'tag_parsing':'simple',
        'strip_brackets':'no',
        'update_id':'bib-key',
        'btn_bibtex':'Import BibTeX file ...'}

headers = {'content-type': 'multipart/form-data',
           'User-Agent':'jkitchin/johnrkitchin@gmail.com bibtexupload'}

r = requests.post(url, headers=headers, data=data, cookies=cookies, files={})
print r
