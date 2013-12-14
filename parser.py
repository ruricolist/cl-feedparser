import feedparser
import json
import sys
import time

item_keys = ['title', 'title_detail', 'id', 'summary', 'author',
             'author_detail', 'content']
feed_keys = ['bozo', 'title', 'link', 'description',
             'language', 'title_detail', 'icon', 'links', 'ttl']

def epoch(ts):
    return int(time.mktime(ts))

universal_offset = 2208988800

def ts_to_universal (ts):
    return epoch(ts) + universal_offset

def struct_time_p(obj):
    return isinstance(obj, time.struct_time)

def transfer_date (e, e2, key):
    date = e[key]
    if struct_time_p(date):
        e2[key] = ts_to_universal(date)
    return e2

def simplify_entry(e):
    e2 = {key: e.get(key) for key in item_keys}
    e2['link'] = e.get('feedburner_origlink') or e.get('link')
    transfer_date(e, e2, 'updated_parsed')
    transfer_date(e, e2, 'published_parsed')
    return e2

def simplify_feed (d):
    d2 = {key: d.feed.get(key) for key in feed_keys}
    d2['version'] = d['version'];
    transfer_date(d, d2, 'updated_parsed')
    transfer_date(d, d2, 'published_parsed')
    d2['entries'] = [simplify_entry(e) for e in d.entries[:10]]
    return d2

def parse_stdin_feed ():
    text = sys.stdin.read()
    d = feedparser.parse(text)
    d2 = simplify_feed(d)
    json.dump(d2, sys.stdout)
