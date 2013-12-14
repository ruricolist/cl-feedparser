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

def simplify_entry(e):
    e2 = {key: e.get(key) for key in item_keys}
    e2['link'] = e.get('feedburner_origlink') or e.get('link')
    pubdate = e.get('updated_parsed') or e.get('published_parsed')
    if struct_time_p(pubdate):
        e2['pubdate'] = ts_to_universal(pubdate)
    return e2

def simplify_feed (d):
    d2 = {key: d.feed.get(key) for key in feed_keys}
    d2['version'] = d['version'];
    pubdate = d.feed.get('updated_parsed') or d.feed.get('published_parsed')
    if struct_time_p(pubdate):
        d2['pubdate'] = ts_to_universal(pubdate)
    d2['entries'] = [simplify_entry(e) for e in d.entries[:10]]
    return d2

def parse_stdin_feed ():
    text = sys.stdin.read()
    d = feedparser.parse(text)
    d2 = simplify_feed(d)
    json.dump(d2, sys.stdout)
