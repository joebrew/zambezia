#!/usr/bin/env python

import sys
import urllib
import httplib

# example usage: ./export_records.py token=<token> content=record format=csv type=flat record_id=clip_id
# content=record&format=csv&type=flat   fields=
# we will have only, token=???? record_id=????? output=??????
# Set the url and path to the API
host = 'ssl.manhica.net'
path = '/clipredcap/api/'
pieces = 1000

def download_ids(token, record_id_field):
    list = []
    paramx = {'content': 'record', 'format': 'csv', 'type': 'flat', 'token': token, 'fields': record_id_field}

    c = httplib.HTTPSConnection(host)
    c.request('POST', path, urllib.urlencode(paramx), {'Content-Type': 'application/x-www-form-urlencoded'})
    r = c.getresponse()
    s = r.read()

    for line in s.split('\n'):
        v = line.split(',')
        if v[0] not in list:
            list.append(v[0])
        #print(v[0])
    c.close()

    list.pop(0)

    return list


def download_all_parts(token, output_file, idlist):
    ids = ''
    count = 0
    added = 0
    left = len(idlist)
    file = open(output_file, 'w')

    for record_id in idlist:
        count += 1
        if len(record_id) > 0:
            ids = record_id if ids == '' else ids + ',' + record_id
            added += 1

        if (count % pieces == 0) or (count==len(idlist)):
            # download from redcap and clean buffer
            left = left - added
            print "downloading %d records still have %d records to download" % (added, left)
            params = {'content': 'record', 'format': 'csv', 'type': 'flat', 'token': token, 'records': ids}
            c = httplib.HTTPSConnection(host)
            c.request('POST', path, urllib.urlencode(params), {'Content-Type': 'application/x-www-form-urlencoded'})
            r = c.getresponse()
            #print r.read()
            file.write(r.read())
            ids = ''
            added = 0
            #print 'adding more records' if (count<len(idlist)) else ''
    file.close()
    return

def main():
    # reading general params
    record_id_field = ''
    output_file = ''
    token = ''

    for arg in sys.argv[1:]:
        k, v = arg.split('=')
        if k == 'record_id':
            record_id_field = v
        if k == 'output':
            output_file = v
        if k == 'token':
            token = v
        if k == 'pieces':
            pieces = int(v)

    if len(sys.argv[1:])==0:
        print "no parameters inserted, exiting"
        exit()

    print "downloading from https://%s%s with apikey=%s, record_id_field=%s to file=%s" % (host, path, token, record_id_field, output_file)
    print "downloading list of record_ids, field=%s" % (record_id_field)
    list = download_ids(token, record_id_field)
    print "downloaded %d record_ids" % (len(list))
    download_all_parts(token, output_file, list)
    print "\nFinished downloading"


if __name__ == '__main__':
    main()