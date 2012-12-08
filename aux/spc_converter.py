#!/usr/bin/python2.7
# -*- coding: utf-8 -*-

from sqlalchemy import MetaData,Column,Table,String,Float,create_engine
from sqlalchemy.orm import sessionmaker

from argparse import ArgumentParser
from os import getcwdu
from os.path import getmtime
from time import localtime
from sys import exit

from struct import unpack

def initSqlEngine(strDbFileName):

    '''
    Assume all filepathes is relative
    '''
    return create_engine('sqlite:///%s' % (strDbFileName,))

def initTable(sqlalchemyEngine):
    md = MetaData()

    tbl = Table('tblMeasurements',md
                    ,Column('file',String,primary_key=True)
                    ,Column('lambda',Float,primary_key=True)
                    ,Column('value'))

    md.create_all(engn)

def getArgParser():

    argParser = ArgumentParser()

    argParser.add_argument('-s','--source',dest='sourceFile',type=str,required=True,
                           help='Source specter file (*.fss) for convert',metavar='SRCFILE')

    argParser.add_argument('-o','--output',dest='outputFile',default='asSrc',type=str,
                           help='Result sqlite database file (*.sqlite). If not present is SRCFILE.sqlite',metavar='DSTFILE')

    argParser.add_argument('-d','--date',dest='dateOfMeasurement',default='asSrc',type=str,
                           help='Date of measurement in "DD/MM/YY. If not present if SRCFILE creation date" format',metavar='SRCDATE')

    return argParser

def getFileStruct(srcFile):

    '''
    params that define *.fss file header data binary format
              ┏ padding beginning mark
              ┃   ┏ datetime in delphi format (float64 where integer part is day count
              ┃   ┃ since 30.12.1899 and mantiss is daypart)
              ┃   ┃     ┏ comment as simple text
              ┃   ┃     ┃     ┏ count of measurements as 4 byte integer
              ┃   ┃     ┃     ┃     ┏ smokie constant
              ┃   ┃     ┃     ┃     ┃     ┏ wave length measurement units as text
              ┃   ┃     ┃     ┃     ┃     ┃     ┏ fluo. ex. measurement units as text
              ┃   ┃     ┃     ┃     ┃     ┃     ┃     ┏ header ending mark
              ┃   ┃     ┃     ┃     ┃     ┃     ┃     ┃                                 '''
    fMarks = (0x0,0x218,0x220,0x2af,0x2b3,0x2b7,0x2bf,0x2c8)
    tSeq   = (  'x'  ,'d'  ,'s'  ,'i'  ,'i'  ,'s'  ,'s')
    tLens  = {'x':1,'s':1,'i':4,'d':8}

    '''
    prepare for reading
    '''
    r = srcFile
    '''
    after zip:
    (0x0 ,|0x218 ,0x220 ,0x2af ,0x2b3 ,0x2b7 ,0x2c0 ,0x2c8 |      )
    (     |0x0   ,0x218 ,0x220 ,0x2af ,0x2b3 ,0x2b7 ,0x2c0 |,0x2c8)
    (     |x     ,d     ,s     ,i     ,i     ,s     ,s     |      )
    '''
    fLens  = tuple([(i-j)/k for i,j,k in zip(fMarks[1:],fMarks[:-1],[tLens[l] for l in tSeq])])
    fStr   = ''.join(['%%i%s' % (s,) for s in tSeq])

    '''
    get and decode the our data
    '''
    structHead = tuple([apply(lambda x: x.decode('cp1251') if type(x) == str else x,(i,)) for i in (unpack(fStr % fLens,r[:fMarks[-1] + 1]))])

    '''
    equivalent for previous expression
    '''
    '''
    structHead = list(unpack(fStr % fLens,r[:fMarks[-1] + 1]))
    for i in xrange(len(structHead)):
        if type(structHead[i]) == str:
            structHead[i] = structHead[i].decode('cp1251')

    structHead = tuple(structHead)
    '''

    '''
    in structHead second element is 0x2b0:0x2b3 unpacked integer, that's equal len of diapasone
    '''
    fStr = '%if%if' % (structHead[2],structHead[2])
    lTmp = unpack(fStr,r[0x141c:])
    
    '''
    not need to set first slice index, because zip return shortest sequence and its exact half
    '''
    structBody = zip(lTmp,lTmp[len(lTmp)/2:])

    return (structHead,structBody)

if __name__ == '__main__':

    args = getArgParser().parse_known_args()
    srcFileName = '%s/%s' % (getcwdu(),args[0].sourceFile.decode('utf-8')) if not args[0].sourceFile[0] == '/' else args[0].sourceFile.decode('utf-8')
    dstFileName = '%s.db' % (srcFileName if args[0].outputFile == 'asSrc' else args[0].outputFile.decode('utf-8'))
    fileTime = localtime(getmtime(srcFileName))

    '''
    decoding sources
    '''
    try:
        f = open(srcFileName,'rb')
        binSrc = f.read()
    except Exception as e:
        print(e)
        exit()
    finally:
        f.close()

    lHead,lBody = getFileStruct(binSrc)

    #print(lHead[0])
    #from pprint import pprint

    for i in lHead:
        print i

    '''
    encoding destination
    '''
