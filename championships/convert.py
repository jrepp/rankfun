#!/usr/bin/python

from __future__ import print_function

from os import listdir
from os.path import isfile, join
from fractions import Fraction

import chess.pgn
import hashlib

def summarize(game):
    d = {}
    d.update(game.headers)

    r = game.headers.get("Result", "0-0")
    rparts = r.split('-')

    d["WhiteNameHash"] = hashlib.sha1(d["White"]).hexdigest()[:8]
    d["BlackNameHash"] = hashlib.sha1(d["Black"]).hexdigest()[:8]
    d["Result1"] = float(Fraction(rparts[0]))
    d["Result2"] = float(Fraction(rparts[1]))

    if not d.get("BlackElo"):
        d["BlackElo"] = 1400
    if not d.get("WhiteElo"):
        d["BlackElo"] = 1400

    return d

def convert(filename):
    with open(filename, 'r') as pgn:
        while True:
            g = chess.pgn.read_game(pgn)
            if g is None:
                break;
            d = summarize(g)
    
            # p1,p2,result1,result2,rank1,rank2,moves,date,round
            print("{WhiteNameHash},{BlackNameHash},{Result1},{Result2},{WhiteElo},{BlackElo},{Date},{Round}".format(**d))

def header():
    print("White,Black,WhiteResult,BlackResult,WhiteRank,BlackRank,Date,Round")

def allfiles(dirname):
    header()
    onlyfiles = [ f for f in listdir(dirname) 
            if (isfile(join(dirname, f)) and f.endswith('pgn')) ]
    for f in onlyfiles:
        convert(f)

if __name__ == '__main__':
    allfiles('.')
