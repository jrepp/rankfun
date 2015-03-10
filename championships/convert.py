#!/usr/bin/python

from __future__ import print_function

from os import listdir
from os.path import isfile, join
from fractions import Fraction

import chess.pgn
import hashlib
import re

def normalize_name(n):
    tokens = re.sub('\W', ' ', n.lower()).split()
    return ' '.join(tokens)

def summarize(game):
    d = {}
    d.update(game.headers)

    r = game.headers.get("Result", "0-0")
    rparts = r.split('-')
    
    white_norm = normalize_name(d["White"])
    black_norm = normalize_name(d["Black"])

    d["WhiteName"] = white_norm
    d["BlackName"] = black_norm

    d["WhiteNameHash"] = hashlib.sha1(white_norm).hexdigest()[:8]
    d["BlackNameHash"] = hashlib.sha1(black_norm).hexdigest()[:8]

    d["Result1"] = float(Fraction(rparts[0]))
    d["Result2"] = float(Fraction(rparts[1]))

    if not d.get("BlackElo"):
        d["BlackElo"] = 0

    if not d.get("WhiteElo"):
        d["WhiteElo"] = 0

    return d

def convert_games(filename):
    with open(filename, 'r') as pgn:
        while True:
            g = chess.pgn.read_game(pgn)
            if g is None:
                break;
            d = summarize(g)
    
            # p1,p2,result1,result2,rank1,rank2,moves,date,round
            print("{WhiteName},{WhiteNameHash},{BlackName},{BlackNameHash},{Result1},{Result2},{WhiteElo},{BlackElo},{Date},{Round}".format(**d))

def games_header():
    print("WhiteName,WhiteNameHash,BlackName,BlackNameHash,WhiteResult,BlackResult,WhiteRank,BlackRank,Date,Round")

def each_pgn(dirname, process):
    onlyfiles = [ f for f in listdir(dirname) 
            if (isfile(join(dirname, f)) and f.endswith('pgn')) ]
    for filename in onlyfiles:
        process(filename)

if __name__ == '__main__':
    games_header()
    each_pgn('.', convert_games)
