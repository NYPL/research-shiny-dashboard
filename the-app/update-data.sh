#!/bin/bash

rsync -Phav ../making-the-data/make-it-all/*.dat ./data/;
rsync -Phav ../making-the-data/made-elsewhere/* ./data/;
cp ../making-the-data/stolen-data/visits-by-quarter* ./data/;
