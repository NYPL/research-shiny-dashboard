#!/bin/bash

rsync -Phav ../making-the-data/make-it-all/target/*.dat ./data/;
rsync -Phav ../making-the-data/not-updated/* ./data/;
