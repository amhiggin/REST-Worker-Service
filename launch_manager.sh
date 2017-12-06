#!/bin/sh

# first install dependencies
pip install -r requirements.txt

# then launch manager: $1 is the number of workers
python Manager.py $1