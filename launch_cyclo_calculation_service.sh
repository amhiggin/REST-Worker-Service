#!/bin/sh

# first install dependencies
pip install -r requirements.txt

# launch manager: $1 is the number of workers

print 'Launching Manager node'
python Manager.py $1

for i in $( seq 2 $1 )
do
	print 'Launching worker i'
	python Worker.py &
done
print 'All launched'
python Worker.py