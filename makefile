GSC=../gsc/gsc -:=.. -prelude '(include "lib/lib.scm")'

all: js

all2: js php python ruby

gsi:
	gsi app.scm

js:
	$(GSC) -c -target js app.scm
	d8 app.js

php:
	$(GSC) -c -target php app.scm
	php app.php

python:
	$(GSC) -c -target python app.scm
	python app.py

ruby:
	$(GSC) -c -target ruby app.scm
	ruby app.rb

clean:
	rm -f app.js app.php app.py app.rb *~ lib/*~

tar:
	rm -rf univ-lib
	mkdir univ-lib
	cp makefile univ-lib
	cp app.scm univ-lib
	cp -r lib univ-lib
	COPYFILE_DISABLE=1 tar cf univ-lib.tar univ-lib
	rm -rf univ-lib
