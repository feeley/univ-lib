GSC=../gsc/gsc -:=.. -prelude '(include "lib/lib.scm")'

all: js

all2: js php python ruby

gsi:
	gsi app1.scm

js: app1.js
	time d8 app1.js

app1.js: app1.scm
	$(GSC) -c -target js app1.scm

php: app1.php
	time php app1.php

app1.php:
	$(GSC) -c -target php app1.scm

python: app1.py
	time python app1.py

app1.py:
	$(GSC) -c -target python app1.scm

ruby: app1.rb
	time ruby app1.rb

app1.rb:
	$(GSC) -c -target ruby app1.scm

clean:
	rm -f app1.js app1.php app1.py app1.rb *~ lib/*~

tar:
	rm -rf univ-lib
	mkdir univ-lib
	cp makefile univ-lib
	cp app1.scm univ-lib
	cp -r lib univ-lib
	COPYFILE_DISABLE=1 tar cf univ-lib.tar univ-lib
	rm -rf univ-lib
