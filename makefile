GSC=../gsc/gsc -:=.. -prelude '(include "lib/lib.scm")'

all: js

all2: js php python ruby

gsi:
	gsi app5.scm

js: app5.js
	time d8 app5.js

app5.js: app5.scm
	$(GSC) -c -target js app5.scm

php: app5.php
	time php app5.php

app5.php:
	$(GSC) -c -target php app5.scm

python: app5.py
	time python app5.py

app5.py:
	$(GSC) -c -target python app5.scm

ruby: app5.rb
	time ruby app5.rb

app5.rb:
	$(GSC) -c -target ruby app5.scm

clean:
	rm -f app5.js app5.php app5.py app5.rb *~ lib/*~

tar:
	rm -rf univ-lib
	mkdir univ-lib
	cp makefile univ-lib
	cp app5.scm univ-lib
	cp -r lib univ-lib
	COPYFILE_DISABLE=1 tar cf univ-lib.tar univ-lib
	rm -rf univ-lib
