all: po/kata.pot po/en@lolcat.po 

dist :: all

clean:
	rm -f po/kata.pot po/en@lolcat.po 

po/kata.pot: Kata/Config.hs Main.hs
	xgettext -C --package-name=kata --package-version=0.1 --copyright-holder="Edward Kmett" --msgid-bugs-address=ekmett@gmail.com -o po/kata.pot -s Kata/Config.hs Main.hs

po/en@lolcat.po: po/kata.pot
	msginit -l en_US@lolcat -i po/kata.pot --no-translator -o - | msgfilter --input=- --output-file=po/en@lolcat.po bin/lolcat.pl

#po/kata.mo: po/en@lolcat.po
#	msgfmt -c -v -o po/kata.mo po/en@lolcat.po

.PHONY: clean all
