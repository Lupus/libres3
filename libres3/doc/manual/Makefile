all: manual.pdf html/manual.html

manual.pdf: src/*.tex src/*.eps Makefile
	cd src && latexmk -output-directory=.. -pdf -gg -silent manual

html/manual.html: src/*.tex src/*.eps src/our.css Makefile
	cd src && htlatex manual.tex html,2,next,fn-in "" -d../html/
	cd src && htlatex manual.tex html,2,next,fn-in "" -d../html/
	cp src/our.css html/manual.css
	cp src/our.png html/manual0x.png
	sed -i 's/-_/-/g' html/manual*.html
