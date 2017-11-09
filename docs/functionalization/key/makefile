all: \
	page-1-crop.pdf \
	page-2-crop.pdf

page-%-full.pdf: Drawings.pdf
	pdftk $< cat $*-$* output $@

page-%-crop.pdf: page-%-full.pdf
	pdfcrop $<
	mv page-$*-full-crop.pdf page-$*-crop.pdf

clean:
	rm -f page-*
