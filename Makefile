generate:

	csi -s generator/cv.scm && wkhtmltopdf -T 10 cv-for-pdf.html cv.pdf

install-deps:

	chicken-install -s sxml-transforms
