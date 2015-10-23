all: story.html story.pdf

story.html: story.md
	pandoc -f markdown -t html5 -o story.html story.md

story.pdf: story.md
	pandoc -f markdown -t latex -o story.pdf story.md
