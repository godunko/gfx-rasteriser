
all: build run

build:
	alr build

run:
	rm -rf output
	mkdir output
	(cd output && ../bin/radial_lines)
	diff -u source/radial_lines.ppm output/radial_lines.ppm
