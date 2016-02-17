include make.inc

all: 
	mkdir -p ./lib
	mkdir -p ./objs
	cd ./src; make run

.PHONY: all