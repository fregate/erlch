SHELL := /bin/bash

CURDIR := $(shell pwd)
DESTDIR ?= $(CURDIR)/installed
EBIN_DIR := $(CURDIR)/ebin

REBAR3?=./rebar3

all: clnd

clean:
	$(REBAR3) clean
	find . \( -name '*.beam' -or -name '*.o' -or -name 'erlcinfo' \) -delete

clnd:
	$(REBAR3) as client compile

clnd:
	$(REBAR3) as client release
