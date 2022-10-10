SHELL := /bin/bash

CURDIR := $(shell pwd)
DESTDIR ?= $(CURDIR)/installed
EBIN_DIR := $(CURDIR)/ebin

REBAR3?=./rebar3

all: srvd

clean:
	$(REBAR3) clean
	find . \( -name '*.beam' -or -name '*.o' -or -name 'erlcinfo' \) -delete

srvd:
	$(REBAR3) as server compile

srv:
	$(REBAR3) as server release
