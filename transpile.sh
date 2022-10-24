#!/usr/bin/env bash

LANGUAGE=$1

gosh header.scm | gosh $LANGUAGE/header.scm| gosh simplify.scm | gosh cps.scm | gosh $LANGUAGE/language.scm
