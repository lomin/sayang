#!/usr/bin/env bash
clj -A:depify | clj -A:zprint > deps.edn.tmp ; mv deps.edn.tmp deps.edn