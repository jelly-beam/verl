#!/bin/bash
set -e

# Setup:
#
#     mix escript.install hex ex_doc
#     asdf install erlang 24.0-rc1
#     asdf local erlang 24.0-rc1

rebar3 compile
rebar3 edoc
version=1.1.0
ex_doc "verl" $version "_build/default/lib/verl/ebin" \
  --source-ref v${version} \
  --config docs.exs $@
