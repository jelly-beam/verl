#!/bin/bash
set -e

if [[ -z "$1" || -z "$2" ]]; then
  echo "Usage: vendor.sh TARGET_DIR PREFIX"
  exit 1
fi

source_dir=`dirname $0`/src
target_dir=$1
prefix=$2
verl_version=`cat $source_dir/verl.hrl | grep VERL_VERSION | cut -d'"' -f2`

filenames="verl.hrl \
           verl.erl \
           verl_parser.erl"

search_to_replace=""

rm -f $target_dir/$prefix*

for filename in $filenames; do
  source_path=$source_dir/$filename
  target_path=$target_dir/$prefix$filename

  echo "%% Vendored from verl v$verl_version, do not edit manually" > $target_path
  echo >> $target_path
  cat $source_path >> $target_path

  for word in $search_to_replace; do
    sed -i.bak s/$word/$prefix$word/g $target_path
    rm $target_path.bak
  done
done
