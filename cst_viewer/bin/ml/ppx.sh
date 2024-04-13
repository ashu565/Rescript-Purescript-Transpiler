#!/bin/bash

# Input file passed as argument
input_file=$1

# Output file
output_file="${input_file%.ml}.ppx.ml"

# Run the PPX preprocessor
ppx_deriving_yojson -o "$output_file" "$input_file"

# Return the preprocessed file
echo "$output_file"
