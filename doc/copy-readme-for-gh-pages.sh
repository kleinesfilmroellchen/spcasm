#!/bin/sh

set -e

cd "$(dirname "$(readlink -f "$0")")"

cp '../README.md' 'src/README.md'

sed -i 's/\(\.\.\/\.\.\/README\.md\)/README.md/' 'src/SUMMARY.md'
sed -i 's/doc\/src\/SUMMARY\.md/README.md/' 'src/README.md'
