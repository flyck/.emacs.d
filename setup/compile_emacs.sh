#!/bin/bash

version="25.3"

read -p "Installing v$version of emacs [Enter]"
echo ""
cd ~
mkdir -p src
cd src
wget http://ftp.gnu.org/gnu/emacs/emacs-$version.tar.gz -P ~/src

#Check if download was successfull
if [ -e ~/src/emacs-$version.tar.gz ]
then
    echo "Download succeeded successfully."
else
    echo "Download not completed successfully."
    exit
fi

echo ""
read -p "Unpacking the tar-file [Enter]"

tar -zxf emacs-$version.tar.gz
cd emacs-$version
chmod +x configure

echo ""
echo ""
read -p "Executing ./configure [Enter]"

./configure

echo "There might be some missing packages listed in the ./configure output"
echo "You might want to go ahead and fix or ignore those. Next steps:"
echo "make"
echo "make install (requires root privileges)"
