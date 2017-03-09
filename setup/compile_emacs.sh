#!/bin/bash

read -p "Installing v25.1 of emacs [Enter]"
echo ""
cd ~
mkdir -p src
cd src
wget http://ftp.gnu.org/gnu/emacs/emacs-25.1.tar.gz -P ~/src

#Check if download was successfull
if [ -e ~/src/emacs-25.1.tar.gz ]
then
    echo "Download succeeded successfully."
else
    echo "Download not completed successfully."
    exit
fi

echo ""
read -p "Unpacking the tar-file [Enter]"

tar -zxf emacs-25.1.tar.gz
cd emacs-25.1
chmod +x configure

echo ""
echo ""
read -p "Executing ./configure [Enter]"

./configure

echo "There might be some missing packages listed in the ./configure output"
echo "You might want to go ahead and fix or ignore those. Next steps:"
echo "make"
echo "make install (requires root privileges)"
