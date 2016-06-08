#!/bin/bash

read -p "Installing v24.5 of emacs [Enter]"
echo ""
cd ~
mkdir -p src
cd src
wget http://ftp.gnu.org/gnu/emacs/emacs-24.5.tar.gz -P /~/src

#Check if download was successfull
if [ -e ~/src/emacs-24.5.tar.gz ]
then
    echo "Download succeeded successfully."
else
    echo "Download not completed successfully."
    exit
fi

echo ""
read -p "Unpacking the tar-file [Enter]"

tar -zxf emacs-24.5.tar.gz
cd emacs-24.5
chmod +x configure

echo ""
echo ""
read -p "Executing ./configure [Enter]"

./configure

echo "There might be some missing packages listed in the ./configure output"
echo "You might want to go ahead and fix those. Then execute make and make install."
