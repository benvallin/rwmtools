#!/bin/bash

# Case when OS argument is invalid
if [[ $([ "$1" = "Windows" ] || [ "$1" = "macOS" ]; echo "$?") -ne 0 ]]
then
  printf "\ndifftools set up aborted.\nPlease supply a correct OS type, which must be either \"Windows\" or \"macOS\".\n\n"
  exit 1 
fi

# Case when R is not installed in /c/Program Files/ on a Windows machine
if [[ "$1" = "Windows" ]] 
then 
  if [[ $(ls /c/"Program Files"/ | grep -w 'R' | wc -l) -eq 0 ]]
  then
    printf "\ndifftools set up aborted.\nPlease install R in /c/Program Files/; if R is already installed in a different location, you will have to set up difftools manually.\n\n"
    exit 1
  fi
fi

# Case when R is not installed in /usr/local/bin/ on a macOS machine
if [[ "$1" = "macOS" ]] 
then 
  if [[ $(ls /usr/local/bin/ | grep -w 'Rscript' | wc -l) -eq 0 ]]
  then
    printf "\ndifftools set up aborted.\nPlease install R in /usr/local/bin/; if R is already installed in a different location, you will have to set up difftools manually.\n\n"
    exit 1
  fi
  
# Case when the current shell is not bash or zsh on a macOS machine
  SHELLNAME=$(echo $SHELL | grep -o "[a-z]*$")
	if [[ $([ $SHELLNAME = "bash" ] || [ $SHELLNAME = "zsh" ]; echo "$?") -ne 0 ]]
	then
		printf "\ndifftools set up aborted.\nYour shell is $SHELLNAME and difftools_installer only deals with bash or zsh.\nPlease set up difftools manually (or change your shell to bash or zsh).\n\n"
	  exit 1
	fi
fi

# Case when difftools folder is not on user's Desktop
if [[ "$1" = "Windows" ]]
then
  if [[ $(ls /c/Users/$(whoami)/Desktop/ | grep -w difftools | wc -l) -eq 0 ]]
  then
    printf "\ndifftools set up aborted.\nPlease add difftools folder in /c/Users/$(whoami)/Desktop/.\n\n"
    exit 1
  fi
elif [[ "$1" = "macOS" ]]
then
  if [[ $(ls /Users/$(whoami)/Desktop/ | grep -w difftools | wc -l) -eq 0 ]]
  then
    printf "\ndifftools set up aborted.\nPlease add difftools folder in /Users/$(whoami)/Desktop/.\n\n"
    exit 1
  fi
fi

# Case when difftools folder does not contain mkmed and/or diffd file(s)
if [[ "$1" = "Windows" ]]
then
  if [[ $(ls /c/Users/$(whoami)/Desktop/difftools/ | grep -w mkmed | wc -l) -eq 0 || $(ls /c/Users/$(whoami)/Desktop/difftools/ | grep -w diffd | wc -l) -eq 0 ]]
  then
    printf "\ndifftools set up aborted.\nPlease add mkmed and diffd files in /c/Users/$(whoami)/Desktop/difftools/.\n\n"
    exit 1
  fi
elif [[ "$1" = "macOS" ]]
then
  if [[ $(ls /Users/$(whoami)/Desktop/difftools/ | grep -w mkmed | wc -l) -eq 0 || $(ls /Users/$(whoami)/Desktop/difftools/ | grep -w diffd | wc -l) -eq 0 ]]
  then
    printf "\ndifftools set up aborted.\nPlease add mkmed and diffd files in /Users/$(whoami)/Desktop/difftools/.\n\n"
    exit 1
  fi
fi

# Set up on a Windows machine
if [[ "$1" = "Windows" ]]
then
	printf "\nSetting up difftools for windows...\n"
	if [[ $(ls "$HOME" | grep -w bin | wc -l) -eq 0 ]]
	then
		printf "creating bin folder in $HOME.\n"
		mkdir "$HOME"/bin
	fi
	printf "moving mkmed in $HOME/bin.\n"
	mv /c/Users/$(whoami)/Desktop/difftools/mkmed "$HOME"/bin/mkmed
	printf "moving diffd in $HOME/bin.\n"
	mv /c/Users/$(whoami)/Desktop/difftools/diffd "$HOME"/bin/diffd
	if [[ $(ls -a "$HOME" | grep -w '.bash_profile' | wc -l) -eq 0 ]]
	then
		printf "creating .bash_profile in $HOME.\n"
	else
		printf "modifying .bash_profile.\n"
	fi
	echo 'R_VERSION=$(ls /c/"Program Files"/R/ | cut -d "/" -f 1)' >> "$HOME"/.bash_profile
	echo 'export PATH="$PATH:/c/Program Files/R/$R_VERSION/bin:$HOME/bin"' >> "$HOME"/.bash_profile
	echo 'export LC_ALL=C' >> "$HOME"/.bash_profile
	printf "\ndifftools setup is complete!\n\nTo start using difftools, you first need to quit/restart your terminal or run the following command: source ~/.bash_profile\n\n"
	exit 0
fi

# Set up on a macOS machine
if [[ "$1" = "macOS" ]]
then
  printf "\nSetting up difftools for macOS...\n"
  if [[ $(ls "$HOME" | grep -w bin | wc -l) -eq 0 ]]
  then
    printf "creating bin folder in $HOME.\n"
    mkdir "$HOME"/bin
  fi
  printf "moving mkmed in $HOME/bin.\n"
	mv /Users/$(whoami)/Desktop/difftools/mkmed "$HOME"/bin/mkmed
	printf "moving diffd in $HOME/bin.\n"
	mv /Users/$(whoami)/Desktop/difftools/diffd "$HOME"/bin/diffd
	if [[ $SHELLNAME = "bash" ]]
	then
	  printf "modifying .bash_profile.\n"
	  echo 'export PATH="$PATH:$HOME/bin"' >> "$HOME"/.bash_profile
	  printf "\ndifftools setup is complete!\n\nTo start using difftools, you first need to quit/restart your terminal or run the following command: source ~/.bash_profile\n\n"
	elif [[ $SHELLNAME = "zsh" ]]
	then
	  printf "modifying .zshrc.\n"
	  echo 'export PATH="$PATH:$HOME/bin"' >> "$HOME"/.zshrc
	  printf "\ndifftools setup is complete!\n\nTo start using difftools, you first need to quit/restart your terminal or run the following command: source ~/.zshrc\n\n"
  fi
  exit 0
fi