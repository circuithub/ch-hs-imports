#!/usr/bin/env nix-shell
#!nix-shell -i bash --pure -p nix gnused gawk git cacert

# update fetch.nix to be the same revision as the specified nixos channel's revision

if [ -z "$1" ]; then
  echo "Please specify the nixos channel to update to"
  exit 1
fi

echo "updating to latest revision for channel $1"

# script working directory
SWD=$(dirname $(realpath "$0"))
# get the revision from the current system's nixos version
REV=$(git ls-remote https://github.com/NixOS/nixpkgs-channels "refs/heads/$1" | awk '{print $1}')
# prefetch the revision and get its SHA
HASH=$(nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs-channels/archive/$REV.tar.gz)
# update the revision in fetch.nix
sed -i "/rev = \"/s/\".*/\"$REV\";/" $SWD/fetch.nix
# update the sha in fetch.nix
sed -i "/hash = \"/s/\".*/\"$HASH\";/" $SWD/fetch.nix
