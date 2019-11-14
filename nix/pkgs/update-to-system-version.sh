#!/usr/bin/env nix-shell
#!nix-shell -i bash --pure -p nix gnused gawk

# update fetch.nix to be the same version as the current system's nixos version

# script working directory
SWD=$(dirname $(realpath "$0"))
# get the revision from the current system's nixos version
REV=$(cat /nix/var/nix/profiles/system/nixos-version | awk -F '.' '{print $NF}')
# prefetch the revision and get its SHA
HASH=$(nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs-channels/archive/$REV.tar.gz)
# update the revision in fetch.nix
sed -i "/rev = \"/s/\".*/\"$REV\";/" $SWD/fetch.nix
# update the sha in fetch.nix
sed -i "/hash = \"/s/\".*/\"$HASH\";/" $SWD/fetch.nix
