attrs@{...}:
let
  # fetch the pegged nixpkgs
  nixpkgs = import ./fetch.nix;
  # the overlays that customizes our package set
  overlays =
    # make the pegged nixpkgs available as a dependency as nixpkgs-src
    [(self: super: {nixpkgs-src = nixpkgs;})] ++
    # apply all the overlays
    (import ../overlays) ++
    # apply any other overlays that are passed in when instantiating the package set
    (attrs.overlays or []);
  # pass through all other attributes for package set customization
  passThrough = removeAttrs attrs ["overlays"];

in # instantiate the package set
  import nixpkgs (passThrough // {inherit overlays;})
