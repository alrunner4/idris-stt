{
nixpkgs ? import <nixpkgs> {},
withSource ? false,
}:
(nixpkgs.idris2Packages.buildIdris {
    ipkgName = "stt";
    version = "2025.12.08";
    src = ./.;
    idrisLibraries = [];
}).library { inherit withSource; }
