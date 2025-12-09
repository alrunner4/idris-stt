{
idrx ? builtins.getFlake "github:alrunner4/idrx",
withSource ? false,
}:
idrx.importFromSrc {
  ipkgName = "stt";
  version = "2025.12.08";
  src = ./.;
  idrisLibraries = [];
}
