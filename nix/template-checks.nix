{ pkgs ? import <nixpkgs> { } }:
rec {
  test = "hello";
  ctlPackageJson = builtins.readFile ../package.json;
  ctlScaffoldPackageJson = builtins.readFile ../templates/ctl-scaffold/package.json;

  template-init = test;
  template-build = test;
  template-plutip = test;

  template-deps-json = pkgs.runCommand "template-deps-check"
    {
      inherit ctlPackageJson ctlScaffoldPackageJson;
      nativeBuildInputs = [ pkgs.jq ];
    } ''
    diff <(jq -S .dependencies <<< $ctlPackageJson) <(jq -S .dependencies <<< $ctlScaffoldPackageJson)
    touch $out
  '';

  template-deps-dhall = test;
}
