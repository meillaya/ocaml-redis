{ pkgs, ... }:

{
  # The project only needs the OCaml stdlib plus `unix`, so the shell keeps the
  # toolchain small and makes the formatter version match .ocamlformat.
  languages.ocaml.enable = true;

  packages = [
    pkgs.dune_3
    pkgs.ocamlPackages.ocamlformat
    pkgs.ocamlPackages.tls
    pkgs.ocamlPackages.x509
    pkgs.openssl
    pkgs.redis
    pkgs.jq
  ];

  env.OCAML_REDIS_SERVER = "app/server_main.exe";

  scripts.build.exec = "dune build";

  scripts.tests.exec = "dune test --display=short";

  scripts.fmt.exec = "dune build @fmt --auto-promote || true";

  scripts.fmt-check.exec = "dune build @fmt";

  scripts.server.exec = ''
    dune exec app/server_main.exe -- "$@"
  '';

  scripts.live-test.exec = "OCAML_REDIS_LIVE=1 dune exec test/test_main.exe";

  enterShell = ''
    echo "ocaml-redis development shell"
    echo "  ocaml       $(ocaml -version)"
    echo "  dune        $(dune --version)"
    echo "  ocamlformat $(ocamlformat --version)"
    echo "  commands    build tests fmt fmt-check server live-test"
  '';

  enterTest = "dune test --display=short";
}
