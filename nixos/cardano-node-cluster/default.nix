{ ... } @ inputs:

{
  system = "x86_64-linux";

  modules = [
    { _module.args.inputs = inputs; }
    ./cardano-node-cluter.nix
  ];
}
