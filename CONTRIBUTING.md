# Contributing

## Build the sandbox vm

```
nix build .#nixosConfigurations.sandbox.config.system.build.toplevel
nix build .#nixosConfigurations.sandbox.config.system.build.vm
```

## Run sandbox vm

```
nix build .#nixosConfigurations.sandbox.config.system.build.vm
./result/bin/run-nixos-vm
```

Note that image state is stored in a qcow2 image, you may wish to remove this if
state from previous builds is interfering with your current build:

```
rm nixos.qcow2; ./result/bin/run-nixos-vm
```

Before running any scripts, it's important to wait for the BFT nodes to finish starting up (check for the existence of `/var/lib/cardano-node/state/node-bft1/node.sock`.
