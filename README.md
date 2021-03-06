
See the FA1.2 [Quick Start Tutorial](https://assets.tqtezos.com/token-contracts/1-fa12-lorentz) for more detail.

## Setting Up
### Requirements
#### Tezos-client
To set up the tezos-client, follow the instructions in the [Setup Tezos Client](https://assets.tqtezos.com/setup/1-tezos-client) tutorial

#### Stack -v 1.9.3
This project requires `stack@1.9.3` (newer versions have a known bug). You can install this version of `stack` from their [github repo](https://github.com/commercialhaskell/stack/releases/tag/v1.9.3). You may find the instructions [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/#install-older-versions) helpful, though they are written with windows operating system in mind. 

You can [add the stack executable to your path](https://docs.haskellstack.org/en/stable/install_and_upgrade/#path) if you want to access it as a global command (i.e. `stack --version`) or you can provide a path to the executable when you call it (e.g. `~/Downloads/stack --version`)

### Installing Dependencies
`stack build` Note that this will take some time. 

## Multisig
See [here](README_SPECIALIZED.md) for the specalized multisig. Note that all commands in the readme are run from the root of the project.