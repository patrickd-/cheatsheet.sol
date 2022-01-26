# [cheatsheet.sol](https://github.com/patrickd-/cheatsheet.sol/blob/main/contracts/cheatsheet.sol)🔗

This cheatsheet was once a [GitHub Gist](https://gist.github.com/patrickd-/5000fc1eb3e9d92f555a9ea2af94de54) and was moved into its own repository to allow for contributions.

---


[![Compiles](https://github.com/patrickd-/cheatsheet.sol/actions/workflows/compiles.yaml/badge.svg)](https://github.com/patrickd-/cheatsheet.sol/actions/workflows/compiles.yaml) [![Lint](https://github.com/patrickd-/cheatsheet.sol/actions/workflows/lint.yaml/badge.svg)](https://github.com/patrickd-/cheatsheet.sol/actions/workflows/lint.yaml)

## It compiles!

Play around with this cheatsheet in [Remix](https://remix.ethereum.org/)!

Create a new file `contracts/cheatsheet.sol` within Remix's `default_workspace` and copy everything from [cheatsheet.sol](https://raw.githubusercontent.com/patrickd-/cheatsheet.sol/main/contracts/cheatsheet.sol) into it.

## Contribute

Pull requests are very welcome, note though that I consider this my personal cheatsheet and I'll make the final decision on what goes in and whatnot (sorry!).

After checking out the repository make sure to install the dev-dependencies:

```
npm install
```

Before pushing changes make sure to check everything's in order:

```
npm run compiles && npm run lint
```

## License

The cheatsheet was MIT licensed originally, but since it makes use of some example contracts from remix which are GPLv3 licensed, the license was changed so those files can be included here in order to check whether it compiles in the CI pipelines (GitHub Workflow Actions). See [LICENSE](https://github.com/patrickd-/cheatsheet.sol/blob/main/LICENSE) file.
