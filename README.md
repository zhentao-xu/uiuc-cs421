# uiuc-cs421
My UIUC CS421 Learning Code

# Setup
### 1. Uninstall Haskell Installation (Mac)

```shell
sudo uninstall-hs all --remove
```


### 2. Install Haskell Installation (Mac)
```shell
# Install the command-line developer tools from a terminal
xcode-select --install

# Run the ghcup installer from a terminal by running the following command.
# GHCup is a tool that installs and manages core Haskell tools, including cabal, stack, haskell-language-server, and ghc
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_GHC_VERSION=9.2.5 sh
```

Open another terminal
```shell
ghc --version
```

### Setup VS Code
1. Install the [Haskell Syntax Highlighting](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell) VSCode Extension



