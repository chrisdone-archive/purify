# purify

Reproducible builds for PureScript

## Usage

Make a `purify.yaml` file like this:

``` yaml
output-file: static/js/index.js
extra-deps:
  - repo: https://github.com/purescript/purescript-console.git
    commit: 63d347fa006847170de9c549392601971bd2cc7c
  - repo: https://github.com/purescript/purescript-eff.git
    commit: d6c32f884c434dc2af94a2e9143fac0a0be97281
  - repo: https://github.com/purescript/purescript-prelude.git
    commit: 4b9fdde22cc521b5e132ec69ac95b814aca63356
  - repo: https://github.com/purescript-contrib/purescript-dom.git
    commit: 7b6bd8a397e47c9de32e3eca33792736735ee903
```

Then run `purify` in the directory. [Example output](https://gist.github.com/chrisdone/2fa8215cd73274d2a22cb021f1f5345a).
