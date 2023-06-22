# Revision History

## 0.3.2 (June 22, 2023)

+ Bump version numbers [#14](https://github.com/sthenauth/zxcvbn-hs/pull/14)
+ Fix minor hlint error
+ Add ghc 9.6 support

## 0.3.1 (May 26, 2022)

+ Bump version numbers

## 0.3.0.0 (October 29, 2020)

  * Added a `ToJSON` instance to the `Score` type.

  * Minor releases:

    - Version 0.3.1 (May 26, 2022): Update dependency bounds

## 0.2.1.0 (September 25, 2019)

  * Export the entire `HasConfig` class so external code can use the
    `config` lens.

## 0.2.0.0 (September 12, 2019)

  * Make it possible for external projects to use the code generation
    tool (`zxcvbn-tools`)

  * Due to the `binary-orphans -> binary-instances` rename this
    package now only compiles under `nixpkgs-unstable`.

## 0.1.0.0 (September 10, 2019)

  * Initial (unreleased) version
