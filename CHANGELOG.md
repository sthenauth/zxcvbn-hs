# Revision History

## 0.3.6 2023.09.11 
+ bump tasty bounds

## 0.3.4 2023.08.13 
+ Bump opt parse applicative [#20](https://github.com/sthenauth/zxcvbn-hs/pull/20)

## 0.3.3 (Aug 02 2023)
* Bump [#17](https://github.com/sthenauth/zxcvbn-hs/pull/17)
  aeson >=1.3 && <2.2 (latest: 2.2.0.0)
  hedgehog >=0.6 && <1.3 (latest: 1.3)


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
