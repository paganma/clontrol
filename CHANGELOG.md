# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## 0.1.0 - 2026-01-05
#### Features
- (**cps-form-emitter**) Switch to using context parameter - (13445de) - Marco Paganoni
- (**cps-form-emitter**) Isolating lexical contexts during conversion - (3d37e49) - Marco Paganoni
- (**cps-form-emitter**) Emit a concrete recur form whenever possible - (538be38) - Marco Paganoi
- (**direct-marker**) Remove unused direct operation types - (35c652f) - Marco Paganoni
- (**form-builder**) Implement RecurTailBuilder - (5a9c92a) - Marco Paganoi
- (**function**) Implement call-shift - (e3f7cfc) - Marco Paganoni
- (**function**) Add definitions for fn-cps - (6bd9bce) - Marco Paganoni
- (**function_type_reader**) Use a predef set of :direct namespaces - (f3442e6) - Marco Paganoi
- (**hole**) Change conversions to and from hole to direct style - (3ed1cc9) - Marco Paganoni
- (**hole**) Build indirect recur-tail as the continuation body - (1108c7d) - Marco Paganoi
- (**operator-test**) Test scope-preservation - (b5214a8) - Marco Paganoni
- (**operator-test**) Add new tests for try-catch - (2741dee) - Marco Paganoni
- (**recur-dominator-marker**) Implement basic marking - (821711a) - Marco Paganoni
- (**shadowings-tagger**) Improve naming - (2625af2) - Marco Paganoi
- Initial structure - (0617c23) - Marco Paganoni
#### Bug Fixes
- (**core-test**) Use smoke instead of shift - (721c3a3) - Marco Paganoni
- (**core-test**) Use smoke instead of shift - (b5acca1) - Marco Paganoni
- (**cps-form-emitter**) Simplify hole capturing - (16ba381) - Marco Paganoi
- (**cps-form-emitter**) Deduplicate continuations for invocations - (5c1a2b8) - Marco Paganoi
- (**cps-form-emitter**) Refactor out emit-recur-intermediate - (72ecc3b) - Marco Paganoi
- (**cps-form-emitter**) Remove calls to reify-hole - (c5a16f4) - Marco Paganoni
- (**cps-form-emitter**) Emit correct try-finally forms - (3b0e0be) - Marco Paganoi
- (**cps-form-emitter**) Add DEFN tests - (9f3ff5f) - Marco Paganoni
- (**cps-form-emitter**) Fix semantics of CPS try-catch blocks - (406423b) - Marco Paganoni
- (**cps-form-emitter**) Use prepend-binding instead of a let block - (f43c9d6) - Marco Paganoi
- (**cps-form-emitter**) Remove calls to reify-hole - (cde1953) - Marco Paganoni
- (**deps**) Remove deprecated dependency - (4750398) - Marco Paganoi
- (**direct-marker**) Use renamed node/tail? - (60603c5) - Marco Paganoni
- (**direct-marker**) Use renamed read-control-type - (4602e43) - Marco Paganoni
- (**direct-marker**) Defer thunking to recursion points - (e302ac7) - Marco Paganoni
- (**direct-marker**) Defer thunking to recursion points - (f9fc265) - Marco Paganoni
- (**form-builder**) Adjust order of definitions - (dbb719c) - Marco Paganoi
- (**hole**) Refactor hole capturers - (59195f2) - Marco Paganoi
- (**hole**) Refactor reify-hole and isolate-hole outside of emitter - (2b3d79c) - Marco Paganoi
- (**hole**) Thread context into reify-closure-hole - (d3a1d31) - Marco Paganoi
- (**hole**) Propagate plug-inner meta - (0a4d0d9) - Marco Paganoni
- (**hole**) Refactor reify-closure-hole out of cps-form-emitter - (055d8ad) - Marco Paganoni
- (**meta-reader**) Read the entire map instead of a single entry - (bd6155e) - Marco Paganoi
- (**node**) Correct definition of intermediate? - (de19f84) - Marco Paganoi
- (**node**) Rename tail-node? to tail?; add intermediate? predicate - (bc55426) - Marco Paganoni
- (**operator**) Modify emit-reset-to to receive local-bindings nodes - (5e1d3e3) - Marco Paganoni
- (**operator**) Test recursions on a direct target - (a2e00ad) - Marco Paganoni
- (**operator**) Test recursions on a direct target - (144815e) - Marco Paganoni
- (**operator-test**) Add more try-catch tests - (c88de9d) - Marco Paganoi
- (**parser**) Avoid parsing the body of fn*, reify*, deftype* forms - (694b55a) - Marco Paganoni
- (**pure-marker**) Remove unused pure operation types - (f487acd) - Marco Paganoni
- (**recur-dominator-marker**) Adjust traversal order - (67199c4) - Marco Paganoi
- (**recur-dominator-marker**) Complete marking strategy - (81d9024) - Marco Paganoi
- (**shifter**) Optimize calls to unknown functions - (2fc73a7) - Marco Paganoni
- (**shifter**) Attach handler to throw-out-of-prompt-exception - (8991255) - Marco Paganoi
- (**shifter**) Adjust name of def-invoke-unknown-fn - (36b6da2) - Marco Paganoi
- (**smoke-test**) More tests handling escaped operations - (f78c33b) - Marco Paganoi
- (**smoke-test**) Add failing recursion test - (563d285) - Marco Paganoni
- (**smoke-test**) Add failing recursion test - (a0e004a) - Marco Paganoni
- (**thunk**) Remove unused code - (7b0c708) - Marco Paganoni
- Formatting - (da056fb) - Marco Paganoni
- Remove recur-dominator-marker - (860c54e) - Marco Paganoni
- Refactor local and parameter names - (8f1f0a0) - Marco Paganoi
- Revert back to thunking for converting loops to CPS - (cccc17c) - Marco Paganoi
- Improve the efficiency of emitted intermediate forms - (dfbdbc8) - Marco Paganoi
- Reorganizing intermediate emitters - (bff0043) - Marco Paganoni
#### Miscellaneous Chores
- (**control-type-reader**) Rename from function-type-reader - (849519f) - Marco Paganoni
- (**core-test**) Refactor tests in operator-test into core-test - (17c3978) - Marco Paganoni
- (**cps-form-emitter**) Refactored emit-intermediate - (44f001d) - Marco Paganoni
- (**direct-marker**) Improve docstring for mark-expression - (4b41687) - Marco Paganoni
- (**direct-marker**) Improve docstring for mark-expression - (400841b) - Marco Paganoni
- (**meta-reader**) Fix formatting - (ce91f0d) - Marco Paganoi
- (**pure-marker**) Sort *pure-operations* - (e3e530b) - Marco Paganoni
- (**release.yml**) Remove empty line - (93f8d17) - Marco Paganoni
- (**shifter**) Adjust formatting - (3a83c02) - Marco Paganoi
- (**smoke-test**) Remove logging - (3a5b637) - Marco Paganoni
- (**smoke-test**) Remove logging - (5af4621) - Marco Paganoni
- Fix release action - (27fd62e) - Marco Paganoni
- Mock release Github action - (93f5092) - Marco Paganoni
- Remove unnecessary extra path - (64ea23b) - Marco Paganoi
- Remove README note on loop transformation - (92388fb) - Marco Paganoi
- Add development dependencies - (cb15995) - Marco Paganoi
- Reorganize deps.edn - (cd9c9c5) - Marco Paganoni
- Setup cloverage - (559b498) - Marco Paganoni
- Setup build - (96a88c3) - Marco Paganoni
- Fix style; silence warning - (baf9a91) - Marco Paganoni
- Add :lint-as definitions for clj-kondo - (c3703e7) - Marco Paganoi
- Add clj-kondo/config to dependencies - (325dd5d) - Marco Paganoni

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).