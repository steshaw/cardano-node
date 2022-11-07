# Team

The release team is made up of the following:

* Rotating release engineer from one of the development teams
* SRE team supporting deployment and CI/CD
* Test Engineers that focus on integration tests
* Performance Engineers running benchmarking tests

# Release Process

This is the release process for node releases

1. Release Squad Lead will work with rotating engineer and dev leads to determine where to cut the releases from ledger, network and node. These will be published via CHaP and will follow the defined process of the team for versioning, etc...
2. These will be integrated up the stack to the node to produce a release branch
3. Release Squad Lead will work with Test/Performance sub-teams to initiate a testing round. This may be a tagged release or may be tip of branch.
4. A release candidate will be deployed to preview/preprod by SRE
5. Community will be notified of release candidate and given a few days to provide feedback
6. Release Squad Lead will draft Release notes
7. Head of Engineering will meet with release team and identify if release should be published as stable or beta (pre-release) in GitHub. stable releases can go all the way to mainnet, beta should only go to preprod and wait for that release to be relabeled as stable or a new stable release to be cut.
8. Release is published

# Rotating Release Engineer Role

All sprints are aligned across the node and it's components. At the end of a sprint cycle the new rotationg release engineer is decided on by the leadership team.
This person's primary duties are integration of new releases of dependencies up the stack to the node. They serve this role until the release is finalized
according to the above release process (ideally 1 sprint cycle).

# Sub-Teams

## SRE

The SRE team provides the tooling for CI/CD and initiates the deployments to environments. They are responsible for updating dashboards/alerts to align with new
node features/refactoring.

## Test Engineers

The test engineers are responsible for writing and running integration tests from `cardano-node-tests` repository. They execute integration tests as well as
tests that measure node synchronization times between releases.

## Performance Engineers

Performance engineers run benchmarks of the node and report any improvements/regressions between node versions.

