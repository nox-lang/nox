// Package pkgmgr implements the small subset of package-manifest handling
// (nox.toml) and project scaffolding described in the Nox language spec:
// `nox init`, `nox get`, and reading dependency declarations for `nox
// build`. It intentionally implements just enough of a TOML-like format for
