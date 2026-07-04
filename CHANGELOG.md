# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Support for the Mixpanel `/import` ingestion endpoint via `importEvents` (up to 2000 records per request).
- An `ImportPayload` sum type modelling what `/import` accepts: ordinary events (`mkImportEvent`) and `$merge` identity records (`mkMergeEvent`, original ID merge projects only).
- `mkIdentifiedEvent` for linking a device id to a user id on simplified ID merge projects.
- `ImportResponse` and `FailedRecord` types for inspecting per-record results of strict-mode imports.
- `ProjectToken` and `ServiceAccount` credential types with an `Auth` class that turns them into wire-level `Credentials`.
- `ImportAuth` and `ServiceAuth` capability classes that make passing an unsupported credential type to an endpoint a compile error.

## [0.3.0] - 2020-12-14

### Fixed

- expose EngageData
- EngageData.ignoreTime should be a `Maybe Bool`
- fix parsing of ignoreTime

## [0.2.1] - 2019-08-26

### Fixed

- 0.2.0 was a brown-paper-bag release

## [0.2.0] - 2019-03-13

### Changes

- #4 add servant 0.16 support @domenkozar

## [0.1.1] - 2018-11-28

### Changes

- ditch hpack @domenkozar
- add Show instance for MixpanelError

## [0.1.0.0] - 2018-08-25

- Intial release @domenkozar
