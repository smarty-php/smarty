# AGENTS.md

This file is the single source of truth for AI coding assistants working in this repo (including Claude Code, claude.ai/code).

## Project

Smarty v4 — PHP template engine, maintained on the `support/4` branch. Single Composer package (`smarty/smarty`), no namespaces, source in `libs/`, autoloaded via a Composer `classmap` of `libs/`. Supports PHP 7.1–8.6.

Do not use PHP syntax newer than 7.1 in `libs/` unless it is guarded for older runtimes.

Smarty v5 lives on `master` with a different layout (`src/`, namespace `Smarty\`). Fixes are often ported between the two; do not copy v5 code or paths into this branch without adapting them.

## Commands

```bash
# Install dependencies
composer install

# Run all tests (runs `composer update` first, then PHPUnit; extra args are passed to PHPUnit)
./run-tests.sh

# Run PHPUnit directly
php ./vendor/phpunit/phpunit/phpunit

# Run a single test file
php ./vendor/phpunit/phpunit/phpunit tests/UnitTests/TemplateSource/TagTests/If/CompileIfTest.php

# Run tests matching a PHPUnit group
php ./vendor/phpunit/phpunit/phpunit --group 20221124

# Run tests excluding slow group
php ./vendor/phpunit/phpunit/phpunit --exclude-group slow

# Run the suite on every supported PHP version (Docker, see docker-compose.yml)
./run-tests-for-all-php-versions.sh
```

There is no build step, linter or static analysis configured. No typecheck step.

## Lexer and parser

The lexer/parser sources and their generated PHP classes are both committed:

| Source | Generated (committed) |
|---|---|
| `lexer/smarty_internal_templatelexer.plex` | `libs/sysplugins/smarty_internal_templatelexer.php` |
| `lexer/smarty_internal_configfilelexer.plex` | `libs/sysplugins/smarty_internal_configfilelexer.php` |
| `lexer/smarty_internal_templateparser.y` | `libs/sysplugins/smarty_internal_templateparser.php` |
| `lexer/smarty_internal_configfileparser.y` | `libs/sysplugins/smarty_internal_configfileparser.php` |

There is no `make` step on this branch, and the generated files were reformatted by hand after generation, so they are not a byte-for-byte generator output. When changing the grammar or lexer, make the same change in both the source and the generated file and keep them in sync. Ask before regenerating them with the `smarty/smarty-lexer` dev dependency.

## Architecture

- `libs/Smarty.class.php` — main `Smarty` class, extends `Smarty_Internal_TemplateBase`. Version constant: `Smarty::SMARTY_VERSION`.
- `libs/sysplugins/` — internal classes (`smarty_internal_*`, `smarty_*`): compiler, compile handlers for tags (`smarty_internal_compile_*`), runtime helpers (`smarty_internal_runtime_*`), resources, cache resources, template/data objects, generated lexer/parser.
- `libs/plugins/` — built-in plugins, one file per plugin, named `<type>.<name>.php` (`function.*`, `modifier.*`, `modifiercompiler.*`, `block.*`, `outputfilter.*`, `variablefilter.*`) plus `shared.*` helpers.
- `libs/Autoloader.php`, `libs/bootstrap.php` — autoloader for non-Composer installs.
- `libs/functions.php` — global helper functions, included by `Smarty.class.php`.
- `libs/debug.tpl` — template for the debug console.

## Tests

- Framework: PHPUnit 7.5/8.5 (bootstrap: `tests/Bootstrap.php`).
- All tests extend `PHPUnit_Smarty` (defined in `tests/PHPUnit_Smarty.php`), which provides `setUpSmarty($dir)`.
- Test suite root: `tests/UnitTests/`. Typical test `setUp()` calls `$this->setUpSmarty(__DIR__)`.
- Each test directory may have its own `templates/`, `configs/` subdirectories. Compiled output goes to `templates_c/` and `cache/` (auto-created by the test harness).
- Running the suite scatters generated `templates_c/`, `cache/`, and `templates_tmp/` directories (and empty runtime `templates/`/`configs/` dirs) throughout `tests/` and the repo root. These are not tracked — treat them as noise in `git status`, never commit them, and clean them with `git clean -fd` (exclude tool dirs like `.serena`).
- Three test files are excluded in `phpunit.xml`: Memcache, APC, and HttpModifiedSince tests (require external services).
- Tests needing MySQL/PDO are gated by constants in `tests/Config.php` (disabled by default).

## CI

GitHub Actions (`.github/workflows/ci.yml`): matrix of PHP 7.1–8.6 on ubuntu, plus JIT runs for PHP 8.0+. Steps: `composer validate` → `./run-tests.sh`. No deploy step.

## Docs

Markdown in `docs/`, built with mkdocs + Material theme (`mkdocs.yml`), versioned with `mike`.

## Release

`./make-release.sh <version>` — only v4.x.x. Updates changelog and version constant on a `release/<version>` branch, merges it into `support/4` and tags it.

### Changelog

Never edit `CHANGELOG.md` directly. Every change that should appear in `CHANGELOG.md` must add a new markdown file under `changelog/`. At release time `utilities/update-changelog.php` concatenates all `changelog/*.md` files into the `## [Unreleased]` section, so:

- One file per change, containing a single line that starts with a dash (`- ...`). The filename is arbitrary; name it after the issue number (e.g. `1036.md`) when there is one, otherwise use a short descriptive slug.
- Include a markdown link to the relevant issue when one exists, e.g. `[#1036](https://github.com/smarty-php/smarty/issues/1036)`. Omit the link when there is no public issue (e.g. embargoed security reports).
