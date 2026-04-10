# AGENTS.md

## Project

Smarty v5 — PHP template engine. Single Composer package (`smarty/smarty`), namespace `Smarty\`, source in `src/`, autoloaded via PSR-4. Supports PHP 7.2–8.5.

## Commands

```bash
# Install dependencies
composer install

# Regenerate lexers and parsers (required before tests; CI runs this)
make -B

# Run all tests
php ./vendor/phpunit/phpunit/phpunit

# Run a single test file
php ./vendor/phpunit/phpunit/phpunit tests/UnitTests/TemplateSource/TagTests/If/CompileIfTest.php

# Run tests matching a PHPUnit group
php ./vendor/phpunit/phpunit/phpunit --group 20221124

# Run tests excluding slow group
php ./vendor/phpunit/phpunit/phpunit --exclude-group slow
```

There is no linter or static analysis configured. No typecheck step.

## Generated code — do not hand-edit

Four files are generated from grammar/lexer definitions via `make`:

| Source (edit this) | Generated (do not edit) |
|---|---|
| `src/Lexer/TemplateLexer.plex` | `src/Lexer/TemplateLexer.php` |
| `src/Lexer/ConfigfileLexer.plex` | `src/Lexer/ConfigfileLexer.php` |
| `src/Parser/TemplateParser.y` | `src/Parser/TemplateParser.php` |
| `src/Parser/ConfigfileParser.y` | `src/Parser/ConfigfileParser.php` |

After editing a `.plex` or `.y` file, run `make -B` to regenerate. The generators require the `smarty/smarty-lexer` dev dependency.

## Architecture

- `src/Smarty.php` — main class, extends `TemplateBase`. Version constant: `Smarty::SMARTY_VERSION`.
- `src/Compile/`, `src/Compiler/` — template compilation pipeline.
- `src/Lexer/`, `src/Parser/` — lexer/parser (generated, see above).
- `src/Extension/` — extension system (`ExtensionInterface`, `CoreExtension`, `DefaultExtension`, `BCPluginsAdapter`).
- `src/Runtime/` — runtime helpers (foreach, capture, inheritance, tpl functions).
- `src/Resource/`, `src/Cacheresource/` — template resource and cache resource handlers.
- `src/BlockHandler/`, `src/FunctionHandler/`, `src/Filter/` — built-in tags, functions, and filters.
- `libs/Smarty.class.php` — non-Composer autoload stub. Points to `src/`. Not the main source.
- `src/functions.php` — global helper functions, always loaded via Composer `files` autoload.

## Tests

- Framework: PHPUnit 7.5/8.5 (bootstrap: `tests/Bootstrap.php`).
- All tests extend `PHPUnit_Smarty` (defined in `tests/PHPUnit_Smarty.php`), which provides `setUpSmarty($dir)`.
- Test suite root: `tests/UnitTests/`. Typical test `setUp()` calls `$this->setUpSmarty(__DIR__)`.
- Each test directory may have its own `templates/`, `configs/` subdirectories. Compiled output goes to `templates_c/` and `cache/` (auto-created by the test harness).
- Three test files are excluded in `phpunit.xml`: Memcache, APC, and HttpModifiedSince tests (require external services).
- Tests needing MySQL/PDO are gated by constants in `tests/Config.php` (disabled by default).

## CI

GitHub Actions (`.github/workflows/ci.yml`): matrix of PHP 7.2–8.5 on ubuntu + windows. Steps: `composer install` → `make -B` → `phpunit`. No deploy step.

## Docs

Markdown in `docs/`, built with mkdocs + Material theme. Preview: `mkdocs serve`. Published via `mike deploy 5.x`.

## Release

`./make-release.sh <version>` — only v5.x.x. Updates changelog and version constant, creates a merge commit and tag on `master`.
