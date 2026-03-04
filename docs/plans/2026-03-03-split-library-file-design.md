# Split compile_time_continuation_library.go

**Date**: 2026-03-03
**Status**: Approved
**Type**: Refactoring — file decomposition, no behavior change

## Problem

`machine/compile_time_continuation_library.go` is 824 lines handling 5+ concerns:
library forms, export, import, cond-expand, define-syntax, and letrec* body.

## Design

Split into 4 new files + residual original:

### compile_library_forms.go (~340 lines)

Library definition entry point and its sub-declarations.

- `CompileDefineLibrary`
- `processLibraryDeclaration`
- `processLibraryExport` + `parseExportSpec`
- `CompileExport` (top-level error stub)
- `processIncludeLibraryDeclarations`

### compile_import.go (~110 lines)

Import resolution for both top-level and library-internal contexts.

- `CompileImport`
- `processLibraryImport`

### compile_cond_expand.go (~220 lines)

Feature requirement parsing and cond-expand compilation.

- `resolveCondExpandClause`
- `CompileCondExpand`
- `processCondExpand`
- `parseFeatureRequirement`
- `parseFeatureRequirementList`

### compile_define_syntax.go (~75 lines)

Syntax definition handling.

- `CompileDefineSyntax`

### compile_time_continuation_library.go (remains, ~45 lines)

Only the letrec* body compilation.

- `compileLibraryBegin`

## Constraints

- All files stay in `package machine`
- No API changes, no behavior changes
- Each file gets only the imports it actually uses
- Copyright header on each new file
