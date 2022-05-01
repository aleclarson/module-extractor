# module-extractor

[![npm](https://img.shields.io/npm/v/module-extractor.svg)](https://www.npmjs.com/package/module-extractor)
[![Code style: Prettier](https://img.shields.io/badge/code_style-prettier-ff69b4.svg)](https://github.com/prettier/prettier)
[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://paypal.me/alecdotbiz)

> Extract a module and its dependencies into a new package

&nbsp;

### Usage

```ts
import { extractModules } from 'module-extractor'

const extraction = extractModules({
  // At least one entry module must be given
  entries: ['index.ts'],
  // The directory that contains package.json and source files
  pkgRoot: '/path/to/package',
  // Where to write the extracted modules and package.json
  outPkgRoot: './extracted',
})

// Called as the module graph is crawled
extraction.on('moduleAdded', module => {...})

// Called when an import statement points to an unknown file
extraction.on('moduleNotFound', (id, importer) => {...})

// Called after each tree-shaked module is written
extraction.on('moduleWritten', (filename, code, module) => {...})

// Called after the new package.json is written
extraction.on('packageCreated', (pkgJson) => {...})

// Called when the extraction is complete
extraction.then(() => {...})
```

&nbsp;

### Quirks

- Namespace imports prevent tree-shaking
  ```ts
  // All exports of "./foo" will be kept, even if only some are needed.
  import * as foo from './foo'
  ```

- Top-level side effects with unused return values are not preserved
  ```ts
  // × Not preserved in module copy!
  console.log('test')
  ```
