// @ts-check
const { extractModules } = require('./')
const { yellow, bold, gray } = require('kleur/colors')
const path = require('path')

const extraction = extractModules({
  debug: true,
  entries: ['runtime/server/index.ts'],
  pkgRoot: '/Users/aleclarson/dev/oss/astro/packages/astro',
  outPkgRoot: path.resolve('extracted'),
  copyFiles: [
    'tsconfig.json',
    'src/@types/serialize-javascript.d.ts',
    'src/@types/shorthash.d.ts',
  ],
  copyDeps: ['typescript'],
})

extraction.on('moduleNotFound', (id, importer) => {
  console.warn(
    yellow(bold('warn')) + ' "%s" not found',
    id,
    gray(`\n     imported by ${importer}`)
  )
})
