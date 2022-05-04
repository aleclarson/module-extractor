import {
  t,
  NodePath,
  getBabelProgram,
  getExportDeclarations,
  getImportDeclarations,
  resolveReferences,
  Exported,
  Imported,
  Referenced,
  remove,
  replaceWith,
} from './babel'
import MagicString from 'magic-string'
import { EventEmitter } from 'events'
import { StrictEventEmitter } from 'strict-event-emitter-types'
import resolve from 'resolve'
import fs from 'saxon/sync'
import path from 'path'

export interface ExtractModuleOptions {
  /**
   * Module paths (relative to `srcRoot`) that are intended to
   * be the entry point(s) to the new package.
   */
  entries: string[]
  /**
   * The `src` directory shared by all entry modules.
   * Relative to the `pkgRoot` option.
   * @default "src"
   */
  srcRoot?: string
  /**
   * Where the associated `package.json` lives
   */
  pkgRoot: string
  /**
   * Which properties to copy from `package.json`
   * @default name|version|description|license|author|contributors|keywords|engines
   */
  pkgProps?: string[]
  /**
   * Which files to copy verbatim from the `pkgRoot`
   */
  copyFiles?: string[]
  /**
   * Which dependencies to copy even if never used
   */
  copyDeps?: string[]
  /**
   * Where to write source files.
   * Relative to the `outPkgRoot` option.
   * @default "src"
   */
  outSrcRoot?: string
  /** Where to emit `package.json` */
  outPkgRoot: string
  /** Skip writing */
  dryRun?: boolean
  /** Enable debug logs */
  debug?: boolean
}

interface Events {
  moduleAdded(module: Module): void
  moduleNotFound(id: string, importer: string): void
  moduleWritten(outPath: string, code: string, module: Module): void
  packageCreated(outPkg: Record<string, any>): void
}

export interface ModuleExtraction
  extends PromiseLike<void>,
    StrictEventEmitter<EventEmitter, Events> {
  modules: Map<string, Module>
  writtenModules: Map<string, string>
}

const localPathRE = /^\.\.?(\/|$)/

export function extractModules({
  pkgRoot,
  srcRoot = 'src',
  outPkgRoot,
  outSrcRoot = 'src',
  pkgProps = DEFAULT_PKG_PROPS,
  entries,
  copyFiles: filesToCopy = [],
  copyDeps: depsToCopy = [],
  dryRun,
  debug,
}: ExtractModuleOptions): ModuleExtraction {
  const debugLog = debug ? console.log : null!
  const kleur = debug
    ? (require('kleur/colors') as typeof import('kleur/colors'))
    : null!

  pkgRoot = path.resolve(pkgRoot)
  srcRoot = path.resolve(pkgRoot, srcRoot)
  outPkgRoot = path.resolve(outPkgRoot)
  outSrcRoot = path.resolve(outPkgRoot, outSrcRoot)

  const extraction = new EventEmitter() as ModuleExtraction
  const modules = (extraction.modules = new Map<string, Module>())
  const modulesByImportDecl = new Map<NodePath, Module>()
  const externalDeps = new Map<string, Module[]>()

  function addModule(modulePath: string, isEntry = false) {
    const moduleId = path.relative(srcRoot, modulePath)

    let module = modules.get(moduleId)
    if (module) {
      return module
    }

    const code = fs.read(modulePath)
    const ast = getBabelProgram(code, modulePath)
    const imports = getImportDeclarations(ast)
    const exports = getExportDeclarations(ast)
    const namedExports = exports.reduce((exports, decl) => {
      if (decl.isExportDefaultDeclaration()) {
        exports.default = decl
      } else if (decl.isExportNamedDeclaration()) {
        const exportedDecl = decl.get('declaration')
        if (exportedDecl.isDeclaration()) {
          if (exportedDecl.isVariableDeclaration()) {
            for (const varDecl of exportedDecl.get('declarations')) {
              const name = (varDecl.node.id as t.Identifier).name
              exports[name] = varDecl
            }
          } else {
            const { name } = (exportedDecl.node as t.FunctionDeclaration).id!
            exports[name] = exportedDecl as any
          }
        } else {
          for (const spec of decl.get('specifiers')) {
            const name = (spec.node.exported as t.Identifier).name
            exports[name] = spec
          }
        }
      }
      return exports
    }, {} as Record<string, NodePath<Exported>>)

    module = {
      id: moduleId,
      path: modulePath,
      code,
      ast,
      imports: new Map(),
      exports: { ...namedExports },
      usedExports: {},
      noTreeShake: false,
      isEntry,
    }

    modules.set(moduleId, module)
    extraction.emit('moduleAdded', module)

    // Find local module dependencies.
    const localDeps = new Map<NodePath, string>()
    for (const decl of imports) {
      const source = decl.get('source') as NodePath
      if (source.isStringLiteral()) {
        const sourcePath = source.node.value
        if (localPathRE.test(sourcePath)) {
          localDeps.set(decl, sourcePath)
        } else {
          const importers = externalDeps.get(sourcePath) || []
          externalDeps.set(sourcePath, importers.concat(module))
        }
      }
    }
    for (const decl of exports) {
      const source = decl.get('source') as NodePath
      if (source.isStringLiteral()) {
        const sourcePath = source.node.value
        if (localPathRE.test(sourcePath)) {
          localDeps.set(decl, sourcePath)
        } else {
          const importers = externalDeps.get(sourcePath) || []
          externalDeps.set(sourcePath, importers.concat(module))
        }
      }
    }

    for (const [decl, id] of localDeps) {
      try {
        let depPath = path.resolve(modulePath, '..', id)
        if (!fs.isFile(depPath)) {
          if (depPath.endsWith('.js')) {
            depPath = depPath.replace(/\.js$/, '')
          }
          depPath = resolve.sync(depPath, {
            basedir: path.dirname(modulePath),
            extensions: ['.ts', '.tsx', '.js', '.jsx', '.d.ts'],
          })
        }
        const dep = addModule(depPath)
        if (decl.isExportAllDeclaration()) {
          Object.assign(module.exports, dep.exports, namedExports)
          modulesByImportDecl.set(decl, dep)
        } else if (decl.isExportNamedDeclaration() && decl.node.source) {
          modulesByImportDecl.set(decl, dep)
        } else if (decl.isImportDeclaration()) {
          const decls = module.imports.get(dep) || []
          module.imports.set(dep, decls.concat(decl))
          modulesByImportDecl.set(decl, dep)
        }
      } catch (e: any) {
        if (
          e.code == 'ENOENT' ||
          e.code == 'MODULE_NOT_FOUND' ||
          e.code == 'EISDIR'
        ) {
          extraction.emit('moduleNotFound', id, modulePath)
        } else {
          throw e
        }
      }
    }

    return module
  }

  const writtenModules = (extraction.writtenModules = new Map<string, string>())
  const writeModule = (module: Module, code = module.code) => {
    const outPath = path.join(outSrcRoot, module.id)
    if (!dryRun) {
      fs.mkdir(path.dirname(outPath))
      fs.write(outPath, code)
    }

    writtenModules.set(module.id, code)
    extraction.emit('moduleWritten', outPath, code, module)
  }

  function getImportedModule(
    importer: Module,
    importDecl: NodePath<t.ImportDeclaration | t.ExportDeclaration>
  ) {
    const source = importDecl.get('source') as NodePath<t.StringLiteral | null>
    if (!source.node) {
      return null
    }
    const sourcePath = source.node.value
    if (!localPathRE.test(sourcePath)) {
      return null
    }
    const dep = modulesByImportDecl.get(importDecl)
    if (!dep) {
      throw Error(`Missing "${sourcePath}" dependency`)
    }
    return dep
  }

  const promise = new Promise<void>(resolve => {
    process.nextTick(() => {
      // 1. Resolve imported modules
      const entryModules = entries.map(entryId =>
        addModule(path.join(srcRoot, entryId), true)
      )

      // 2. Detect which exports are used
      const entryDeps = new Set<Module>()
      for (const entryModule of entryModules) {
        disableTreeShake(entryModule)
        entryModule.imports.forEach((decls, dep) => {
          entryDeps.add(dep)
          outer: for (const decl of decls) {
            for (const spec of decl.get('specifiers')) {
              preserveImport(spec, dep)
              if (dep.noTreeShake) {
                break outer
              }
            }
          }
        })
        for (const name in entryModule.exports) {
          const exported = entryModule.exports[name]
          const exportDecl = exported.getStatementParent()
          if (
            exportDecl &&
            (exportDecl.isExportAllDeclaration() ||
              exportDecl.isExportNamedDeclaration())
          ) {
            const dep = getImportedModule(entryModule, exportDecl)
            if (dep) {
              preserveImport(exported as NodePath<Imported>, dep)
              entryDeps.add(dep)
            }
          }
        }
      }

      debug &&
        debugLog(
          '\nEntry dependencies:',
          Array.from(entryDeps, dep => '\n  ' + kleur.green(dep.id)).join('')
        )

      for (const dep of entryDeps) {
        traverseExports(dep)
      }

      const extractedModules = [
        ...entryModules,
        ...traversedExportsByModule.keys(),
      ]

      // 3. Extract modules, remove any unused code
      extractModules(extractedModules)

      // At least one of the extracted modules must import an external
      // module before its package is added to the new package.json
      for (const [id, importers] of externalDeps) {
        const required = extractedModules.some(module =>
          importers.includes(module)
        )
        if (!required) {
          externalDeps.delete(id)
        }
      }

      debug &&
        debugLog(
          '\nExternal dependencies:',
          Array.from(externalDeps.keys(), dep => {
            return '\n  ' + kleur.cyan(dep)
          }).join('')
        )

      // 4. Create a new package.json
      const inPkg = require(path.join(pkgRoot, 'package.json'))
      const outPkg: any = {}
      for (const prop of pkgProps) {
        const val = inPkg[prop]
        if (val !== undefined) {
          outPkg[prop] = val
        }
      }
      const depTypes = ['dependencies', 'devDependencies', 'peerDependencies']
      outer: for (const [id, importers] of externalDeps) {
        // The `externalDeps` collection may contain nested module paths,
        // not just package names. Let's keep removing the last path part
        // until none are left or the package name is found.
        for (let pkgId = id; pkgId !== '.'; pkgId = path.dirname(pkgId)) {
          for (const depType of depTypes) {
            const version = inPkg[depType]?.[pkgId]
            if (version) {
              outPkg[depType] ??= {}
              outPkg[depType][pkgId] = version
              if (!pkgId.startsWith('@types/')) {
                depsToCopy.push(
                  '@types/' +
                    (pkgId[0] == '@'
                      ? pkgId.slice(1).replace('/', '__')
                      : pkgId)
                )
              }
              continue outer
            }
          }
        }
        extraction.emit('moduleNotFound', id, importers[0].id)
      }
      outer: for (const pkgId of depsToCopy) {
        for (const depType of depTypes) {
          const version = inPkg[depType]?.[pkgId]
          if (version) {
            outPkg[depType] ??= {}
            outPkg[depType][pkgId] = version
            continue outer
          }
        }
      }
      extraction.emit('packageCreated', outPkg)
      const outPkgPath = path.join(outPkgRoot, 'package.json')
      if (!dryRun) {
        fs.write(outPkgPath, JSON.stringify(outPkg, null, 2))
        for (const file of filesToCopy) {
          try {
            fs.copy(path.join(pkgRoot, file), path.join(outPkgRoot, file))
          } catch (e: any) {
            e.message =
              `Failed to copy "${file}" from package root. ` + e.message
            console.error(e)
          }
        }
      }

      resolve()
    })
  })

  const moduleRefs = new Map<Module, Set<NodePath<Referenced>>>()
  const traversedExportsByModule = new Map<Module, Set<string>>()

  function traverseExports(module: Module) {
    let traversedExports = traversedExportsByModule.get(module)!
    if (!traversedExports) {
      traversedExportsByModule.set(module, (traversedExports = new Set()))
    }

    let usedExportIds = module.noTreeShake
      ? Object.keys(module.exports)
      : Object.keys(module.usedExports)

    usedExportIds = usedExportIds.filter(name => {
      if (!traversedExports.has(name)) {
        traversedExports.add(name)
        return true
      }
    })

    if (!usedExportIds.length) {
      return
    }

    debug &&
      debugLog(
        `\nTraversing exports of "${module.id}"`,
        usedExportIds.map(e => '\n  ' + kleur.yellow(e)).join('')
      )

    const reExportedDecls: NodePath<Exported>[] = []
    const usedExports = usedExportIds
      .map(name => {
        return module.exports[name]
      })
      .filter(decl => {
        if (module.ast.isAncestor(decl)) return true
        reExportedDecls.push(decl)
        return false
      })

    const refs = resolveReferences(usedExports, path => {
      const inExportDecl = usedExports.some(decl => decl.isAncestor(path))
      return !inExportDecl
    })

    // Save needed statements for the trimming phase.
    const usedExportStmts = usedExports.map(decl => decl.getStatementParent()!)
    moduleRefs.set(module, new Set(refs.concat(usedExportStmts)))

    // Track which modules are referenced by our used exports.
    const referencedDeps = new Set<Module>()
    for (const ref of refs) {
      if (ref.isExportDeclaration()) {
        continue
      }
      const stmtParent = ref.getStatementParent()
      if (stmtParent?.isImportDeclaration()) {
        const dep = getImportedModule(module, stmtParent)
        if (dep) {
          preserveImport(ref as NodePath<Imported>, dep)
          referencedDeps.add(dep)
        }
      }
    }

    if (!referencedDeps.size) {
      debug && debugLog('\n%s used no modules', module.id)
      return
    }

    debug &&
      debugLog(
        '\n%s used %d modules:',
        module.id,
        referencedDeps.size,
        Array.from(referencedDeps, dep => '\n  ' + kleur.green(dep.id)).join('')
      )

    for (const dep of referencedDeps) {
      traverseExports(dep)
    }
  }

  function extractModules(extractedModules: Module[]) {
    debug &&
      debugLog(
        '\nExtracted modules:%s',
        extractedModules
          .sort((a, b) => (a.id < b.id ? -1 : 1))
          .map(module => '\n  ' + kleur.green(module.id))
          .join('')
      )

    for (const module of extractedModules) {
      if (module.isEntry) {
        writeModule(module)
        continue
      }

      const usedSpecs: NodePath<Imported>[] = []
      const usedStmts = Array.from(moduleRefs.get(module)!, ref => {
        const stmt = ref.getStatementParent()!
        if (stmt.isImportDeclaration()) {
          usedSpecs.push(ref as NodePath<Imported>)
        }
        return stmt
      })

      const editor = new MagicString(module.code)
      for (const stmt of module.ast.get('body')) {
        // Remove unused statements.
        if (!usedStmts.includes(stmt)) {
          remove(stmt, editor)
        }
        // Remove unused import specifiers.
        else if (
          stmt.isImportDeclaration() ||
          (stmt.isExportNamedDeclaration() && stmt.node.source)
        ) {
          const specs = stmt.get('specifiers') as NodePath<Imported>[]
          for (const spec of specs) {
            if (!usedSpecs.includes(spec)) {
              spec.remove()
            }
          }
          stmt.node.trailingComments = null
          replaceWith(stmt, stmt.toString() + '\n', editor)
        }
      }
      writeModule(module, editor.toString())
    }
  }

  extraction.then = promise.then.bind(promise)
  return extraction
}

export interface Module {
  id: string
  path: string
  code: string
  ast: NodePath<t.Program>
  imports: Map<Module, NodePath<t.ImportDeclaration>[]>
  exports: Record<string, NodePath<Exported>>
  usedExports: Record<string, NodePath<Imported>[]>
  noTreeShake: boolean
  isEntry: boolean
}

function disableTreeShake(module: Module) {
  module.noTreeShake = true
  module.usedExports = null!
}

function preserveImport(spec: NodePath<Imported>, dep: Module) {
  if (dep.noTreeShake) {
    return // Probably importing from an entry module.
  }
  if (spec.isImportDefaultSpecifier() || spec.isExportDefaultSpecifier()) {
    const users = dep.usedExports.default || []
    dep.usedExports.default = users.concat(spec)
  } else if (
    spec.isImportNamespaceSpecifier() ||
    spec.isExportNamespaceSpecifier()
  ) {
    // TODO: track namespace access
    disableTreeShake(dep)
  } else if (spec.isImportSpecifier() || spec.isExportSpecifier()) {
    const imported = (
      spec.isImportSpecifier() ? spec.node.imported : spec.node.local
    ) as t.Identifier
    const users = dep.usedExports[imported.name] || []
    dep.usedExports[imported.name] = users.concat(spec)
  }
}

export const DEFAULT_PKG_PROPS = [
  'name',
  'version',
  'description',
  'license',
  'author',
  'contributors',
  'keywords',
  'engines',
]
