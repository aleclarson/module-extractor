import * as babel from '@babel/core'
import { NodePath, types as t } from '@babel/core'
import { Binding } from '@babel/traverse'
import { getTypeBinding, TypeBinding } from 'babel-type-scopes'
import MagicString from 'magic-string'

export { babel, t, NodePath }

export function getBabelProgram(source: string, filename: string) {
  let program: NodePath<t.Program> | undefined
  const visitor: babel.Visitor = {
    Program: path => {
      program = path
      path.stop()
    },
  }

  // Use the AST for traversal, but not code generation.
  transformSync(source, filename, {
    plugins: [{ visitor }],
    sourceMaps: false,
    sourceType: 'module',
    code: false,
  })

  return program!
}

export const transformSync = (
  code: string,
  filename: string,
  config: babel.TransformOptions | babel.PluginItem[]
) => babel.transformSync(code, getBabelConfig(filename, config))

const babelTypeScriptSyntax = require.resolve('@babel/plugin-syntax-typescript')

export function inferSyntaxPlugins(filename: string): babel.PluginItem[] {
  return /\.tsx?$/.test(filename)
    ? [[babelTypeScriptSyntax, { isTSX: filename.endsWith('x') }]]
    : []
}

export function getBabelConfig(
  filename: string,
  config: babel.TransformOptions | babel.PluginItem[] = {}
): babel.TransformOptions {
  if (Array.isArray(config)) {
    config = { plugins: config }
  }
  const syntaxPlugins = inferSyntaxPlugins(filename)
  if (syntaxPlugins.length) {
    config.plugins = syntaxPlugins.concat(config.plugins || [])
  }
  return {
    filename,
    babelrc: false,
    configFile: false,
    sourceMaps: true,
    ...config,
  }
}

export function getImportDeclarations(
  program: NodePath<t.Program>
): NodePath<t.ImportDeclaration>[] {
  return program.get('body').filter(stmt => stmt.isImportDeclaration()) as any
}

export function getExportDeclarations(
  program: NodePath<t.Program>
): NodePath<t.ExportDeclaration>[] {
  return program.get('body').filter(stmt => stmt.isExportDeclaration()) as any
}

export type Imported =
  | t.ImportNamespaceSpecifier
  | t.ImportDefaultSpecifier
  | t.ImportSpecifier
  | t.ExportNamespaceSpecifier
  | t.ExportDefaultSpecifier
  | t.ExportSpecifier

export type Exported =
  | t.ExportDefaultDeclaration
  | t.ExportNamespaceSpecifier
  | t.ExportDefaultSpecifier
  | t.ExportSpecifier
  | t.VariableDeclarator
  | t.TSInterfaceDeclaration
  | t.TSTypeAliasDeclaration

export type Referenced =
  | t.Statement
  | t.ImportNamespaceSpecifier
  | t.ImportDefaultSpecifier
  | t.ImportSpecifier

const toArray = <T>(arg: T): (T extends readonly (infer U)[] ? U : T)[] =>
  Array.isArray(arg) ? arg : ([arg] as any)

export function resolveReferences(
  rootPaths: NodePath | NodePath[],
  filter = (_path: NodePath) => true,
  onError = console.error
): NodePath<Referenced>[] {
  const crawled = new Set<NodePath>()
  const referenced = new Set<NodePath<Referenced>>()

  toArray(rootPaths).forEach(crawl)

  function crawl(basePath: NodePath) {
    crawled.add(basePath)
    if (basePath.isIdentifier() || basePath.isJSXIdentifier()) {
      return onIdentifier(basePath)
    }
    basePath.traverse({
      JSXIdentifier: onIdentifier,
      Identifier: onIdentifier,
    })
  }

  function onIdentifier(path: NodePath<t.Identifier | t.JSXIdentifier>) {
    const { parentPath } = path
    if (parentPath.isJSXClosingElement() || isPropertyName(path)) {
      return
    }

    const { name } = path.node
    if (path.isJSXIdentifier() && /^[a-z]/.test(name)) {
      return
    }

    let binding: Binding | TypeBinding | undefined = path.scope.getBinding(name)
    if (!binding) {
      // For some odd reason, the "constructor" keyword always has
      // a type binding, so we need to skip it.
      if (name !== 'constructor') {
        binding = getTypeBinding(path, name)
      }
      if (!binding) {
        return // Global or undeclared variable/type
      }
    }

    let bindPath: NodePath | null = binding.path
    if (bindPath && !isReferrable(bindPath)) {
      bindPath = bindPath.getStatementParent()
    }
    if (!bindPath) {
      return onError(Error(`Failed to resolve "${name}" binding`))
    }
    if (getFirstAncestor(bindPath, p => referenced.has(p as any))) {
      return // Inside a referenced statement
    }

    addReference(bindPath)

    if (
      crawled.has(bindPath) ||
      getFirstAncestor(bindPath, p => crawled.has(p))
    ) {
      return // Already crawled
    }

    crawl(bindPath)
  }

  function addReference(path: babel.NodePath | null | undefined) {
    if (path && filter(path)) {
      if (!path.parentPath?.isImportDeclaration()) {
        path = path.getStatementParent()
      }
      if (path) {
        referenced.add(path as NodePath<Referenced>)
      }
    }
  }

  return Array.from(referenced).sort((a, b) => {
    return a.node.start! - b.node.start!
  })
}

function isReferrable(bindPath: NodePath) {
  return (
    bindPath.isStatement() ||
    bindPath.isImportSpecifier() ||
    bindPath.isImportDefaultSpecifier() ||
    bindPath.isImportNamespaceSpecifier()
  )
}

export function getFirstAncestor(
  path: NodePath,
  test: (path: NodePath) => boolean
) {
  let parentPath = path.parentPath
  while (parentPath && !test(parentPath)) {
    parentPath = parentPath.parentPath
  }
  return parentPath
}

/**
 * Is the given node path either…
 *   - the name of an object property being declared
 *   - the name of a property being accessed
 */
export function isPropertyName({ parentKey, parentPath }: NodePath) {
  return (
    parentPath &&
    ((parentPath.isObjectProperty() &&
      !parentPath.node.computed &&
      parentKey === 'key') ||
      (parentPath.isMemberExpression() &&
        !parentPath.node.computed &&
        parentKey === 'property'))
  )
}

/** Remove a `NodePath`, its preceding whitespace, and its trailing newline (if one exists). */
export function remove(path: NodePath, source: MagicString) {
  const [start, end] = getExpandedRange(path, source)
  source.remove(start, end)
  path.remove()
}

export function replaceWith(
  path: NodePath,
  replacement: string,
  source: MagicString
) {
  const [start, end] = getExpandedRange(path, source)
  source.overwrite(start, end, replacement)
}

function getExpandedRange(path: NodePath, source: MagicString) {
  let start = path.node.start!
  let end = path.node.end!
  if (path.node.leadingComments) {
    start = path.node.leadingComments.reduce(
      (start, comment) => Math.min(start, comment.start),
      start
    )
  }
  start = getWhitespaceStart(start, source.original)
  end = getTrailingLineBreak(end, source.original)
  return [start, end] as const
}

export function getWhitespaceStart(start: number, source: string) {
  return start - /(^|\n)([\n ]*)$/.exec(source.slice(0, start))![2].length
}

export function getTrailingLineBreak(end: number, source: string) {
  return source[end] === '\n' ? end + 1 : end
}
