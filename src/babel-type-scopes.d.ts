declare module 'babel-type-scopes' {
  import { NodePath as Path, types as t } from '@babel/core'

  export type TypeBinding = {
    kind: 'import' | 'declaration' | 'expression' | 'param'
    path: Path
    id: Path
  }

  export type TypeScope =
    | t.TSTypeAliasDeclaration
    | t.TSInterfaceDeclaration
    | t.TSFunctionType
    | t.Scopable

  export function getClosestTypeScope(path: Path): Path
  export function getTypeBinding(
    path: Path,
    name: string
  ): TypeBinding | undefined
  export function getOwnTypeBindings(path: Path): Record<string, TypeBinding>
  export function isTypeScope(path: Path): path is Path<TypeScope>
}
