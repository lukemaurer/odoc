  $ ocamlc -c -bin-annot a.mli
  $ odoc compile a.cmti
  $ odoc link a.odoc
  File "a.odoc":
  Warning: While resolving the expansion of include at File "a.mli", line 25, character 0
  While resolving the expansion of include at File "a.mli", line 20, character 4
  Failed to lookup type identifier((root A).{A}1, true).t Parent_module: Lookup failure (module): (root A).{A}1
  File "a.odoc":
  Warning: While resolving the expansion of include at File "a.mli", line 25, character 0
  Failed to resolve module path identifier((root A).{A}2, true) Lookup failure (module): (root A).{A}2
  File "a.odoc":
  Warning: While resolving the expansion of include at File "a.mli", line 20, character 4
  Failed to lookup type identifier((root A).{A}1, true).t Parent_module: Lookup failure (module): (root A).{A}1
  File "a.odoc":
  Warning: Failed to resolve module path identifier((root A).{A}2, true) Lookup failure (module): (root A).{A}2
  File "a.odoc":
  Warning: While resolving the expansion of include at File "a.mli", line 16, character 0
  Failed to resolve module path identifier((root A).{A}1, true) Lookup failure (module): (root A).{A}1
  File "a.odoc":
  Warning: Failed to resolve module path identifier((root A).{A}1, true) Lookup failure (module): (root A).{A}1

