(lang dune 3.21)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This file is part of the Codex semantics library                      ;;
;;    (Union-find lattice subcomponent).                                  ;;
;;                                                                        ;;
;;  Copyright (C) 2026                                                    ;;
;;    CEA (Commissariat à l'énergie atomique et aux énergies              ;;
;;         alternatives)                                                  ;;
;;                                                                        ;;
;;  you can redistribute it and/or modify it under the terms of the GNU   ;;
;;  Lesser General Public License as published by the Free Software       ;;
;;  Foundation, version 2.1.                                              ;;
;;                                                                        ;;
;;  It is distributed in the hope that it will be useful,                 ;;
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of        ;;
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;;
;;  GNU Lesser General Public License for more details.                   ;;
;;                                                                        ;;
;;  See the GNU Lesser General Public License version 3.0                 ;;
;;  for more details (enclosed in the file LICENSE).                      ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(using mdx 0.2)

(name union-find-lattice)
(version 0.1.0)

(generate_opam_files true)

(source (github codex-semantics-library/union-find-lattice))
(bug_reports "https://github.com/codex-semantics-library/union-find-lattice/issues")
(homepage "https://codex.top/api/union-find-lattice")
(license "lgpl-2.1-or-later")
(documentation "https://codex.top/api/union-find-lattice")

(authors 
 "Dorian Lesbre <dorian.lesbre@cea.fr>" 
 "Matthieu Lemerre <matthieu.lemerre@cea.fr>")
(maintainers
 "Dorian Lesbre <dorian.lesbre@cea.fr>"
 "Matthieu Lemerre <matthieu.lemerre@cea.fr>")

(package
 (name union-find-lattice)
 (synopsis
  "Persistent union-find data-structures with lattice operations (order, meet, join)")
 (depends
  ocaml
  (patricia-tree (>= 0.14.0))
  (qcheck (and (> 0.90.0) :with-test))
  (mdx :with-test)
  (zarith :with-test)
  (csv :with-dev-setup)
  (ISO8601 :with-dev-setup))
 (tags
  ("union-find" "persistent" "lattice" "join" "meet")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
