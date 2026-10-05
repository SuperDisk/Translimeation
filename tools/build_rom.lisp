;; Run from the repository root: sbcl --script tools/build_rom.lisp
;; Use the same font, reflow and writes as the ROM-free release build.
(require :asdf)
(uiop:run-program '("python3" "tools/build_ips.py" "--verify-rom" "slime_original.gba"
                    "--output-rom" "dist/slime.gba")
                  :output *standard-output* :error-output *error-output*)
