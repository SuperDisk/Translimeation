# Translimeation

English romhack of Slime Mori Mori for GBA.

The human translation is `text-dumps/after-translate2.txt` (completion commit
`400d0aa`, fixes through `9460f99`). `after-translate3.txt` is a formatting-stripped
copy; `all-text-deepl-zipped.txt` is machine translation. The human script still
needs repairs and layout review before a complete release.

From the repository root, with your local `slime_original.gba`:

```sh
python tools/audit_text.py
sbcl --script tools/build_preview.lisp
```

This generates `slime-professional-preview.gba`, an **intentionally partial
preview**. The audit recovers definite opcode arguments without rewriting the
translator's prose. Unresolved entries keep their original ROM data. Inspect
[text-dumps/translation-audit.json](text-dumps/translation-audit.json) and
[text-dumps/preview-layout-errors.txt](text-dumps/preview-layout-errors.txt).
The generated preview scripts are local build artifacts; the original scripts
are unchanged. `trans` rejects damaged controls instead of blindly injecting
`after-translate2.txt`.

The dialogue renderer already supports variable-width glyphs. Reflow now reads
the ROM's width records, preserves controls and spaces, and rejects unknown
characters. Standard dialogue uses 208px and two lines; other windows need
explicit profiles. Speaker names and credits use separate paths.

[Reverse-engineering notes](slime%20game%20reverse%20engineering.txt) contain ROM
addresses, opcode meanings, font packing, remaining translation issues, and a
concrete plan for replacing the dialogue font with independently sized glyphs.

Validation (SBCL needs no Quicklisp packages for this injector):

```sh
sbcl --script tools/test_text.lisp
python tools/test_text_tools.py
# Optional: requires the Python unicorn package
python tools/probe_text_renderer.py
```

The last check executes the original ARM renderer and verifies advances and
composed pixels. These checks do not replace a full-game emulator playtest.

To regenerate the complete original text inventory and opcode migration:

```sh
python tools/extract_text.py
python tools/audit_text.py
```

[rom-dialogue.txt](text-dumps/rom-dialogue.txt) contains every valid indexed
original dialogue slot. [rom-extra-text.txt](text-dumps/rom-extra-text.txt)
contains the additional menus, shop labels, resident names, grids and orphaned
scripts. Its numeric IDs are **ROM offsets**, not dialogue indices.
[rom-text-inventory.json](text-dumps/rom-text-inventory.json) preserves the format,
coordinates for credits, exact bytes, and reference evidence for all 2,561
records. [rom-text-coverage.json](text-dumps/rom-text-coverage.json) records the
125 banks and complete byte coverage of both identified text sections.

[after-translate2-named.txt](text-dumps/after-translate2-named.txt) is the human
script migrated to named commands, with prose retained. It still has the eleven
missing endings and four unsupported-glyph entries; use the separate partial
preview workflow above for injection. Historical dumps remain unchanged.
Current decoding/encoding uses `SCROLL`, `CLEAR`, `DELAY`, `SHOW-PROMPT`,
`WAIT-INPUT`, `YES-NO`, `OPEN-MENU`, `SWITCH-WINDOW`, `COLOR`, `DYNAMIC-TEXT`,
`PLAYER-NAME`, and `NOP`, alongside `NAME` and `NEWLINE`. Raw `BYTE` and `CONTROL`
commands are rejected by the injector. Known duplicate font glyphs use `GLYPH`.

Additional validation:

```sh
python tools/test_extraction.py
# Optional: requires unicorn; runs the ROM's actual dialogue interpreter
python tools/probe_text_opcodes.py
```

This establishes coverage of encoded text. Japanese baked into graphics still
needs a visual asset audit; the existing `tools/slime_gfx.py` handles graphics
extraction. Unreferenced dialogue is retained without assuming it is reachable.
