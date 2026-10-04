# Translimeation

English romhack of Slime Mori Mori for GBA.

The human translation is `text-dumps/after-translate2.txt` (completion commit
`400d0aa`, fixes through `9460f99`). `after-translate3.txt` is a formatting-stripped
copy; `all-text-deepl-zipped.txt` is machine translation. The working dialogue
is now [professional-dialogue.txt](text-dumps/professional-dialogue.txt), with
formatting corrections against the recovered ROM script. Historical deliveries
remain unchanged.

The [formatting review](text-dumps/professional-formatting-review.json) records
303 changed entries with before/after text and reasons: missing word spaces
across color/name commands, stray padding and punctuation, misplaced or leaking
highlights, duplicate waits, and obvious typos. Eleven extraction-truncated
endings have newly supplied English, explicitly attributed as restorations.
The nine tablet reveals now share exactly the same text and glyph positions,
with visibility channels matching the original ROM. English pronouns replacing
player names were reviewed rather than mechanically changed back to names.

This is a structural audit of all 2,267 dialogue records and manual review of
the suspect formatting, not a certification of every translation. Six meaning
issues are listed separately in the review. Japanese names/placeholders and
source-only text still need translation work. Specialized layouts still need
review before a complete release.

From the repository root, with your local `slime_original.gba`:

```sh
python tools/check_professional.py
python tools/audit_text.py
sbcl --script tools/build_preview.lisp
```

This generates `slime-professional-preview.gba`, an **intentionally partial
preview**. The audit defaults to the corrected working dialogue, leaving the
historical named copy alone. The current build injects 2,212 entries; 55 fail
the conservative two-line layout check and keep their original ROM data. These
include the eight-line tablet screen and noninteractive captions; adding input
waits would change their behavior. Inspect
[text-dumps/translation-audit.json](text-dumps/translation-audit.json) and
[text-dumps/preview-layout-errors.txt](text-dumps/preview-layout-errors.txt).
The generated preview scripts are local build artifacts; the original scripts
are unchanged. `load-texts` uses the corrected working dialogue. `trans` rejects damaged controls instead of blindly injecting
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
python tools/test_professional.py
python tools/test_extraction.py
# Optional: requires the Python unicorn package
python tools/probe_text_renderer.py
```

The last check executes the original ARM renderer and verifies advances and
composed pixels. These checks do not replace a full-game emulator playtest.

To regenerate the complete original text inventory and opcode migration:

```sh
python tools/extract_text.py
python tools/audit_text.py --script text-dumps/after-translate2.txt --review-script text-dumps/after-translate2-named.txt --report /tmp/historical-translation-audit.json --preview-script /tmp/historical-preview.txt
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

[professional-credits.json](text-dumps/professional-credits.json) contains the
15 delivered credit translations in their actual row/x/text format, alongside
all 19 original cards. Coordinate bytes mistakenly printed as `ザ`/commas or
decoded as dialogue commands have been removed from the prose. The title and
added fan credit are split to fit the screen and 32-byte temporary buffer.
Four missing translations remain explicitly null. The dialogue preview does
not inject credits; they require the separate credits pointer table. Names and
romanization are retained for editorial review.

When editing `professional-dialogue.txt`, update the corresponding before/after
record and reason in `professional-formatting-review.json`. Run the formatting
checker and rebuild the preview. The checker compares execution controls with
the original ROM, verifies encoding, catches English word-boundary mistakes,
checks all tablet glyph positions/visibility masks, and validates credit bounds.
It also verifies that every change from the historical named copy is recorded.

Additional validation:

```sh
python tools/test_extraction.py
# Optional: requires unicorn; runs the ROM's actual dialogue interpreter
python tools/probe_text_opcodes.py
```

This establishes coverage of encoded text. Japanese baked into graphics still
needs a visual asset audit; the existing `tools/slime_gfx.py` handles graphics
extraction. Unreferenced dialogue is retained without assuming it is reachable.
