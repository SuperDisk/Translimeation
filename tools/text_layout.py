"""Per-entry layout policy. Builds never consult the original script or ROM."""
import struct
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
LAYOUTS = 'tools/text-layouts.txt'
READING_CODE = 'tools/reading_pause.bin'
READING_SOURCE = 'tools/reading_pause.s'
READING_HOOKS = {0x9621C: 0x08096270, 0x95FE8: 0x080961DA}
MODES = {'dialogue', 'cued', 'choice', 'static', 'fixed'}


def load_layouts(root=ROOT):
    layouts = {}
    for line in (root / LAYOUTS).read_text().splitlines():
        line = line.split('#', 1)[0].strip()
        if not line:
            continue
        span, mode, ending, cues = line.split()
        ends = list(map(int, span.split('-')))
        first, last = ends[0], ends[-1]
        cues = int(cues)
        if mode not in MODES or ending not in ('wait', 'none', 'prompt') or cues < 0 or first > last:
            raise ValueError(f'Invalid text layout: {line}')
        if mode in ('static', 'fixed') and (ending != 'none' or cues):
            raise ValueError(f'Noninteractive layout cannot wait: {line}')
        for index in range(first, last + 1):
            if index in layouts:
                raise ValueError(f'Duplicate layout for {index}')
            layouts[index] = (mode, ending, cues)
    return layouts


def validate_layouts(entries, layouts):
    for index, *tokens in entries:
        if index not in layouts:
            raise ValueError(f'{index}: choose a layout in {LAYOUTS} before building')
        mode, ending, cues = layouts[index]
        if tokens.count(['CUE']) != cues:
            raise ValueError(f'{index}: expected {cues} scene cues; preserve their order when editing')
        if mode == 'fixed':
            continue
        if any(t in (['WAIT-INPUT'], ['WAIT-FOR-A'], ['SHOW-PROMPT']) for t in tokens):
            raise ValueError(f'{index}: use CUE for a scene acknowledgement or PAGE for intentional pacing')
        if mode in ('static', 'choice') and ['PAGE'] in tokens:
            raise ValueError(f'{index}: {mode} text cannot insert reading pauses')


def lisp_layouts(layouts):
    # Indexed layout, width, height, pagination permission, automatic final wait.
    return [[i, mode, 208, 2, int(mode in ('dialogue', 'cued')), {'none': 0, 'wait': 1, 'prompt': 2}[end]]
            for i, (mode, end, _) in sorted(layouts.items())]


def pack_reading_pause(offset, root=ROOT):
    if offset & 3:
        raise ValueError('Reading-pause code must be word aligned')
    code = (root / READING_CODE).read_bytes()
    if len(code) != 180:
        raise ValueError('Reassemble reading_pause.s and update its entry offsets')
    base = 0x08000000 + offset
    return [(0x9621C, struct.pack('<I', base)),
            (0x95FE8, struct.pack('<I', base + 0x3C))], code


def verify_reading_hooks(rom):
    for offset, expected in READING_HOOKS.items():
        if struct.unpack_from('<I', rom, offset)[0] != expected:
            raise ValueError(f'Unexpected dialogue dispatch slot at {offset:06x}')
