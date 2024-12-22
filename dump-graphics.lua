-- Constants for palette RAM
local BG_PALETTE_RAM_START = 0x05000000
local BG_PALETTE_RAM_END = 0x050001FF

function tablelength(T)
  local count = 0
  for _ in pairs(T) do count = count + 1 end
  return count
end

-- Function to extract individual colors from a BG palette
local function extract_bg_palettes()
  local raw_bytes = memory.read_bytes_as_array(BG_PALETTE_RAM_START, BG_PALETTE_RAM_END - BG_PALETTE_RAM_START + 1)
  local palettes = {}

  for i = 1, #raw_bytes, 2 do
    local color = raw_bytes[i] + (raw_bytes[i + 1] << 8)
    table.insert(palettes, color)
  end

  return palettes
end

-- Helper function to union two sets of palettes
local function union_palettes(existing_palettes, new_palettes)
  local palette_set = {}
  for _, color in ipairs(existing_palettes) do
    palette_set[color] = true
  end
  for _, color in ipairs(new_palettes) do
    palette_set[color] = true
  end
  return palette_set
end

-- Extend active_regions to store palettes
local active_regions = {}

local function on_clobber_bg(addr, val, flags)
  print(active_regions[addr].palettes)
  print(string.format("Clobbered bg: %X %d", addr, tablelength(active_regions[addr].palettes)))
  event.unregisterbyname(string.format("%X", addr))
  active_regions[addr] = nil
end

local function on_decompress_start()
  start = emu.getregister("R1")
  readstart = emu.getregister("R0")
  -- print("Decompression started at 0x" .. string.format("%X", start))
end

local function on_decompress_end()
  local _end = emu.getregister("R1")
  local length = (_end - start) + 2
  -- print("Decompression ended, difference: 0x" .. string.format("%X", length))

  local byte_array = memory.read_bytes_as_array(start, length)
  local file_path = string.format("./slime_graphics/%X.bin", readstart)
  local file = io.open(file_path, "wb")
  if file then
    file:write(string.char(table.unpack(byte_array)))
    file:close()
    -- print("Memory dumped successfully to " .. file_path)
  else
    print("Error opening file for writing.")
  end

  -- Store the active region along with the current BG palettes
  active_regions[start] = {
    region_end = _end,
    map = {},
    palettes = {
      0 = {},
      1 = {},
      2 = {},
      3 = {},
      4 = {},
      5 = {},
      6 = {},
      7 = {},
      8 = {},
      9 = {},
      10 = {},
      11 = {},
      12 = {},
      13 = {},
      14 = {},
      15 = {},
    }
  }

  event.on_bus_write(on_clobber_bg, start, string.format("%X", start))
end

local function on_frame_start()
  local current_palettes = extract_bg_palettes()

  for region_start, data in pairs(active_regions) do
    data.palettes = union_palettes(data.palettes, current_palettes)
  end
end

-- Register events
event.on_bus_exec(on_decompress_start, 0x8098AC8)
event.on_bus_exec(on_decompress_end, 0x8098B72)
event.onframestart(on_frame_start)

-- Main loop
while true do
  emu.yield()
end
