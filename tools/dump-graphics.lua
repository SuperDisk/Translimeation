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

  for slot = 0, 15 do
    palettes[slot] = {}
  end

  for i = 1, #raw_bytes, 2 do
    local color = raw_bytes[i] + (raw_bytes[i + 1] << 8)
    local slot = math.floor((i - 1) / 32)
    table.insert(palettes[slot], color)
  end

  return palettes
end

-- Helper function to union two sets of palettes
local function union_palettes(existing_palettes, new_palettes)
  for slot = 0, 15 do
    local slot_set = {}
    for _, color in ipairs(existing_palettes[slot]) do
      slot_set[color] = true
    end
    for _, color in ipairs(new_palettes[slot]) do
      slot_set[color] = true
    end

    local union_result = {}
    for color, _ in pairs(slot_set) do
      table.insert(union_result, color)
    end
    existing_palettes[slot] = union_result
  end

  return existing_palettes
end

-- Extend active_regions to store palettes
local active_regions = {}

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
    map = 0,
    palettes = {}
  }

  for slot = 0, 15 do
    active_regions[start].palettes[slot] = {}
  end

  -- print(string.format("Installing clobber handler: %X", readstart))
  -- event.on_bus_write(
  --   function(addr, val, flags)
  --     event.unregisterbyname(string.format("%X", start))

      -- print(string.format("Clobbered bg: %X", readstart))

      -- local file_path = string.format("./slime_graphics/%X.pal", readstart)
      -- local file = io.open(file_path, "wb")
      -- if file then
      --   local palettes = active_regions[addr].palettes
      --   for slot = 0, 15 do
      --     if palettes[slot] then
      --       for _, color in ipairs(palettes[slot]) do
      --         file:write(string.char(color & 0xFF, (color >> 8) & 0xFF))
      --       end
      --     end
      --   end
      --   file:close()
      --   -- print(string.format("Palette dumped successfully to %s", file_path))
      -- else
      --   print("Error opening file for writing.")
      -- end

      -- active_regions[addr] = nil
    -- end,
    -- start,
    -- string.format("%X", start))
end

local function on_frame_start()
  -- local current_palettes = extract_bg_palettes()

  -- for region_start, data in pairs(active_regions) do
    -- data.palettes = union_palettes(data.palettes, current_palettes)
  -- end

  -- for reg = 0x4000008, 0x400000E, 2 do
  --   local val = memory.read_s16_le(reg)
  --   local char_block = (val >> 2) & 0x3
  --   local screen_block = (val >> 8) & 0x1F

  --   char_block = 0x6000000 + (char_block * (1024*16))
  --   screen_block = 0x6000000 + (screen_block * (1024*2))

  --   for region_start, data in pairs(active_regions) do
  --     if char_block == region_start then
  --       data.map = screen_block
  --       print(string.format("found! %x has map %x", region_start, screen_block))
  --     end
  --   end
  -- end
end

-- Register events
event.on_bus_exec(on_decompress_start, 0x8098AC8)
event.on_bus_exec(on_decompress_end, 0x8098B72)
event.onframestart(on_frame_start)

-- Main loop
while true do
  emu.yield()
end
