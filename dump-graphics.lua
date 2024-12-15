local readstart = 0
local start = 0

local function on_decompress_start()
  start = emu.getregister("R1")
  readstart = emu.getregister("R0")
  print("Decompression started at 0x" .. string.format("%X", start))
end

local function on_decompress_end()
  local _end = emu.getregister("R1")
  local length = _end - start
  print("Decompression ended, difference: 0x" .. string.format("%X", length))

  local byte_array = memory.read_bytes_as_array(start, length)
  local file = io.open(string.format("./slime_graphics/%X.bin", readstart), "w")
  if file then
    file:write(string.char(table.unpack(byte_array)))
    file:close()
    print("Memory dumped successfully.")
  else
    print("Error opening file for writing.")
  end
end

event.on_bus_exec(on_decompress_start, 0x8098AC8)
event.on_bus_exec(on_decompress_end, 0x8098B72)

while true do
  emu.yield()
end
