-- the first program in every language

function go(a)
io.write("Hello world, from ",a,"!\n")
return 3, 4, 5
end

local z, x = go(_VERSION)

local a, b, c, d, e = go("Lua-D")
io.write("got ",a, " ", b,"\n")
