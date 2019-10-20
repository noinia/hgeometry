local hs = require "lualibhelper"

hs.hs_init()
print(hs.add_in_haskell(1, 2))
print(hs.add_in_haskell(-10, 20))
hs.hs_exit()
