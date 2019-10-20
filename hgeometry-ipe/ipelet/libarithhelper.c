#include "LibArith_stub.h"
#include "lua.h"
#include "stdio.h"

int hs_init_lua(lua_State *L)
{
  hs_init(NULL, NULL);
  return 0;
}

int hs_exit_lua(lua_State *L)
{
  hs_exit();
  return 0;
}

int luaopen_lualibhelper(lua_State *L)
{
  lua_newtable(L);

  lua_pushcfunction(L, (int (*)(lua_State*))add);
  lua_setfield(L, -2, "add_in_haskell");

  lua_pushcfunction(L, (int (*)(lua_State*))run);
  lua_setfield(L, -2, "runHaskellIpelet");

  lua_pushcfunction(L, hs_init_lua);
  lua_setfield(L, -2, "hs_init");

  lua_pushcfunction(L, hs_exit_lua);
  lua_setfield(L, -2, "hs_exit");

  return 1;
}
