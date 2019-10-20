local hs = _G.require "lualibhelper"

label = "HGeometry"
about = [[ Bindings to HGeometry-ipelets ]]

function run(ui, num)
   hs.hs_init()
   print(ui.doc)
   local res = hs.runHaskellIpelet(ui.pno, ui.vno)
   print(res)
   ui.creation("HGeometry ipelet", res)
   hs.hs_exit()
end

methods = {
  { label = "Run" }
}

shortcuts.ipelet_1_hgeometry = "Ctrl+Alt+H"
