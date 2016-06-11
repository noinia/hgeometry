module SPG where


sumExp k ss = sum [ c*(s**k) | (c,s) <- ss]


toExp ss = [ (c,exp $ ln s)
           | (c,s) <- ss ]
