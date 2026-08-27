let runtime x = x + 1
let bad x = let[@ghost] y = (runtime x [@ghost]) in x
