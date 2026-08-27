let trusted x = [%verocaml.requires x >= 0]; x + 1
[@@verocaml.external_body]
