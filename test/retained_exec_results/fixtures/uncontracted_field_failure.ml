[@@@verocaml.verify]

let body_fact value =
  [%verocaml.ensures fun result -> result = 44];
  (Provider.uncontracted_record value).Provider.unstated
