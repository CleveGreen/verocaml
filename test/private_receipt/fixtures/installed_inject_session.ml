let inject (session : Verification_session.t) =
  Interface_specification.verify_consumer ~timeout_ms:1 ~dependency_files:[]
    ~consumer_file:"consumer.cmt" session
