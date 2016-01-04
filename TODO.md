make sure all resources have a corresponding _version mapped to
/resource/versions

  - expose parameters of exactly: id, xxx_id, limit, offset, sort

Find all GET methods that return an array
  -- validate that there is a corresponding GET /xxxx/versions method
     -- validate return type is xxx_version
