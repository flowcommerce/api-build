Keep all _version models w/ same structure - 3 leading fields, then a
typed one

	"user_version": {
	    "fields": [
		{ "name": "id", "type": "string" },
		{ "name": "timestamp", "type": "date-time-iso8601" },
		{ "name": "type", "type": "change_type" },
		{ "name": "user", "type": "user" }
	    ]
	},


make sure all resources have a corresponding _version mapped to
/resource/versions

  - expose parameters of exactly: id, xxx_id, limit, offset, sort

for models w/ organization, ensure organization is second field (after id)

for resources w/ sort parameter:
  - default to lower(name), created_at if there is a name field
  - otherwise default to created_at

for response 401 - verify type: unit, description: "unauthorized request"

validate codes:
  GET: 200, 401
  POST: 200,401, 404, 422
  PUT: 200,401, 404, 422
  DELETE: 204 (unit), 401, 404
