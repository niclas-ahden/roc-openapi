## Request bodies of type `application/x-www-form-urlencoded`, such as
## `name=gadget&tags=new&tags=sale`, read into the value the body's schema
## describes, so that they are validated like JSON bodies.
##
## A field's text is read as what its schema takes: a number, a boolean, the
## text itself, null for empty text, or a list of one. A name given more than
## once makes a list. Brackets nest, as Stripe sends them, so
## `items[0][price]=p1&metadata[order]=7` is
## `{"items": [{"price": "p1"}], "metadata": {"order": "7"}}`, where the
## schema says `items` is an array. `expand[]=a` is the same as `expand=a`.
import JsonValue
import Schema
import url.Uri

# A field of a form: the segments of its name, such as ["items", "0", "price"]
# for `items[0][price]`, and its text.
Field : { segments : List(Str), text : Str }

# A form as a tree, before its schema says what the text in it stands for.
Tree := [Texts(List(Str)), Fields(List({ key : Str, value : Tree }))]

# The schemas a part of a form has to match, and every schema they stand for
# through `allOf`, `anyOf`, `oneOf` and `$ref`, which say what types it may
# have.
Shape : { schemas : List(JsonValue), branches : List(JsonValue) }

Form :: [].{

	## Read a form body into a value for the schemas in `schemas`, which are
	## from `doc`, nesting at most `max_depth` deep. With no schema, every
	## field is text.
	decode : List(U8), List(JsonValue), Schema.Doc, U64 -> Try(JsonValue, [InvalidForm(Str)])
	decode = |body, schemas, doc, max_depth| {
		text = Str.from_utf8(body) ? |BadUtf8({ index, problem: _ })| InvalidForm("Invalid UTF-8 at byte ${index.to_str()}")
		fields = parse_fields(text)?
		if fields.any(|field| field.segments.len() > max_depth) {
			return Err(too_deep(max_depth))
		}

		tree = build(fields, [])?
		value = read(tree, shape_of(schemas, doc), doc)
		if value.depth() > max_depth {
			Err(too_deep(max_depth))
		} else {
			Ok(value)
		}
	}
}

too_deep : U64 -> [InvalidForm(Str)]
too_deep = |max_depth| InvalidForm("Nested deeper than ${max_depth.to_str()} levels")

# Parsing

# The fields of a form in the order they come. `a=1&&b` has two fields, and
# `b` is the empty text.
parse_fields : Str -> Try(List(Field), [InvalidForm(Str)])
parse_fields = |text| {
	var $fields = []
	for pair in text.split_on("&") {
		if !pair.is_empty() {
			{ before: raw_name, after: raw_text } = pair.split_first("=").ok_or({ before: pair, after: "" })
			name = unescape(raw_name)?
			$fields = $fields.append({ segments: name_segments(name), text: unescape(raw_text)? })
		}
	}
	Ok($fields)
}

# A "+" is a space, and a "%" starts the hex code of a byte.
unescape : Str -> Try(Str, [InvalidForm(Str)])
unescape = |escaped|
	Uri.percent_decode(escaped.replace_each("+", " ")).map_err(|InvalidEncoding| InvalidForm("Invalid percent-encoding in ${escaped}"))

# `items[0][price]` is ["items", "0", "price"]. A trailing `[]` adds nothing,
# since a name given more than once makes a list anyway. A name that is not
# made of a name and brackets is a single segment, as it is.
name_segments : Str -> List(Str)
name_segments = |name|
	match name.split_first("[") {
		Ok({ before, after }) if !before.is_empty() and after.ends_with("]") => {
			inside = after.drop_suffix("]").split_on("][")
			if inside.any(|segment| segment.contains("[") or segment.contains("]")) {
				[name]
			} else {
				segments = [before].concat(inside)
				if segments.last() == Ok("") segments.drop_last(1) else segments
			}
		}

		_ => [name]
	}

# The tree of the fields under `path`, with their names from there on. Fields
# that share a segment go together, in the order the first of them comes.
build : List(Field), List(Str) -> Try(Tree, [InvalidForm(Str)])
build = |fields, path| {
	ended = fields.keep_if(|field| field.segments.is_empty())
	if ended.is_empty() {
		var $children = []
		for group in group_by_first_segment(fields) {
			$children = $children.append({ key: group.key, value: build(group.fields, path.append(group.key))? })
		}
		Ok(Tree.(Fields($children)))
	} else if ended.len() == fields.len() {
		Ok(Tree.(Texts(ended.map(|field| field.text))))
	} else {
		Err(InvalidForm("Both a value and fields at ${JsonValue.pointer(path)}"))
	}
}

group_by_first_segment : List(Field) -> List({ key : Str, fields : List(Field) })
group_by_first_segment = |fields| {
	grouped = fields.fold(
		{ groups: [], index: Dict.empty() },
		|so_far, field|
			match field.segments {
				[key, .. as rest] => {
					inner = { segments: rest, text: field.text }
					match so_far.index.get(key) {
						Ok(at) => { ..so_far, groups: so_far.groups.update(at, |group| { ..group, fields: group.fields.append(inner) }).ok_or(so_far.groups) }
						Err(KeyNotFound) => { groups: so_far.groups.append({ key, fields: [inner] }), index: so_far.index.insert(key, so_far.groups.len()) }
					}
				}

				[] => so_far
			},
	)
	grouped.groups
}

# Reading

read : Tree, Shape, Schema.Doc -> JsonValue
read = |Tree.(tree), shape, doc|
	match tree {
		Texts([text]) => read_text(text, shape, doc, Bool.True)
		Texts(texts) => JsonValue.(Array(texts.map(|text| read_text(text, item_shape(shape, doc), doc, Bool.False))))
		Fields(fields) =>
			match indexed(fields) {
				Ok(items) if types_of(shape).contains("array") => JsonValue.(Array(items.map(|item| read(item, item_shape(shape, doc), doc))))
				_ => JsonValue.(Object(fields.map(|{ key, value }| { key, value: read(value, field_shape(shape, key, doc), doc) })))
			}
	}

# The values of fields named 0, 1, 2 and so on, in that order. Numbers may be
# missing, as in `items[0]` and `items[2]`, but each must be written plainly,
# without a sign or leading zeros.
indexed : List({ key : Str, value : Tree }) -> Try(List(Tree), [NotIndexed])
indexed = |fields| {
	var $items = []
	for { key, value } in fields {
		index = U64.from_str(key) ? |_| NotIndexed
		if index.to_str() != key {
			return Err(NotIndexed)
		}
		$items = $items.append({ index, value })
	}
	Ok($items.sort_by(|item| item.index).map(|item| item.value))
}

# The value a field's text stands for: the first of a number, a boolean, the
# text itself, null for empty text, and a list of one, that the field's
# schemas take. When none does, it is the first whose type the schemas name,
# else the text, so that the error says what is wrong with the text.
read_text : Str, Shape, Schema.Doc, Bool -> JsonValue
read_text = |text, shape, doc, may_be_list| {
	types = types_of(shape)
	readings = List.join([
		number_reading(text),
		if text == "true" or text == "false" [JsonValue.(Bool(text == "true"))] else [],
		[JsonValue.(String(text))],
		if text.is_empty() [JsonValue.(Null)] else [],
		if may_be_list and types.contains("array") [JsonValue.(Array([read_text(text, item_shape(shape, doc), doc, Bool.False)]))] else [],
	])
	match readings.find_first(|reading| shape.schemas.any(|schema| Schema.validate(reading, schema, doc).is_empty())) {
		Ok(reading) => reading
		Err(_) => readings.find_first(|reading| names_type(types, reading)).ok_or(JsonValue.(String(text)))
	}
}

# Text that is a JSON number, such as `30` or `-1.5e3`, and nothing more.
number_reading : Str -> List(JsonValue)
number_reading = |text|
	match JsonValue.parse(text.to_utf8()) {
		Ok(value) if value.get_number().is_ok() and text.trim() == text => [value]
		_ => []
	}

names_type : List(Str), JsonValue -> Bool
names_type = |types, value| {
	name = value.type_name()
	types.contains(name) or (name == "number" and types.contains("integer"))
}

# Shapes

shape_of : List(JsonValue), Schema.Doc -> Shape
shape_of = |schemas, doc| { schemas, branches: schemas.join_map(|schema| Schema.branches(schema, doc.root)) }

# The types the branches of a shape name. One with "properties" or "items"
# but no "type" is an object or an array.
types_of : Shape -> List(Str)
types_of = |shape|
	shape.branches.join_map(
		|branch| {
			named =
				match branch.get_field("type") {
					Ok(type) =>
						match (type.get_str(), type.get_array()) {
							(Ok(name), _) => [name]
							(_, Ok(names)) => names.keep_oks(JsonValue.get_str)
							_ => []
						}

					Err(_) => []
				}
			implied = List.join([
				if branch.get_field("properties").is_ok() or branch.get_field("additionalProperties").is_ok() ["object"] else [],
				if branch.get_field("items").is_ok() ["array"] else [],
			])
			named.concat(implied)
		},
	)

# The shape of the field `key` of an object: its schema under "properties",
# else the "additionalProperties" schema, in each branch that has one.
field_shape : Shape, Str, Schema.Doc -> Shape
field_shape = |shape, key, doc| {
	field_schema = |branch|
		match branch.get_field("properties").map_ok(|properties| properties.get_field(key)) {
			Ok(Ok(schema)) => Ok(schema)
			_ =>
				match branch.get_field("additionalProperties") {
					Ok(additional) if additional.get_object().is_ok() => Ok(additional)
					_ => Err(NoSchema)
				}
		}
	shape_of(shape.branches.keep_oks(field_schema), doc)
}

# The shape of the items of an array, from each branch with "items".
item_shape : Shape, Schema.Doc -> Shape
item_shape = |shape, doc| shape_of(shape.branches.keep_oks(|branch| branch.get_field("items")), doc)

# Tests

# A form read against `schema`, as JSON text, or the problem with it.
decoded : Str, Str -> Str
decoded = |form, schema| decoded_in(form, schema, "{}")

# The same, with `$ref`s that point into `root_doc`.
decoded_in : Str, Str, Str -> Str
decoded_in = |form, schema, root_doc| {
	json = |text| JsonValue.parse(text.to_utf8()) ?? crash "A test's JSON does not parse: ${text}"
	match Form.decode(form.to_utf8(), [json(schema)], Schema.prepare(json(root_doc)), 32) {
		Ok(value) => value.to_str()
		Err(InvalidForm(message)) => "InvalidForm: ${message}"
	}
}

# With no schema, every field is text.
expect decoded("a=1&b=true&c=", "{}") == "{\"a\":\"1\",\"b\":\"true\",\"c\":\"\"}"

expect decoded("a=x%20y+z%2B%26&%C3%B6=%E2%9C%93", "{}") == "{\"a\":\"x y z+&\",\"ö\":\"✓\"}"

expect decoded("flag&&a=1&", "{}") == "{\"flag\":\"\",\"a\":\"1\"}"

expect decoded("", "{}") == "{}"

# Types from the schema.
widget_schema : Str
widget_schema =
	\\{
	\\  "type": "object",
	\\  "properties": {
	\\    "name": { "type": "string" },
	\\    "count": { "type": "integer" },
	\\    "price": { "type": "number" },
	\\    "active": { "type": "boolean" },
	\\    "note": { "type": "integer", "nullable": true },
	\\    "tags": { "type": "array", "items": { "type": "string" } },
	\\    "sizes": { "type": "array", "items": { "type": "integer" } }
	\\  }
	\\}

expect decoded("name=42&count=42&price=4.5&active=false&note=", widget_schema) == "{\"name\":\"42\",\"count\":42,\"price\":4.5,\"active\":false,\"note\":null}"

# Text that is not what the schema wants stays text, for the error to name.
expect decoded("count=many&active=yes&price=%201", widget_schema) == "{\"count\":\"many\",\"active\":\"yes\",\"price\":\" 1\"}"

expect decoded("count=1.5", widget_schema) == "{\"count\":1.5}"

# A name given more than once is a list, and so is one given once where the
# schema wants a list.
expect decoded("tags=a&tags=b&sizes=1&sizes=2", widget_schema) == "{\"tags\":[\"a\",\"b\"],\"sizes\":[1,2]}"

expect decoded("tags=a&sizes=1", widget_schema) == "{\"tags\":[\"a\"],\"sizes\":[1]}"

expect decoded("tags[]=a&tags[]=b", widget_schema) == "{\"tags\":[\"a\",\"b\"]}"

expect decoded("sizes=x", widget_schema) == "{\"sizes\":[\"x\"]}"

# Given more than once where the schema wants one value, it is a list still.
expect decoded("name=a&name=b", widget_schema) == "{\"name\":[\"a\",\"b\"]}"

# Brackets nest, and numbered fields are an array where the schema wants one.
stripe_like : Str
stripe_like =
	\\{
	\\  "type": "object",
	\\  "properties": {
	\\    "items": {
	\\      "type": "array",
	\\      "items": { "type": "object", "properties": { "price": { "type": "string" }, "quantity": { "type": "integer" } } }
	\\    },
	\\    "metadata": {
	\\      "anyOf": [{ "type": "object", "additionalProperties": { "type": "string" } }, { "type": "string", "enum": [""] }]
	\\    },
	\\    "cancel_at": { "anyOf": [{ "type": "integer" }, { "type": "string", "enum": ["max_period_end"] }] },
	\\    "fee": { "anyOf": [{ "type": "number" }, { "type": "string", "enum": [""] }] }
	\\  }
	\\}

expect decoded("items[0][price]=p1&items[1][price]=p2&items[0][quantity]=2", stripe_like) == "{\"items\":[{\"price\":\"p1\",\"quantity\":2},{\"price\":\"p2\"}]}"

expect decoded("items%5B0%5D%5Bprice%5D=p1", stripe_like) == "{\"items\":[{\"price\":\"p1\"}]}"

# Missing numbers close up, in the order of the numbers.
expect decoded("items[2][price]=b&items[0][price]=a", stripe_like) == "{\"items\":[{\"price\":\"a\"},{\"price\":\"b\"}]}"

# Fields that are not all numbers make an object, which the schema can then
# reject.
expect decoded("items[0][price]=a&items[x][price]=b", stripe_like) == "{\"items\":{\"0\":{\"price\":\"a\"},\"x\":{\"price\":\"b\"}}}"

expect decoded("items[01][price]=a", stripe_like) == "{\"items\":{\"01\":{\"price\":\"a\"}}}"

expect decoded("metadata[order]=7&metadata[note]=", stripe_like) == "{\"metadata\":{\"order\":\"7\",\"note\":\"\"}}"

expect decoded("metadata=", stripe_like) == "{\"metadata\":\"\"}"

# Each reading is tried against every branch of an anyOf.
expect decoded("cancel_at=1700000000&fee=12.5", stripe_like) == "{\"cancel_at\":1700000000,\"fee\":12.5}"

expect decoded("cancel_at=max_period_end&fee=", stripe_like) == "{\"cancel_at\":\"max_period_end\",\"fee\":\"\"}"

expect decoded("cancel_at=soon", stripe_like) == "{\"cancel_at\":\"soon\"}"

# Through $ref and allOf.
expect {
	root_doc =
		\\{
		\\  "components": {
		\\    "schemas": {
		\\      "Base": { "type": "object", "properties": { "id": { "type": "integer" } } },
		\\      "Item": { "allOf": [{ "$ref": "#/components/schemas/Base" }, { "properties": { "on": { "type": "boolean" } } }] }
		\\    }
		\\  }
		\\}
	decoded_in("id=7&on=true&other=7", "{ \"$ref\": \"#/components/schemas/Item\" }", root_doc) == "{\"id\":7,\"on\":true,\"other\":\"7\"}"
}

# A schema that includes itself is followed once.
expect {
	root_doc =
		\\{ "components": { "schemas": { "Node": { "type": "object", "properties": { "n": { "type": "integer" }, "child": { "$ref": "#/components/schemas/Node" } } } } } }
	decoded_in("n=1&child[n]=2&child[child][n]=3", "{ \"$ref\": \"#/components/schemas/Node\" }", root_doc) == "{\"n\":1,\"child\":{\"n\":2,\"child\":{\"n\":3}}}"
}

# Names that are not made of a name and brackets are names as they are.
expect decoded("a[b=1&a]b=2&[a]=3&a[b]c=4&a[b]]=5", "{}") == "{\"a[b\":\"1\",\"a]b\":\"2\",\"[a]\":\"3\",\"a[b]c\":\"4\",\"a[b]]\":\"5\"}"

# Problems

expect decoded("a=1&a[b]=2", "{}") == "InvalidForm: Both a value and fields at /a"

expect decoded("a=%zz", "{}") == "InvalidForm: Invalid percent-encoding in %zz"

expect decoded("a=%FF", "{}") == "InvalidForm: Invalid percent-encoding in %FF"

expect
	match Form.decode([0x61, 0x3D, 0xFF], [], Schema.prepare(JsonValue.(Object([]))), 32) {
		Err(InvalidForm(message)) => message == "Invalid UTF-8 at byte 2"
		Ok(_) => Bool.False
	}

# Depth: the form itself is one level, and each bracket and list one more.
expect decoded("a[b][c]=1", "{}") == "{\"a\":{\"b\":{\"c\":\"1\"}}}"

expect {
	at_most = |form, max_depth|
		match Form.decode(form.to_utf8(), [], Schema.prepare(JsonValue.(Object([]))), max_depth) {
			Ok(_) => "ok"
			Err(InvalidForm(message)) => message
		}
	outcomes = [
		at_most("a[b][c]=1", 3),
		at_most("a[b][c]=1", 2),
		# A list is a level too.
		at_most("a[b]=1&a[b]=2", 2),
		# Too deep before the tree is built.
		at_most("a${Str.repeat("[a]", 100_000)}=1", 32),
	]
	outcomes == ["ok", "Nested deeper than 2 levels", "Nested deeper than 2 levels", "Nested deeper than 32 levels"]
}
