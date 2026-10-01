## A JSON document as a tree, for the specs, schemas and bodies whose shape is
## only known at runtime. The builtin `Json.parse` builds it through
## `parser_for`, so this module only describes the tree and how to read it.
##
## Numbers are `F64`. Object fields keep their source order and are not
## deduplicated, and `get_field` returns the first field with the name.
JsonValue := [
	Null,
	Bool(Bool),
	Number(F64),
	String(Str),
	Array(List(JsonValue)),
	Object(List({ key : Str, value : JsonValue })),
].{

	## Parse JSON bytes. The message in `InvalidJson` says what was expected and
	## where, as a JSON Pointer, such as
	## `Expected ',' and a field, or '}', after this field at /paths/~1widgets/post/summary`.
	##
	## A problem inside a single value, such as a bad escape in a string or a
	## malformed number, is an `Invalid value`, because the builtin parser
	## does not say more than that.
	##
	## Arrays and objects may nest 2000 deep. `parse_with_max_depth` sets a
	## lower limit.
	parse : List(U8) -> Try(JsonValue, [InvalidJson(Str)])
	parse = |bytes| {
		text = Str.from_utf8(bytes) ? |BadUtf8({ index, problem: _ })| InvalidJson("Invalid UTF-8 at byte ${index.to_str()}")
		Json.parse(text).map_err(
			|problem|
				match problem {
					Malformed({ expected, at }) => InvalidJson("${expected} at ${JsonValue.pointer(at)}")
					TooDeep => InvalidJson("Nested deeper than ${max_nesting_depth.to_str()} levels")
					# The builtin parser fails on its own only for text after a whole document.
					InvalidJson(_) => InvalidJson("Unexpected text after the document")
				},
		)
	}

	## Parse JSON bytes whose arrays and objects nest at most `max_depth`
	## deep, such as `[[1]]` at 2. Deeper text fails before parsing starts,
	## because parsing it, and validating it against a schema that refers to
	## itself, takes stack in proportion to the depth.
	parse_with_max_depth : List(U8), U64 -> Try(JsonValue, [InvalidJson(Str)])
	parse_with_max_depth = |bytes, max_depth|
		if nests_deeper_than(bytes, max_depth) {
			Err(InvalidJson("Nested deeper than ${max_depth.to_str()} levels"))
		} else {
			JsonValue.parse(bytes)
		}

	## A JSON Pointer (RFC 6901) for a path through a document, such as
	## `/paths/~1widgets/post`. The document itself is `/`.
	pointer : List(Str) -> Str
	pointer = |path| "/${Str.join_with(path.map(escape_segment), "/")}"

	parser_for = |encoding| |state| parse_value(encoding, state, 0)

	## Equality as JSON Schema defines it, which is what `==` and `!=` call.
	## Objects are equal when they have the same fields, in any order. Floats
	## have no `==` of their own, which is why this is written out by hand.
	is_eq : JsonValue, JsonValue -> Bool
	is_eq = |JsonValue.(a), JsonValue.(b)|
		match (a, b) {
			(Null, Null) => Bool.True
			(Bool(x), Bool(y)) => x == y
			(Number(x), Number(y)) => F64.is_float_eq(x, y)
			(String(x), String(y)) => x == y
			(Array(xs), Array(ys)) => xs == ys
			(Object(xs), Object(ys)) => xs.len() == ys.len() and xs.all(|field| ys.contains(field)) and ys.all(|field| xs.contains(field))
			_ => Bool.False
		}

	## The first field of an object with this name.
	get_field : JsonValue, Str -> Try(JsonValue, [FieldNotFound(Str)])
	get_field = |JsonValue.(inner), name|
		match inner {
			Object(fields) =>
				fields
					.find_first(|field| field.key == name)
					.map_ok(|field| field.value)
					.map_err(|_| FieldNotFound(name))

			_ => Err(FieldNotFound(name))
		}

	get_str : JsonValue -> Try(Str, [NotAString])
	get_str = |JsonValue.(inner)|
		match inner {
			String(str) => Ok(str)
			_ => Err(NotAString)
		}

	get_bool : JsonValue -> Try(Bool, [NotABool])
	get_bool = |JsonValue.(inner)|
		match inner {
			Bool(bool) => Ok(bool)
			_ => Err(NotABool)
		}

	get_number : JsonValue -> Try(F64, [NotANumber])
	get_number = |JsonValue.(inner)|
		match inner {
			Number(number) => Ok(number)
			_ => Err(NotANumber)
		}

	get_array : JsonValue -> Try(List(JsonValue), [NotAnArray])
	get_array = |JsonValue.(inner)|
		match inner {
			Array(items) => Ok(items)
			_ => Err(NotAnArray)
		}

	get_object : JsonValue -> Try(List({ key : Str, value : JsonValue }), [NotAnObject])
	get_object = |JsonValue.(inner)|
		match inner {
			Object(fields) => Ok(fields)
			_ => Err(NotAnObject)
		}

	is_null : JsonValue -> Bool
	is_null = |JsonValue.(inner)|
		match inner {
			Null => Bool.True
			_ => Bool.False
		}

	## How deep the arrays and objects in a value nest, as
	## `parse_with_max_depth` counts, such as 2 for `{"a": [1]}`.
	depth : JsonValue -> U64
	depth = |JsonValue.(inner)|
		match inner {
			Array(items) => 1 + items.map(JsonValue.depth).max().ok_or(0)
			Object(fields) => 1 + fields.map(|field| field.value.depth()).max().ok_or(0)
			_ => 0
		}

	## The name JSON Schema's `type` keyword uses for the value.
	type_name : JsonValue -> Str
	type_name = |JsonValue.(inner)|
		match inner {
			Null => "null"
			Bool(_) => "boolean"
			Number(_) => "number"
			String(_) => "string"
			Array(_) => "array"
			Object(_) => "object"
		}

	## The value as compact JSON text, such as `{"a":[1,"b"]}`.
	to_str : JsonValue -> Str
	to_str = |JsonValue.(inner)|
		match inner {
			Null => "null"
			Bool(bool) => if bool "true" else "false"
			Number(number) => number.to_str()
			String(str) => Json.to_str(str)
			Array(items) => "[${Str.join_with(items.map(JsonValue.to_str), ",")}]"
			Object(fields) => "{${Str.join_with(fields.map(|{ key, value }| "${Json.to_str(key)}:${value.to_str()}"), ",")}}"
		}
}

# Parsing

# The builtin parser has no nesting limit, and a document nested deep enough
# overflows the stack. This one fails to parse instead.
max_nesting_depth : U64
max_nesting_depth = 2000

# The parse state is opaque, so there is no peeking at the next byte to see
# what kind of value comes. Each kind is tried in turn instead. The state is an
# immutable value, so backing out of a failed attempt costs nothing.
#
# Strings, arrays and objects go first, because they give up after one byte.
# The null, boolean and number parsers read to the end of the token before
# they give up, and `[`, `{` and the inside of a string do not end a token.
# Tried first, they would read most of a deep array or a long string at every
# level, which makes parsing quadratic.
#
# A failure is `Malformed`, with what was expected and the path to where. Each
# level of nesting adds its field name or index to the front of that path as
# the failure returns through it.
#
# `depth` is how many arrays and objects hold the value. One that is itself an
# array or object takes it a level deeper, which past the limit is `TooDeep`,
# without a pointer, which would be thousands of segments long.
parse_value : _, _, U64 -> Try({ value : JsonValue, rest : _ }, [Malformed({ expected : Str, at : List(Str) }), TooDeep])
parse_value = |encoding, state, depth|
	match Json.parse_str(encoding, state) {
		Ok({ value, rest }) => Ok({ value: JsonValue.(String(value)), rest })
		Err(_) =>
			match Json.parse_list_start(encoding, state) {
				Ok(start) => if depth >= max_nesting_depth Err(TooDeep) else parse_items(encoding, after_start(start), [], depth + 1)
				Err(_) =>
				# Objects are read as dicts are, since their field names
				# are not known upfront, but into a list that keeps the
				# order and any repeated names.
					match encoding.parse_dict_start(state) {
						Ok(start) => if depth >= max_nesting_depth Err(TooDeep) else parse_fields(encoding, after_start(start), [], depth + 1)
						Err(_) => parse_scalar(encoding, state)
					}
			}
	}

after_start = |start|
	match start {
		Uncounted(rest) => rest
		Counted({ rest, len: _ }) => rest
	}

parse_scalar = |encoding, state|
	match Json.parse_null(encoding, state) {
		Ok(rest) => Ok({ value: JsonValue.(Null), rest })
		Err(_) =>
			match Json.parse_bool(encoding, state) {
				Ok({ value, rest }) => Ok({ value: JsonValue.(Bool(value)), rest })
				Err(_) =>
					match Json.parse_f64(encoding, state) {
						Ok({ value, rest }) => Ok({ value: JsonValue.(Number(value)), rest })
						Err(_) => Err(Malformed({ expected: "Invalid value", at: [] }))
					}
			}
	}

# The failures below are matched on, rather than mapped with `? |_| ...`, since
# a handler like that slows down the whole parse by about 12% even when
# nothing fails.
parse_items = |encoding, state, items, depth| {
	next_item =
		match Json.parse_list_next(encoding, state) {
			Ok(next) => next
			Err(_) => return Err(Malformed({ expected: "Expected a value", at: [items.len().to_str()] }))
		}

	match next_item {
		Done(rest) => Ok({ value: JsonValue.(Array(items)), rest })
		Item(item_state) => {
			{ value, rest } =
				match parse_value(encoding, item_state, depth) {
					Ok(parsed) => parsed
					Err(problem) => return Err(inside(problem, items.len().to_str()))
				}

			match Json.parse_list_after_item(encoding, rest) {
				Ok(Continue(next)) => parse_items(encoding, next, items.append(value), depth)
				Ok(Done(after)) => Ok({ value: JsonValue.(Array(items.append(value))), rest: after })
				Err(_) => Err(Malformed({ expected: "Expected ',' and a value, or ']', after this item", at: [items.len().to_str()] }))
			}
		}
	}
}

parse_fields = |encoding, state, fields, depth| {
	next_field =
		match encoding.parse_dict_next(state) {
			Ok(next) => next
			Err(_) => return Err(Malformed({ expected: "Expected a field or '}'", at: [] }))
		}

	match next_field {
		Done(rest) => Ok({ value: JsonValue.(Object(fields)), rest })
		Entry(entry_state) => {
			{ value: key, rest: after_key } =
				match Json.parse_str(encoding, entry_state) {
					Ok(parsed) => parsed
					Err(_) => return Err(Malformed({ expected: "Expected a field name in double quotes", at: [] }))
				}

			value_state =
				match encoding.parse_dict_after_key(after_key) {
					Ok(next) => next
					Err(_) => return Err(Malformed({ expected: "Expected ':' after this field name", at: [key] }))
				}

			{ value, rest } =
				match parse_value(encoding, value_state, depth) {
					Ok(parsed) => parsed
					Err(problem) => return Err(inside(problem, key))
				}

			match encoding.parse_dict_after_entry(rest) {
				Ok(Continue(next)) => parse_fields(encoding, next, fields.append({ key, value }), depth)
				Ok(Done(after)) => Ok({ value: JsonValue.(Object(fields.append({ key, value }))), rest: after })
				Err(_) => Err(Malformed({ expected: "Expected ',' and a field, or '}', after this field", at: [key] }))
			}
		}
	}
}

# A failure inside the field or item `segment`, seen from the value around it.
inside = |problem, segment|
	match problem {
		Malformed({ expected, at }) => Malformed({ expected, at: at.prepend(segment) })
		TooDeep => TooDeep
	}

# Whether the arrays and objects in JSON text nest deeper than `max_depth`,
# from a count of the brackets and braces outside of strings. Text that is not
# JSON is for the parser to report, so a stray closing bracket is let be.
nests_deeper_than : List(U8), U64 -> Bool
nests_deeper_than = |bytes, max_depth| {
	var $depth = 0
	var $in_string = Bool.False
	var $escaped = Bool.False
	for byte in bytes {
		if $escaped {
			$escaped = Bool.False
		} else if $in_string {
			if byte == '\\' {
				$escaped = Bool.True
			} else if byte == '"' {
				$in_string = Bool.False
			}
		} else if byte == '"' {
			$in_string = Bool.True
		} else if byte == '[' or byte == '{' {
			$depth = $depth + 1
			if $depth > max_depth {
				return Bool.True
			}
		} else if byte == ']' or byte == '}' {
			$depth = $depth.minus_saturated(1)
		}
	}
	Bool.False
}

# RFC 6901: "~" is written "~0" and "/" is written "~1", in that order, so
# that a "~1" in the segment comes out as "~01".
escape_segment : Str -> Str
escape_segment = |segment|
	segment.replace_each("~", "~0").replace_each("/", "~1")

# Tests

parse_str : Str -> Try(JsonValue, [InvalidJson(Str)])
parse_str = |text| JsonValue.parse(text.to_utf8())

expect parse_str("null") == Ok(JsonValue.(Null))

expect parse_str(" true ") == Ok(JsonValue.(Bool(Bool.True)))

expect parse_str("-1.5e2") == Ok(JsonValue.(Number(-150.0)))

expect parse_str("\"a\\nb\\u00e9\\ud83e\\udd85\"") == Ok(JsonValue.(String("a\nbé🦅")))

expect parse_str("[1, [], {}]") == Ok(JsonValue.(Array([JsonValue.(Number(1.0)), JsonValue.(Array([])), JsonValue.(Object([]))])))

# Fields keep their order, and a repeated name is kept too.
expect {
	parsed = parse_str("{\"b\": {\"c\": [true, null]}, \"a\": 1, \"b\": 2}")
	expected = JsonValue.(
		Object([
			{ key: "b", value: JsonValue.(Object([{ key: "c", value: JsonValue.(Array([JsonValue.(Bool(Bool.True)), JsonValue.(Null)])) }])) },
			{ key: "a", value: JsonValue.(Number(1.0)) },
			{ key: "b", value: JsonValue.(Number(2.0)) },
		]),
	)
	# Equality does not see the order, but the text written back does.
	parsed == Ok(expected) and parsed.map_ok(JsonValue.to_str) == Ok("{\"b\":{\"c\":[true,null]},\"a\":1,\"b\":2}")
}

expect parse_str("[1,]").is_err()

expect parse_str("{\"a\": 1,}").is_err()

expect parse_str("01").is_err()

expect parse_str("1e400").is_err()

expect parse_str("{} trailing").is_err()

expect parse_str("").is_err()

expect JsonValue.parse(['"', 0xFF, '"']).is_err()

# Depth

nested : U64 -> Str
nested = |depth| Str.repeat("[", depth).concat(Str.repeat("]", depth))

expect parse_str(nested(2000)).is_ok()

expect parse_str(nested(2001)) == Err(InvalidJson("Nested deeper than 2000 levels"))

# A document far too deep fails rather than overflowing the stack.
expect parse_str(Str.repeat("[", 100_000)).is_err()

expect ["1", "[]", "{\"a\": [1]}", "[[], [[{}]]]"].map(|text| parse_str(text).map_ok(JsonValue.depth)) == [Ok(0), Ok(1), Ok(2), Ok(4)]

expect JsonValue.parse_with_max_depth(nested(3).to_utf8(), 3).is_ok()

expect JsonValue.parse_with_max_depth(nested(4).to_utf8(), 3) == Err(InvalidJson("Nested deeper than 3 levels"))

expect JsonValue.parse_with_max_depth("{\"a\": {\"b\": [{}]}}".to_utf8(), 3) == Err(InvalidJson("Nested deeper than 3 levels"))

# Brackets in strings do not nest, escaped quotes and all.
expect JsonValue.parse_with_max_depth("[\"[[{{\", \"\\\"[[\", \"\\\\\"]".to_utf8(), 1).is_ok()

# Scalars nest no deeper than what holds them.
expect JsonValue.parse_with_max_depth("[1, \"a\", null]".to_utf8(), 1).is_ok()

expect JsonValue.parse_with_max_depth("1".to_utf8(), 0).is_ok()

# Text that is too deep fails before it is parsed, so it does not need to be
# JSON.
expect JsonValue.parse_with_max_depth(Str.repeat("[", 100_000).to_utf8(), 32) == Err(InvalidJson("Nested deeper than 32 levels"))

# Text that is not JSON gets the parser's message.
expect JsonValue.parse_with_max_depth("]][".to_utf8(), 1) == Err(InvalidJson("Invalid value at /"))

# A long string deep down is read once, not once per level above it.
expect {
	long = Str.repeat("a", 1_000_000)
	parsed = parse_str("${Str.repeat("[", 2000)}\"${long}\"${Str.repeat("]", 2000)}")
	parsed.is_ok()
}

# Error messages say what was expected, and where as a JSON Pointer.

expect parse_str("{\"items\": [1, 2, tru]}") == Err(InvalidJson("Invalid value at /items/2"))

expect parse_str("{\"a\" 1}") == Err(InvalidJson("Expected ':' after this field name at /a"))

expect parse_str("{\"a\": [1 2]}") == Err(InvalidJson("Expected ',' and a value, or ']', after this item at /a/0"))

# A trailing comma.
expect parse_str("{\"a\": [1, 2,]}") == Err(InvalidJson("Expected ',' and a value, or ']', after this item at /a/1"))

expect parse_str("{\"paths\": {\"/w\": {\"summary\": \"x\" \"get\": {}}}}") == Err(InvalidJson("Expected ',' and a field, or '}', after this field at /paths/~1w/summary"))

expect parse_str("{a: 1}") == Err(InvalidJson("Expected a field name in double quotes at /"))

# A bad escape is an invalid value like any other problem inside one.
expect parse_str("{\"s\": \"\\q\"}") == Err(InvalidJson("Invalid value at /s"))

expect parse_str("") == Err(InvalidJson("Invalid value at /"))

expect parse_str("[1] x") == Err(InvalidJson("Unexpected text after the document"))

expect JsonValue.parse(['[', '"', 0xFF, '"', ']']) == Err(InvalidJson("Invalid UTF-8 at byte 2"))

expect JsonValue.pointer([]) == "/"

expect JsonValue.pointer(["a/b", "~c", "0"]) == "/a~1b/~0c/0"

# Accessors

expect parse_str("{\"a\": 1, \"a\": 2}").map_ok(|object| JsonValue.get_field(object, "a")) == Ok(Ok(JsonValue.(Number(1.0))))

expect JsonValue.get_field(JsonValue.(Object([])), "a") == Err(FieldNotFound("a"))

expect JsonValue.get_field(JsonValue.(Array([])), "a") == Err(FieldNotFound("a"))

expect JsonValue.get_str(JsonValue.(String("x"))) == Ok("x")

expect JsonValue.get_str(JsonValue.(Number(1.0))) == Err(NotAString)

expect JsonValue.get_bool(JsonValue.(String("true"))) == Err(NotABool)

expect JsonValue.get_number(JsonValue.(String("1"))) == Err(NotANumber)

expect JsonValue.get_array(JsonValue.(Object([]))) == Err(NotAnArray)

expect JsonValue.get_object(JsonValue.(Array([]))) == Err(NotAnObject)

expect JsonValue.is_null(JsonValue.(Null))

expect !JsonValue.is_null(JsonValue.(String("")))

expect
	[JsonValue.(Null), JsonValue.(Bool(Bool.False)), JsonValue.(Number(0.0)), JsonValue.(String("")), JsonValue.(Array([])), JsonValue.(Object([]))].map(JsonValue.type_name)
		== ["null", "boolean", "number", "string", "array", "object"]

# Writing

expect parse_str("{\"a\": [1, -2.5, \"b\\\"c\", true, null], \"d\": {}}").map_ok(JsonValue.to_str) == Ok("{\"a\":[1,-2.5,\"b\\\"c\",true,null],\"d\":{}}")

# Equality

expect JsonValue.(Number(0.0)) == JsonValue.(Number(-0.0))

expect JsonValue.(Number(1.0)) != JsonValue.(String("1"))

# Objects are equal with their fields in any order, at any depth.
expect parse_str("{\"x\": 1, \"y\": 2}") == parse_str("{\"y\": 2, \"x\": 1}")

expect parse_str("[{\"a\": {\"x\": 1, \"y\": [2]}}]") == parse_str("[{\"a\": {\"y\": [2], \"x\": 1}}]")

expect parse_str("{\"x\": 1, \"y\": 2}") != parse_str("{\"x\": 1, \"y\": 3}")

expect parse_str("{\"x\": 1}") != parse_str("{\"x\": 1, \"y\": 2}")

expect parse_str("{\"x\": 1, \"y\": 2}") != parse_str("{\"x\": 1}")

# Arrays are equal only in the same order.
expect parse_str("[1, 2]") != parse_str("[2, 1]")
