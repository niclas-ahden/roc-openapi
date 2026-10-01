## ECMAScript regular expressions, as much of them as JSON Schema's `pattern`
## keyword needs: literals, `.`, classes such as `[a-z]` and `[^/]`, the
## escapes `\d`, `\w`, `\s` and `\b` and their negations, groups, `|`, the
## anchors `^` and `$`, and the quantifiers `*`, `+`, `?`, `{n}`, `{n,}` and
## `{n,m}`. Lookaround, backreferences and Unicode property escapes are
## `Unsupported`.

CharRange : { from : U32, to : U32 }

# A range of characters, or with `Outside`, every character outside some
# ranges, which is what \D, \W and \S are inside a class.
ClassItem : [Within(CharRange), Outside(List(CharRange))]

# One step of a pattern. The whole pattern is a list of them, matched in turn.
Node := [
	Char(U32),
	# `.`, any character but a line terminator.
	AnyChar,
	# A character one of the items covers, or with `negated`, one none of
	# them does.
	Class({ negated : Bool, items : List(ClassItem) }),
	InputStart,
	InputEnd,
	WordBoundary,
	NotWordBoundary,
	Group(List(List(Node))),
	Repeat({ body : List(Node), min : U64, max : [AtMost(U64), Unbounded] }),
	# Matches nothing, and only once the match has moved past this position.
	# It ends a repetition of something that can match the empty string.
	MovedPast(U64),
]

Pattern :: List(Node).{

	## Parse the source of a regular expression, such as `^[a-z]+$`.
	parse : Str -> Try(Pattern, [Unsupported(Str)])
	parse = |source| {
		chars = code_points(source)
		{ alternatives, pos } = parse_alternatives(chars, 0, [])?
		if pos < chars.len() {
			# Only a ")" without a "(" ends the alternatives early.
			Err(Unsupported("Unbalanced ) in ${source}"))
		} else {
			Ok(Pattern.([Node.(Group(alternatives))]))
		}
	}

	## Whether the pattern matches anywhere in the string, which is how JSON
	## Schema reads `pattern`. Only `^` and `$` tie it to the ends.
	##
	## Matching backtracks, so a pattern such as `(a+)+b` takes exponential
	## time on some strings. A search that takes more than `step_limit` steps
	## gives up with `TooComplex`.
	matches : Pattern, Str -> Try(Bool, [TooComplex])
	matches = |Pattern.(nodes), str| {
		input = code_points(str)
		starts =
			if is_anchored(nodes) {
				[0]
			} else {
				List.repeat(0, input.len() + 1).map_with_index(|_, index| index)
			}

		outcome = starts.fold_until(
			NotFound(step_limit),
			|searched, start|
				match searched {
					NotFound(steps) =>
						match search(input, { todo: nodes, pos: start }, steps) {
							NotFound(left) => Continue(NotFound(left))
							other => Break(other)
						}

					other => Break(other)
				},
		)

		match outcome {
			Found => Ok(Bool.True)
			NotFound(_) => Ok(Bool.False)
			OutOfSteps => Err(TooComplex)
		}
	}
}

# Parsing

# Alternatives separated by "|", up to the end or to the ")" that closes their
# group.
parse_alternatives = |chars, pos, alternatives| {
	{ sequence, pos: after } = parse_sequence(chars, pos, [])?
	all = alternatives.append(sequence)
	match chars.get(after) {
		Ok('|') => parse_alternatives(chars, after + 1, all)
		_ => Ok({ alternatives: all, pos: after })
	}
}

parse_sequence = |chars, pos, sequence|
	match chars.get(pos) {
		Ok('|') => Ok({ sequence, pos })
		Ok(')') => Ok({ sequence, pos })
		Ok(char) => {
			{ node, pos: after_atom } = parse_atom(chars, char, pos + 1)?
			{ node: quantified, pos: after } = parse_quantifier(chars, after_atom, node)?
			parse_sequence(chars, after, sequence.append(quantified))
		}

		Err(_) => Ok({ sequence, pos })
	}

# The atom that starts with `char`, which sits just before `pos`.
parse_atom = |chars, char, pos|
	match char {
		'(' => parse_group(chars, pos)
		'[' => parse_class(chars, pos)
		'\\' => parse_escape(chars, pos)
		'.' => Ok({ node: Node.(AnyChar), pos })
		'^' => Ok({ node: Node.(InputStart), pos })
		'$' => Ok({ node: Node.(InputEnd), pos })
		'*' => Err(Unsupported("Nothing to repeat"))
		'+' => Err(Unsupported("Nothing to repeat"))
		'?' => Err(Unsupported("Nothing to repeat"))
		_ => Ok({ node: Node.(Char(char)), pos })
	}

# The inside of a group, after its "(". Capturing, non-capturing "(?:" and
# named "(?<name>" groups all match the same. The other "(?" groups look
# around, which is not supported.
parse_group = |chars, pos| {
	body = group_body_start(chars, pos)?
	{ alternatives, pos: after } = parse_alternatives(chars, body, [])?
	match chars.get(after) {
		Ok(')') => Ok({ node: Node.(Group(alternatives)), pos: after + 1 })
		_ => Err(Unsupported("Unclosed group"))
	}
}

group_body_start = |chars, pos|
	match (chars.get(pos), chars.get(pos + 1), chars.get(pos + 2)) {
		(Ok('?'), Ok(':'), _) => Ok(pos + 2)
		(Ok('?'), Ok('<'), Ok(after)) if after != '=' and after != '!' =>
			match chars.drop_first(pos).find_first_index(|char| char == '>') {
				Ok(offset) => Ok(pos + offset + 1)
				Err(_) => Err(Unsupported("Unclosed group name"))
			}

		(Ok('?'), _, _) => Err(Unsupported("Lookaround"))
		_ => Ok(pos)
	}

# The inside of a class, after its "[".
parse_class = |chars, pos| {
	negated = chars.get(pos) == Ok('^')
	{ items, pos: after } = parse_class_items(chars, if negated pos + 1 else pos, [])?
	Ok({ node: Node.(Class({ negated, items })), pos: after })
}

parse_class_items = |chars, pos, items|
	match chars.get(pos) {
		Ok(']') => Ok({ items, pos: pos + 1 })
		Ok(_) => {
			{ value: first, pos: after_first } = parse_class_atom(chars, pos)?
			match (first, chars.get(after_first), chars.get(after_first + 1)) {
				# A "-" between two characters makes a range, but a "-" at the end
				# of the class is itself.
				(Single(from), Ok('-'), Ok(next)) if next != ']' => {
					{ value: last, pos: after_last } = parse_class_atom(chars, after_first + 1)?
					match last {
						Single(to) if from <= to => parse_class_items(chars, after_last, items.append(Within({ from, to })))
						Single(_) => Err(Unsupported("Class range out of order"))
						# Next to a set such as \d, the "-" is itself, as in [a-\d].
						Set(set) => parse_class_items(chars, after_last, items.concat([Within({ from, to: from }), Within({ from: '-', to: '-' })]).concat(set))
					}
				}

				(Single(char), _, _) => parse_class_items(chars, after_first, items.append(Within({ from: char, to: char })))
				(Set(set), _, _) => parse_class_items(chars, after_first, items.concat(set))
			}
		}

		Err(_) => Err(Unsupported("Unclosed class"))
	}

parse_class_atom = |chars, pos|
	match chars.get(pos) {
		Ok('\\') =>
			match chars.get(pos + 1) {
				# Inside a class, \b is a backspace.
				Ok('b') => Ok({ value: Single(8), pos: pos + 2 })
				Ok(letter) =>
					match class_escape(letter) {
						Ok(set) => Ok({ value: Set(set), pos: pos + 2 })
						Err(_) => character_escape(chars, letter, pos + 2).map_ok(|{ char, pos: after }| { value: Single(char), pos: after })
					}

				Err(_) => Err(Unsupported("Trailing backslash"))
			}

		Ok(char) => Ok({ value: Single(char), pos: pos + 1 })
		Err(_) => Err(Unsupported("Unclosed class"))
	}

# The escape after a "\" outside a class.
parse_escape = |chars, pos|
	match chars.get(pos) {
		Ok('b') => Ok({ node: Node.(WordBoundary), pos: pos + 1 })
		Ok('B') => Ok({ node: Node.(NotWordBoundary), pos: pos + 1 })
		Ok(letter) =>
			match class_escape(letter) {
				Ok(items) => Ok({ node: Node.(Class({ negated: Bool.False, items })), pos: pos + 1 })
				Err(_) => character_escape(chars, letter, pos + 1).map_ok(|{ char, pos: after }| { node: Node.(Char(char)), pos: after })
			}

		Err(_) => Err(Unsupported("Trailing backslash"))
	}

# \d, \w and \s, and their negations.
class_escape : U32 -> Try(List(ClassItem), [NotASet])
class_escape = |letter|
	match letter {
		'd' => Ok(digits.map(|range| Within(range)))
		'D' => Ok([Outside(digits)])
		'w' => Ok(word_chars.map(|range| Within(range)))
		'W' => Ok([Outside(word_chars)])
		's' => Ok(spaces.map(|range| Within(range)))
		'S' => Ok([Outside(spaces)])
		_ => Err(NotASet)
	}

# The character an escape such as \n, \x41 or é stands for, given the
# letter after the "\" and the position after that letter. Other punctuation
# and letters stand for themselves, like \. and \-.
character_escape : List(U32), U32, U64 -> Try({ char : U32, pos : U64 }, [Unsupported(Str)])
character_escape = |chars, letter, pos|
	match letter {
		'n' => Ok({ char: '\n', pos })
		't' => Ok({ char: '\t', pos })
		'r' => Ok({ char: '\r', pos })
		'f' => Ok({ char: 12, pos })
		'v' => Ok({ char: 11, pos })
		'0' =>
			match chars.get(pos) {
				Ok(next) if is_digit(next) => Err(Unsupported("Octal escape"))
				_ => Ok({ char: 0, pos })
			}

		'x' => hex_escape(chars, pos, 2)
		'u' => hex_escape(chars, pos, 4)
		'c' => Err(Unsupported("Control escape"))
		'k' => Err(Unsupported("Backreference"))
		'p' => Err(Unsupported("Unicode property escape"))
		'P' => Err(Unsupported("Unicode property escape"))
		_ if is_digit(letter) => Err(Unsupported("Backreference"))
		_ => Ok({ char: letter, pos })
	}

hex_escape : List(U32), U64, U64 -> Try({ char : U32, pos : U64 }, [Unsupported(Str)])
hex_escape = |chars, pos, count| {
	hex = chars.sublist({ start: pos, len: count })
	if hex.len() == count and hex.all(|digit| hex_digit(digit).is_ok()) {
		Ok({ char: hex.fold(0, |total, digit| total * 16 + hex_digit(digit).ok_or(0)), pos: pos + count })
	} else {
		Err(Unsupported("Malformed escape"))
	}
}

hex_digit : U32 -> Try(U32, [NotHex])
hex_digit = |char|
	if char >= '0' and char <= '9' {
		Ok(char - '0')
	} else if char >= 'a' and char <= 'f' {
		Ok(char - 'a' + 10)
	} else if char >= 'A' and char <= 'F' {
		Ok(char - 'A' + 10)
	} else {
		Err(NotHex)
	}

# The quantifier after a node, if there is one.
parse_quantifier = |chars, pos, node| {
	quantifier =
		match chars.get(pos) {
			Ok('*') => Ok({ min: 0, max: Unbounded, pos: pos + 1 })
			Ok('+') => Ok({ min: 1, max: Unbounded, pos: pos + 1 })
			Ok('?') => Ok({ min: 0, max: AtMost(1), pos: pos + 1 })
			Ok('{') => parse_braces(chars, pos + 1)
			_ => Err(NoQuantifier)
		}

	match quantifier {
		Ok({ min, max, pos: after }) =>
			if is_below(max, min) {
				Err(Unsupported("Quantifier range out of order"))
			} else {
				# A "?" after a quantifier makes it lazy, which changes what a
				# match captures but not whether there is one.
				end = if chars.get(after) == Ok('?') after + 1 else after
				Ok({ node: Node.(Repeat({ body: [node], min, max })), pos: end })
			}

		# A "{" that starts no quantifier is itself, read as the next atom.
		Err(_) => Ok({ node, pos })
	}
}

is_below = |max, min|
	match max {
		AtMost(count) => count < min
		Unbounded => Bool.False
	}

# "{n}", "{n,}" or "{n,m}", after the "{".
parse_braces = |chars, pos| {
	{ value: min, pos: after_min } = parse_count(chars, pos, pos, 0)?
	match chars.get(after_min) {
		Ok('}') => Ok({ min, max: AtMost(min), pos: after_min + 1 })
		Ok(',') =>
			match parse_count(chars, after_min + 1, after_min + 1, 0) {
				Ok({ value: max, pos: after_max }) if chars.get(after_max) == Ok('}') => Ok({ min, max: AtMost(max), pos: after_max + 1 })
				Err(_) if chars.get(after_min + 1) == Ok('}') => Ok({ min, max: Unbounded, pos: after_min + 2 })
				_ => Err(NoQuantifier)
			}

		_ => Err(NoQuantifier)
	}
}

# A decimal number of at least one digit, from `start`.
parse_count = |chars, start, pos, total|
	match chars.get(pos) {
		Ok(char) if is_digit(char) => parse_count(chars, start, pos + 1, total.times_saturated(10).plus_saturated((char - '0').to_u64()))
		_ => if pos > start Ok({ value: total, pos }) else Err(NoQuantifier)
	}

# Matching

# Steps a search may take before it gives up. A step is one node tried at one
# position.
step_limit : U64
step_limit = 1_000_000

# Whether every way through the nodes starts with "^", which makes every start
# but the first fail at once.
is_anchored : List(Node) -> Bool
is_anchored = |nodes|
	match nodes.first() {
		Ok(Node.(InputStart)) => Bool.True
		Ok(Node.(Group(alternatives))) => alternatives.all(is_anchored)
		_ => Bool.False
	}

# Depth first through the ways a match could go, the last pushed first, until
# one matches, they run out, or so do the steps.
search = |input, start, steps| {
	var $stack = [start]
	var $steps = steps
	while !$stack.is_empty() {
		if $steps == 0 {
			return OutOfSteps
		}
		$steps = $steps - 1

		match $stack.last() {
			Ok(state) =>
				match step(state, input) {
					Matched => {
						return Found
					}

					Next(states) => {
						# Not drop_last, which on the interpreter leaves a slice that
						# the next push copies whole, so a long input took
						# quadratic time.
						$stack = $stack.drop_at($stack.len() - 1).concat(states)
					}
				}

			Err(_) => {}
		}
	}
	NotFound($steps)
}

# Where matching the first node of `todo` at `pos` leads: the states to try
# next, or a whole match when there is nothing left to match.
step = |{ todo, pos }, input|
	match todo.first() {
		Err(_) => Matched
		Ok(Node.(node)) => {
			rest = todo.drop_first(1)
			here = input.get(pos)
			advance = [{ todo: rest, pos: pos + 1 }]
			stay = [{ todo: rest, pos }]

			Next(
				match node {
					Char(expected) => if here == Ok(expected) advance else []
					AnyChar =>
						match here {
							Ok(char) if !is_line_terminator(char) => advance
							_ => []
						}

					Class({ negated, items }) =>
						match here {
							Ok(char) if covers(items, char) != negated => advance
							_ => []
						}

					InputStart => if pos == 0 stay else []
					InputEnd => if pos == input.len() stay else []
					WordBoundary => if at_word_boundary(input, pos) stay else []
					NotWordBoundary => if at_word_boundary(input, pos) [] else stay
					# The first alternative goes on top of the stack.
					Group(alternatives) => alternatives.fold_rev([], |alternative, states| states.append({ todo: alternative.concat(rest), pos }))
					Repeat({ body, min, max }) =>
						if min > 0 {
							[{ todo: body.append(Node.(Repeat({ body, min: min - 1, max: one_less(max) }))).concat(rest), pos }]
						} else {
							match max {
								AtMost(0) => stay
								# Another round goes on top, so the repetition is greedy.
								_ => [{ todo: rest, pos }, { todo: body.concat([Node.(MovedPast(pos)), Node.(Repeat({ body, min: 0, max: one_less(max) }))]).concat(rest), pos }]
							}
						}

					MovedPast(from) => if pos > from stay else []
				},
			)
		}
	}

one_less = |max|
	match max {
		AtMost(count) => AtMost(count - 1)
		Unbounded => Unbounded
	}

covers : List(ClassItem), U32 -> Bool
covers = |items, char|
	items.any(
		|item|
			match item {
				Within(range) => in_range(range, char)
				Outside(ranges) => !ranges.any(|range| in_range(range, char))
			},
	)

in_range : CharRange, U32 -> Bool
in_range = |{ from, to }, char| char >= from and char <= to

at_word_boundary : List(U32), U64 -> Bool
at_word_boundary = |input, pos| {
	before = if pos == 0 Bool.False else input.get(pos - 1).map_ok(is_word_char).ok_or(Bool.False)
	after = input.get(pos).map_ok(is_word_char).ok_or(Bool.False)
	before != after
}

is_word_char : U32 -> Bool
is_word_char = |char| word_chars.any(|range| in_range(range, char))

is_line_terminator : U32 -> Bool
is_line_terminator = |char| char == '\n' or char == '\r' or char == 0x2028 or char == 0x2029

is_digit : U32 -> Bool
is_digit = |char| char >= '0' and char <= '9'

digits : List(CharRange)
digits = [{ from: '0', to: '9' }]

word_chars : List(CharRange)
word_chars = [{ from: 'a', to: 'z' }, { from: 'A', to: 'Z' }, { from: '0', to: '9' }, { from: '_', to: '_' }]

# What \s matches: ECMAScript's whitespace and line terminators.
spaces : List(CharRange)
spaces = [
	{ from: 0x09, to: 0x0D },
	{ from: 0x20, to: 0x20 },
	{ from: 0xA0, to: 0xA0 },
	{ from: 0x1680, to: 0x1680 },
	{ from: 0x2000, to: 0x200A },
	{ from: 0x2028, to: 0x2029 },
	{ from: 0x202F, to: 0x202F },
	{ from: 0x205F, to: 0x205F },
	{ from: 0x3000, to: 0x3000 },
	{ from: 0xFEFF, to: 0xFEFF },
]

# The characters of a string. A `Str` is valid UTF-8, so a continuation byte
# always follows a lead byte, and adds its six bits to the character that
# lead byte started.
code_points : Str -> List(U32)
code_points = |str|
	str.to_utf8().fold(
		[],
		|chars, byte|
			if byte.bitwise_and(0xC0) == 0x80 {
				chars
					.update(chars.len() - 1, |char| char.shl_wrap(6).bitwise_or(byte.bitwise_and(0x3F).to_u32()))
					.ok_or(chars)
			} else {
				chars.append(lead_bits(byte))
			},
	)

lead_bits : U8 -> U32
lead_bits = |byte|
	if byte < 0x80 {
		byte.to_u32()
	} else if byte < 0xE0 {
		byte.bitwise_and(0x1F).to_u32()
	} else if byte < 0xF0 {
		byte.bitwise_and(0x0F).to_u32()
	} else {
		byte.bitwise_and(0x07).to_u32()
	}

# Tests

# Whether `source` matches `str`, with Err for an unsupported pattern.
found : Str, Str -> Try(Bool, [Unsupported(Str), TooComplex])
found = |source, str| {
	pattern = Pattern.parse(source)?
	pattern.matches(str)
}

expect found("abc", "abc") == Ok(Bool.True)

# Unanchored, a pattern matches anywhere.
expect found("github", "a github repo") == Ok(Bool.True)

expect found("github", "gitlab") == Ok(Bool.False)

expect found("^refs/", "refs/heads/main") == Ok(Bool.True)

expect found("^refs/", "x/refs/") == Ok(Bool.False)

expect found("^ab$", "ab") == Ok(Bool.True)

expect found("^ab$", "abc") == Ok(Bool.False)

expect found("", "anything") == Ok(Bool.True)

# Twilio's most common pattern.
account_sid : Str
account_sid = "^AC[0-9a-fA-F]{32}$"

expect found(account_sid, "AC${Str.repeat("a", 32)}") == Ok(Bool.True)

expect found(account_sid, "AC${Str.repeat("a", 31)}") == Ok(Bool.False)

expect found(account_sid, "AC${Str.repeat("a", 33)}") == Ok(Bool.False)

expect found(account_sid, "AC${Str.repeat("g", 32)}") == Ok(Bool.False)

expect found("^(SM|MM)[0-9a-f]{2}$", "MMab") == Ok(Bool.True)

expect found("^(SM|MM)[0-9a-f]{2}$", "XMab") == Ok(Bool.False)

expect found("^refs/(heads|tags|pull)/.*$", "refs/tags/v1") == Ok(Bool.True)

expect found("^refs/(heads|tags|pull)/.*$", "refs/notes/v1") == Ok(Bool.False)

expect found("^\\d+\\.\\d+\\.\\d+$", "1.22.333") == Ok(Bool.True)

expect found("^\\d+\\.\\d+\\.\\d+$", "1.22") == Ok(Bool.False)

expect found("^\\d+\\.\\d+\\.\\d+$", "1x2x3") == Ok(Bool.False)

# A "-" next to an escape, and at the end of a class, is itself.
expect found("^[A-Za-z0-9.\\-_]+$", "a-b_c.d") == Ok(Bool.True)

expect found("^[a-zA-Z0-9._-]+$", "a b") == Ok(Bool.False)

expect found("^/v1/tax/calculations/[^/]+/line_items", "/v1/tax/calculations/calc_1/line_items") == Ok(Bool.True)

expect found("^/v1/tax/calculations/[^/]+/line_items", "/v1/tax/calculations/a/b/line_items") == Ok(Bool.False)

# GitHub's base64 pattern: groups of four, then a padded or full group.
expect {
	base64 = "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{4})$"
	found(base64, "aGVsbG8=") == Ok(Bool.True) and found(base64, "aGVsbG8") == Ok(Bool.False)
}

# Alternatives with an anchor each.
ssh_key : Str
ssh_key = "^ssh-(rsa|dss|ed25519) |^ecdsa-sha2-nistp(256|384|521) "

expect found(ssh_key, "ssh-ed25519 AAAA") == Ok(Bool.True)

expect found(ssh_key, "ecdsa-sha2-nistp384 AAAA") == Ok(Bool.True)

expect found(ssh_key, "ssh-foo AAAA") == Ok(Bool.False)

# Quantifiers

expect found("^a?b$", "b") == Ok(Bool.True)

expect found("^a?b$", "aab") == Ok(Bool.False)

expect found("^a{2,}$", "aaaa") == Ok(Bool.True)

expect found("^a{2,}$", "a") == Ok(Bool.False)

expect found("^a{1,2}$", "aaa") == Ok(Bool.False)

# Lazy quantifiers find the same matches.
expect found("^a+?$", "aaa") == Ok(Bool.True)

# A "{" that starts no quantifier is itself.
expect found("^a{x}$", "a{x}") == Ok(Bool.True)

expect found("^a{,2}$", "a{,2}") == Ok(Bool.True)

# A repetition of something that can match nothing still ends.
expect found("^(a*)*$", "aaa") == Ok(Bool.True)

expect found("^(a*)*b$", "aaa") == Ok(Bool.False)

# Classes and escapes

expect found("^[^0-9]+$", "abc") == Ok(Bool.True)

expect found("^[^0-9]+$", "a1c") == Ok(Bool.False)

expect found("^\\w+\\s\\w+$", "hello world") == Ok(Bool.True)

expect found("^\\W$", "_") == Ok(Bool.False)

expect found("^[\\D]+$", "abc") == Ok(Bool.True)

expect found("^[\\D]+$", "a1") == Ok(Bool.False)

expect found("\\bcat\\b", "a cat sat") == Ok(Bool.True)

expect found("\\bcat\\b", "concatenate") == Ok(Bool.False)

expect found("\\Bcat\\B", "concatenate") == Ok(Bool.True)

expect found("^\\x41\\u00e9$", "Aé") == Ok(Bool.True)

expect found("^\\$\\.\\(\\)$", "$.()") == Ok(Bool.True)

# "." is any character but a line terminator, and it is a character, not a
# byte.
expect found("^.$", "é") == Ok(Bool.True)

expect found("^..$", "🦅") == Ok(Bool.False)

expect found("^.$", "\n") == Ok(Bool.False)

expect found("^[à-ö]$", "ä") == Ok(Bool.True) and found("^[å-ö]$", "ä") == Ok(Bool.False)

# Named and non-capturing groups match like any group.
expect found("^(?<year>\\d{4})-(?:\\d{2})$", "2026-10") == Ok(Bool.True)

# Unsupported

expect found("(?=a)", "a") == Err(Unsupported("Lookaround"))

expect found("(?<!a)b", "b") == Err(Unsupported("Lookaround"))

expect found("(a)\\1", "aa") == Err(Unsupported("Backreference"))

expect found("\\p{L}", "a") == Err(Unsupported("Unicode property escape"))

expect found("a)", "a") == Err(Unsupported("Unbalanced ) in a)"))

expect found("(a", "a") == Err(Unsupported("Unclosed group"))

expect found("[a", "a") == Err(Unsupported("Unclosed class"))

expect found("[z-a]", "a") == Err(Unsupported("Class range out of order"))

expect found("*a", "a") == Err(Unsupported("Nothing to repeat"))

# Backtracking that would take exponential time gives up instead.
expect found("^(a+)+b$", Str.repeat("a", 40)) == Err(TooComplex)

# A long string is fine when the pattern is anchored.
expect found("^[a-z]+$", Str.repeat("a", 100_000)) == Ok(Bool.True)
