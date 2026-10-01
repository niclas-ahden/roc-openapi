## A path template from a spec's "paths", such as
## `/repos/{owner}/{repo}/labels` or `/Messages/{Sid}.json`, to match the paths
## of requests against.
import url.Uri

# A segment of a template is literal text and `{variables}` in turn. A
# variable stands for at least one byte of the request's segment.
Part : [Literal(List(U8)), Variable]

PathTemplate :: List(List(Part)).{

	parse : Str -> PathTemplate
	parse = |template| PathTemplate.(segments_of(template).map(parse_segment))

	## How specifically the template matches the segments of a request path,
	## with a number for each segment: 0 for literal text, 1 for text with a
	## variable in it, and 2 for a variable alone. Of two templates that
	## match, the more specific is the one with the lower number where they
	## first differ, so `/users/me` beats `/users/{id}`.
	rank : PathTemplate, List(Str) -> Try(List(U8), [NoMatch])
	rank = |PathTemplate.(segments), request| rank_segments(segments, request, [])

	## The percent-decoded segments of a request's path, which can come as a
	## whole URL. Its query and fragment are left out.
	request_segments : Str -> List(Str)
	request_segments = |path|
		segments_of(Uri.parse(path).path()).map(|segment| Uri.percent_decode(segment).ok_or(segment))

	## The path a server's URL puts in front of every path in the spec, such
	## as `/v1` for `https://api.example.com/v1/`. It is empty for
	## `https://api.example.com`.
	base_path : Str -> Str
	base_path = |url| Uri.parse(url).path().drop_suffix("/")
}

segments_of : Str -> List(Str)
segments_of = |path| path.drop_prefix("/").split_on("/")

parse_segment : Str -> List(Part)
parse_segment = |text|
	match text.split_first("{") {
		Ok({ before, after }) =>
			match after.split_first("}") {
				Ok({ before: _, after: rest }) => literal(before).append(Variable).concat(parse_segment(rest))
				# A "{" that is never closed is literal text.
				Err(NotFound) => literal(text)
			}

		Err(NotFound) => literal(text)
	}

literal : Str -> List(Part)
literal = |text| if text.is_empty() [] else [Literal(text.to_utf8())]

rank_segments : List(List(Part)), List(Str), List(U8) -> Try(List(U8), [NoMatch])
rank_segments = |segments, request, ranks|
	match (segments, request) {
		([], []) => Ok(ranks)
		([parts, .. as rest], [segment, .. as remaining]) =>
			if matches(parts, segment.to_utf8()) {
				rank_segments(rest, remaining, ranks.append(kind(parts)))
			} else {
				Err(NoMatch)
			}

		_ => Err(NoMatch)
	}

kind : List(Part) -> U8
kind = |parts|
	if !parts.contains(Variable) {
		0
	} else if parts == [Variable] {
		2
	} else {
		1
	}

matches : List(Part), List(U8) -> Bool
matches = |parts, bytes|
	match parts {
		[] => bytes.is_empty()
		[Literal(text), .. as rest] => bytes.starts_with(text) and matches(rest, bytes.drop_first(text.len()))
		[Variable] => !bytes.is_empty()
		# A variable takes at least one byte, and as many as leave a match for
		# the rest, as in `{Sid}.json`.
		[Variable, .. as rest] =>
			List.repeat(0, bytes.len())
				.map_with_index(|_, index| index + 1)
				.any(|taken| matches(rest, bytes.drop_first(taken)))
	}

# Tests

ranked : Str, Str -> Try(List(U8), [NoMatch])
ranked = |template, path| PathTemplate.parse(template).rank(PathTemplate.request_segments(path))

expect ranked("/widgets", "/widgets") == Ok([0])

expect ranked("/widgets/{id}", "/widgets/42") == Ok([0, 2])

expect ranked("/widgets/{id}", "/widgets/42/parts") == Err(NoMatch)

expect ranked("/widgets/{id}", "/widgets") == Err(NoMatch)

# A variable is never empty.
expect ranked("/widgets/{id}", "/widgets/") == Err(NoMatch)

# Variables in a segment with literal text, as Twilio's paths have them.
expect ranked("/Messages/{Sid}.json", "/Messages/SM123.json") == Ok([0, 1])

expect ranked("/Messages/{Sid}.json", "/Messages/.json") == Err(NoMatch)

expect ranked("/Messages/{Sid}.json", "/Messages/SM123.xml") == Err(NoMatch)

expect ranked("/files/{name}.{format}", "/files/a.b.json") == Ok([0, 1])

# The query and fragment are left out, and the host does not matter.
expect ranked("/v3/mail/send", "https://api.sendgrid.com/v3/mail/send?dry=1#top") == Ok([0, 0, 0])

expect ranked("/v3/mail/send", "/v3/mail/send?dry=1") == Ok([0, 0, 0])

# Segments are percent-decoded, so an encoded "/" stays inside its segment.
expect ranked("/files/{name}", "/files/a%2Fb") == Ok([0, 2])

expect ranked("/users/{name}", "/users/%C3%B6rjan") == Ok([0, 2])

expect PathTemplate.request_segments("/users/%C3%B6rjan") == ["users", "örjan"]

expect ranked("/", "/") == Ok([0])

expect ranked("/", "https://example.com") == Ok([0])

# A "{" that is never closed is literal text.
expect ranked("/a{b", "/a{b") == Ok([0])
