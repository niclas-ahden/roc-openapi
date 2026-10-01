#!/usr/bin/env roc
## Runs every tests/*_test.roc as a standalone app through roc-spec.
## Each app validates payloads against a committed OpenAPI spec (SendGrid,
## GitHub, Stripe, Twilio).
## The unit tests are the `expect`s in package/, which
## `roc test package/main.roc` runs.
##
## Optional args: a filename pattern (substring) and --fail-fast, e.g.
## `./tests.roc stripe`.
## Optional env: ROC_OPT (default speed).
##
## Run from the repository root. CI runs this via `nix develop -c ./tests.roc`.
app [main!] {
	pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.27.0/HZanbveSUDoJF8LypR663eH7PpaKEKG36eErEQzmV1Qs.tar.zst",
	spec: "https://github.com/niclas-ahden/roc-spec/releases/download/0.6.0/9ThTkhd7zrviwQpM3LvGd7pvzGhr4ZXNmWJV7pTJc9AJ.tar.zst",
}

import pf.Cmd
import pf.Env
import pf.OsStr exposing [OsStr]
import pf.Path
import pf.Sleep
import pf.Stderr
import pf.Stdout
import pf.Utc
import spec.Spec

effects = {
	spawn_test!: |file, envs|
		Cmd.new(OsStr.utf8("roc"))
			.args_str(["--opt=${opt!({})}", file])
			.envs_str(envs)
			.stdout(Capture)
			.stderr(Capture)
			.spawn_leashed!(),
	try_wait!: Cmd.Child.try_wait!,
	kill!: Cmd.Child.kill!,
	wait!: Cmd.Child.wait!,
	list_dir!: |dir| Path.list!(Path.utf8(dir)).map_ok(|entries| entries.map(Path.display)),
	print!: Stdout.line!,
	utc_now!: Utc.now!,
	sleep_millis!: Sleep.millis!,
}

## Optimization level every test is built with.
opt! : {} => Str
opt! = |{}|
	match Env.var_str!(OsStr.from_str("ROC_OPT")) {
		Ok(val) if val != "" => val
		_ => "speed"
	}

## Parse command line args into pattern and flags. basic-cli passes only the
## args after the script, without the program name.
parse_args : List(Str) -> { pattern : Str, fail_fast : Bool }
parse_args = |args| {
	pattern = args.keep_if(|a| !a.starts_with("--")).first().ok_or("")
	fail_fast = args.contains("--fail-fast")
	{ pattern, fail_fast }
}

## Download and extract every package the tests depend on, in one process,
## before any worker starts. Every test file carries the same dependency
## header, so checking the first one fetches for all of them.
##
## The compiler does not lock the package cache: the first `roc` to want a
## missing package creates its cache directory and starts extracting into it,
## while every other `roc` sees that directory, takes it for a finished
## download, and dies with "PACKAGE DOWNLOAD FAILED ... FileNotFound". On a
## cold cache that wipes out every test that loses the race.
warm_package_cache! : Str => Try({}, _)
warm_package_cache! = |test_dir| {
	# A directory that cannot be listed is for `Spec` to report.
	entries = Path.list!(Path.utf8(test_dir)).ok_or([])
	match entries.map(Path.display).find_first(|name| name.ends_with("_test.roc")) {
		Err(_) => Ok({})
		Ok(file) => {
			Stdout.line!("Warming the package cache...")?
			# Whatever this reports about the file itself is the test run's
			# business, so the output and the exit code are both dropped here.
			_ = Cmd.new(OsStr.utf8("roc")).args_str(["check", file]).exec_output!()
			Ok({})
		}
	}
}

main! : List(OsStr) => Try({}, _)
main! = |os_args| {
	args = os_args.map(|a| OsStr.display(a))
	{ pattern, fail_fast } = parse_args(args)

	warm_package_cache!("tests")?

	results = Spec.run_filtered!(
		effects,
		"tests",
		{
			max_workers: 4,
			worker_envs: |_index| [],
			before_each!: |_index| Ok({}),
			per_test_timeout_ms: 120_000,
			quiet: Bool.True,
			fail_fast,
		},
		pattern,
	)?

	passed = results.count_if(|r| r.passed)
	total = results.len()

	Stdout.line!("")?
	Stdout.line!("${passed.to_str()}/${total.to_str()} tests passed")?

	# A pattern that matches nothing is a failure: a typo'd filter must not
	# produce a green "0/0 passed" run.
	if total == 0 {
		Stderr.line!("No tests matched the pattern '${pattern}'")?
		Err(NoTestsMatched)
	} else if passed == total {
		Ok({})
	} else {
		Err(TestsFailed)
	}
}
