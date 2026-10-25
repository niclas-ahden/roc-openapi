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
	pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.30.0/6XAUy48Jp34GANurMrMf74DtF7NdpkhnqFLHE4gHNzJy.tar.zst",
	spec: "https://github.com/niclas-ahden/roc-spec/releases/download/0.7.0/BGjQKbG9U3V8cH6mRbypUAFVqFoVEu6n9bsA3QmFL1f4.tar.zst",
}

import pf.Cmd
import pf.Env
import pf.OsStr
import pf.Path
import pf.Sleep
import pf.Stderr
import pf.Stdout
import pf.Utc
import spec.Spec

## The basic-cli functions `Spec` uses, passed as they are.
hooks = {
	new: Cmd.new,
	os_str: OsStr.from_str,
	# Leashed, so a test killed on timeout takes its descendants with it.
	spawn!: Cmd.Command.spawn_leashed!,
	poll!: Cmd.Child.poll!,
	wait!: Cmd.Child.wait!,
	kill!: Cmd.Child.kill!,
	path: Path.utf8,
	list!: Path.list!,
	print!: Stdout.line!,
	now!: Utc.now!,
	sleep!: Sleep.millis!,
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
			_ = Cmd.new("roc").args(["check", OsStr.from_str(file)]).run!()
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
		hooks,
		"tests",
		{
			test_command: ["roc", "--opt=${opt!({})}"],
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
