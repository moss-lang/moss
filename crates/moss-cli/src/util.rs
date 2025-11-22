use std::{backtrace::BacktraceStatus, process::ExitCode};

/// Print `error` to stderr, then return [`ExitCode::FAILURE`].
pub fn err_fail(error: anyhow::Error) -> ExitCode {
    eprintln!("{error:#}");
    let backtrace = error.backtrace();
    match backtrace.status() {
        BacktraceStatus::Disabled => eprintln!(
            "note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace"
        ),
        BacktraceStatus::Captured => eprint!("{backtrace}"),
        _ => {}
    }
    ExitCode::FAILURE
}
