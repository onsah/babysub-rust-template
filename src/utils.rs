#![macro_use]

#[cfg(feature = "logging")]
macro_rules! LOG {
    ($($arg:tt)*) => {{
        println!("c LOG {}", format_args!($($arg)*));
    }};
}

#[cfg(not(feature = "logging"))]
macro_rules! LOG {
    ($($arg:tt)*) => {{}};
}

pub(crate) use LOG;

macro_rules! parse_error {
    ($ctx:expr, $msg:expr, $line:expr) => {{
        use std::process;
        eprintln!(
            "babysub: parse error: at line {}: {}",
            $line, $msg
        );
        process::exit(1);
    }};
}

macro_rules! message {
    ($ctx:expr, $($arg:tt)*) => {{
        if $ctx.verbosity >= 0 {
            use std::io::Write;
            if let Err(e) = writeln!($ctx.writer, "{}", format!("c {}", format_args!($($arg)*))) {
                die!("Failed to write message: {}", e);
            }
            if let Err(f) = $ctx.writer.flush() {
                die!("Failed to flush writer: {}", f);
        }}
    }}
}

macro_rules! die {
    ($($arg:tt)*) => {{
        eprintln!("babysub: error: {}", format!($($arg)*));
        std::process::exit(1);
    }}
}

macro_rules! verbose {
    ($ctx:expr, $level:expr, $($arg:tt)*) => {{
        if $ctx.verbosity == $level {
            use std::io::Write;
            if let Err(e) = writeln!($ctx.writer, "{}", format!("c {}", format_args!($($arg)*))) {
                die!("Failed to write message: {}", e);
            }
            if let Err(f) = $ctx.writer.flush() {
                die!("Failed to flush writer: {}", f);
        }}
    }}
}
