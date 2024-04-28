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