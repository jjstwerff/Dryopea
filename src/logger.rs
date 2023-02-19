// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! A logger with a rate limiter inside to create logs but also prevent them from quickly overflowing.
//! The logger is used as a basis to provide log rotation.

extern crate flexi_logger;
pub extern crate log;

use self::flexi_logger::filter::{LogLineFilter, LogLineWriter};
use self::flexi_logger::*;
use std::cell::RefCell;
use std::collections::HashMap;

type Limit = RefCell<HashMap<(&'static str, u32), (u32, i64)>>;

thread_local! {
    static RATE_LIMIT: Limit = RefCell::new(HashMap::new());
    static LOGGER: RefCell<Option<LoggerHandle>> = RefCell::new(None);
}

/// per 10 minutes after the first encountered message we allow a specific message 5 times
#[derive(Debug)]
struct RateLimiter {}

impl RateLimiter {
    pub fn new() -> RateLimiter {
        RateLimiter {}
    }
}

impl LogLineFilter for RateLimiter {
    fn write(
        &self,
        now: &mut DeferredNow,
        record: &Record,
        log_line_writer: &dyn LogLineWriter,
    ) -> std::io::Result<()> {
        RATE_LIMIT.with(|rate_map| {
            let timestamp = now.now().timestamp_millis();
            let mut map = rate_map.borrow_mut();
            let (cnt, time) = map
                .entry((record.file_static().unwrap(), record.line().unwrap()))
                .or_insert_with(|| (1, timestamp));
            if timestamp - *time > 600 {
                *time = timestamp;
                *cnt = 1;
            }
            if *cnt < 6 {
                *cnt += 1;
                log_line_writer.write(now, record)
            } else {
                Ok(())
            }
        })
    }
}

#[allow(dead_code)]
pub fn init_logging() {
    if let Ok(log) = Logger::try_with_env_or_str("debug")
        .unwrap()
        .format(with_thread)
        .log_to_file(
            FileSpec::default()
                .directory("logging")
                .basename("log")
                .suffix("txt"), // use suffix .trc instead of .log
        )
        .write_mode(WriteMode::BufferAndFlush)
        .rotate(
            Criterion::Age(Age::Day), // - create a new file every day
            Naming::Timestamps,       // - let the rotated files have a timestamp in their name
            Cleanup::KeepLogFiles(7), // - keep at most 7 log files
        )
        .filter(Box::new(RateLimiter::new()))
        .start()
    {
        LOGGER.with(|logger| *logger.borrow_mut() = Some(log))
    }
}
