use beans::regex::{Allowed, RegexBuilder};
use criterion::{criterion_group, criterion_main, Criterion};
use rand::{distributions::Alphanumeric, Rng};
use regex::Regex;

pub fn regex_email(criterion: &mut Criterion) {
    // Setup
    let mut group = criterion.benchmark_group("Email");
    let pattern = r"(.*[a-zA-Z0-9.\-]+@[a-zA-Z0-9.\-]+\.\w+.*)";
    let beans_engine = RegexBuilder::new()
        .with_named_regex(pattern, String::from("Email"))
        .unwrap()
        .build();
    let std_engine = Regex::new(pattern).unwrap();
    let size = 1_000_000;
    let mut content: String = rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(size)
        .map(char::from)
        .collect();
    content.push_str("some.address.mail@some.domain");
    // Start benchmark
    group.bench_function("Regex email: beans engine", |b| {
        b.iter(|| beans_engine.find(&content, &Allowed::All))
    });
    group.bench_function("Regex email: std engine", |b| {
        b.iter(|| std_engine.captures(&content))
    });
    group.finish();
}

criterion_group!(benches, regex_email);
criterion_main!(benches);
