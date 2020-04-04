use criterion::{criterion_group, criterion_main, Criterion};
use limonite::lexical::Lexer;
use limonite::interner::StrId;

fn consume_while_benchmark(c: &mut Criterion) {
    let s = "Hello, World!";

    c.bench_function("consume_while", |b| b.iter(|| {
        let mut lexer = Lexer::new(s, StrId::DUMMY);

        lexer.consume_while(|c| c.is_alphanumeric()).node();
    }));
}

criterion_group!(benches, consume_while_benchmark);
criterion_main!(benches);
