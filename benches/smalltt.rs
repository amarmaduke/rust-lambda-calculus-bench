

use rust_lambda_calculus_bench::*;

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use serde_lexpr::from_str;
use pprof::criterion::{PProfProfiler, Output};

fn test_input(body: &'static str) -> common::Syntax {
    let text = format!(r#"
        (let "true" (abs "t" (abs "f" (var . "t")))
        (let "and" (abs "a" (abs "b" (abs "t" (abs "f"
                (app (app (var . "a")
                        (app (app (var . "b") (var . "t")) (var . "f")))
                    (var . "f"))
            ))))
        (let "add" (abs "a" (abs "b" (abs "s" (abs "z"
                (app (app (var . "a") (var . "s"))
                    (app (app (var . "b") (var . "s")) (var . "z"))))
            )))
        (let "mul" (abs "a" (abs "b" (abs "s"
                (app (var . "a") (app (var . "b") (var . "s")))
            )))
        (let "suc" (abs "a" (abs "s" (abs "z"
                (app (var . "s")
                    (app (app (var . "a") (var . "s")) (var . "z")))
            )))
        (let "n1" (abs "s" (abs "z" (app (var . "s") (var . "z"))))
        (let "n2" (app (var . "suc") (var . "n1"))
        (let "n3" (app (var . "suc") (var . "n2"))
        (let "n4" (app (var . "suc") (var . "n3"))
        (let "n5" (app (var . "suc") (var . "n4"))
        (let "n10"  (app (app (var . "mul") (var . "n2")) (var . "n5"))
        (let "n10b"  (app (app (var . "mul") (var . "n5")) (var . "n2"))
        (let "n15"  (app (app (var . "add") (var . "n10")) (var . "n5"))
        (let "n15b"  (app (app (var . "add") (var . "n10b")) (var . "n5"))
        (let "n18"  (app (app (var . "add") (var . "n15")) (var . "n3"))
        (let "n18b"  (app (app (var . "add") (var . "n15b")) (var . "n3"))
        (let "n19"  (app (app (var . "add") (var . "n15")) (var . "n4"))
        (let "n19b"  (app (app (var . "add") (var . "n15b")) (var . "n4"))
        (let "n20"  (app (app (var . "mul") (var . "n2")) (var . "n10"))
        (let "n20b"  (app (app (var . "mul") (var . "n2")) (var . "n10b"))
        (let "n21"  (app (var . "suc") (var . "n20"))
        (let "n21b"  (app (var . "suc") (var . "n20b"))
        (let "n22"  (app (var . "suc") (var . "n21"))
        (let "n22b"  (app (var . "suc") (var . "n21b"))
        (let "n23"  (app (var . "suc") (var . "n22"))
        (let "n23b"  (app (var . "suc") (var . "n22b"))
        (let "leaf" (abs "n" (abs "l" (var . "l")))
        (let "node" (abs "t1" (abs "t2" (abs "n" (abs "l"
                (app (app (var . "n")
                    (app (app (var . "t1") (var . "n")) (var . "l")))
                    (app (app (var . "t2") (var . "n")) (var . "l")))
            ))))
        (let "fullTree" (abs "n" (app (app (var . "n") 
                (abs "t" (app (app (var . "node") (var . "t")) (var . "t"))))
                (var . "leaf")))
        (let "forcedTree" (abs "t" (app (app (var . "t") (var . "and")) (var . "true")))
        (let "t15" (app (var . "fullTree") (var . "n15"))
        (let "t15b" (app (var . "fullTree") (var . "n15b"))
        (let "t18" (app (var . "fullTree") (var . "n18"))
        (let "t18b" (app (var . "fullTree") (var . "n18b"))
        (let "t19" (app (var . "fullTree") (var . "n19"))
        (let "t19b" (app (var . "fullTree") (var . "n19b"))
        (let "t20" (app (var . "fullTree") (var . "n20"))
        (let "t20b" (app (var . "fullTree") (var . "n20b"))
        (let "t21" (app (var . "fullTree") (var . "n21"))
        (let "t21b" (app (var . "fullTree") (var . "n21b"))
        (let "t22" (app (var . "fullTree") (var . "n22"))
        (let "t22b" (app (var . "fullTree") (var . "n22b"))
        (let "t23" (app (var . "fullTree") (var . "n23"))
        (let "t23b" (app (var . "fullTree") (var . "n23b"))
            {}
        ))))))))))))))))))))))))))))))))))))))))))))
    "#, body);
    // (var . "t23")
    // (app (var . "forcedTree") (var . "t23"))
    from_str(&text).unwrap()
}

macro_rules! make_benchmark {
    ($name:ident, $input:expr, $test:expr) => {
        fn $name(c: &mut Criterion) {
            let syntax = test_input($input);
            let basic_term = basic::from_syntax(syntax.clone(), vec![]);
            let basichc_term = basichc::from_syntax(syntax.clone(), vec![]);
            let functor_term = functor::from_syntax(syntax.clone(), vec![]);
            let rec_term = rec::from_syntax(syntax.clone(), vec![]);
            let nogc_term = nogc::from_syntax(syntax.clone(), vec![]);
            let optimal_term = optimal::from_syntax(syntax.clone());
        
            let mut group = c.benchmark_group(format!("smalltt normalize {}", $test));
            group.bench_with_input(BenchmarkId::new("Basic", $test), &basic_term, |b, i| b.iter(|| {
                basic::normalize(i.clone())
            }));
            // group.bench_with_input(BenchmarkId::new("BasicHc", $test), &basichc_term, |b, i| b.iter(|| {
            //     basichc::clear();
            //     basichc::normalize(i.clone())
            // }));
            // group.bench_with_input(BenchmarkId::new("Functor", $test), &functor_term, |b, i| b.iter(|| {
            //     functor::normalize(i.clone())
            // }));
            // group.bench_with_input(BenchmarkId::new("Rec", $test), &rec_term, |b, i| b.iter(|| {
            //     rec::normalize(i.clone())
            // }));
            group.bench_with_input(BenchmarkId::new("Optimal", $test), &optimal_term, |b, i| b.iter(|| {
                optimal::normalize(i.clone())
            }));
            // Will usually crash on high memory tests
            // group.bench_with_input(BenchmarkId::new("NoGc", $test), &nogc_term, |b, i| b.iter(|| {
            //     nogc::normalize(i.clone())
            // }));
            group.finish();
        } 
    }
}

make_benchmark!(benchmark_normalize_t15, "(var . \"t15\")", "t15");
make_benchmark!(benchmark_normalize_t20, "(var . \"t20\")", "t20");
//make_benchmark!(benchmark_normalize_t23, "(var . \"t23\")", "t23");
make_benchmark!(benchmark_normalize_forcedtree_t15, "(app (var . \"forcedTree\") (var . \"t15\"))", "forcedTree t15");
//make_benchmark!(benchmark_normalize_forcedtree_t20, "(app (var . \"forcedTree\") (var . \"t20\"))", "forcedTree t20");
//make_benchmark!(benchmark_normalize_forcedtree_t23, "(app (var . \"forcedTree\") (var . \"t23\"))", "forcedTree t23");

criterion_group!{
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets =
        benchmark_normalize_t15,
        benchmark_normalize_t20,
        //benchmark_normalize_t23,
        benchmark_normalize_forcedtree_t15,
        //benchmark_normalize_forcedtree_t20,
        //benchmark_normalize_forcedtree_t23,
}
criterion_main!(benches);
