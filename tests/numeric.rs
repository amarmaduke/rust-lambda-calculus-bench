
use rust_lambda_calculus_bench::*;
use rust_lambda_calculus_bench::common::*;

use proptest::prelude::*;
use serde_lexpr::from_str;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Expr {
    Const(u8),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>)
}

fn expr_strategy() -> impl Strategy<Value = Expr> {
    let leaf = prop_oneof![
        (1u8..=8u8).prop_map(Expr::Const),
    ];
    leaf.prop_recursive(4, 128, 10, |inner| {
        prop_oneof![
            (inner.clone(), inner.clone()).prop_map(|(x, y)| Expr::Add(Box::new(x), Box::new(y))),
            (inner.clone(), inner.clone()).prop_map(|(x, y)| Expr::Mul(Box::new(x), Box::new(y)))
        ]
    })
}

fn eval(e : &Expr) -> u64 {
    match e {
        Expr::Const(x) => *x as u64,
        Expr::Add(x, y) => eval(x) + eval(y),
        Expr::Mul(x, y) => eval(x) * eval(y),
    }
}

fn to_input(e : &Expr) -> String {
    match e {
        Expr::Const(x) => format!(r#"(var . "n{}")"#, *x),
        Expr::Add(x, y) => {
            let x = to_input(x);
            let y = to_input(y);
            format!(r#"(app (app (var . "add") {}) {})"#, x, y)
        }
        Expr::Mul(x, y) =>  {
            let x = to_input(x);
            let y = to_input(y);
            format!(r#"(app (app (var . "mul") {}) {})"#, x, y)
        }
    }
}

fn test_input(body: &str) -> common::Syntax {
    let text = format!(r#"
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
        (let "n6" (app (var . "suc") (var . "n5"))
        (let "n7" (app (var . "suc") (var . "n6"))
        (let "n8" (app (var . "suc") (var . "n7"))
            {}
        )))))))))))
    "#, body);
    from_str(&text).unwrap()
}

proptest! {
    // #[test]
    // fn test_eval_basic(e in expr_strategy()) {
    //     let value = eval(&e);
    //     let syntax = test_input(to_input(&e).as_str());
    //     let basic_term = basic::from_syntax(syntax.clone(), vec![]);
    //     let normal = basic::normalize(basic_term);
    //     let normal_syntax = basic::to_syntax(normal);
    //     let normal_value = Some(normal_syntax)
    //         .and_then(|x| try_discard_lets(&x))
    //         .and_then(|x| try_numeral(&x));
    //     assert_eq!(Some(value), normal_value);
    // }

    // #[test]
    // fn test_eval_optimal(e in expr_strategy()) {
    //     let value = eval(&e);
    //     let syntax = test_input(to_input(&e).as_str());
    //     let optimal_term = optimal::from_syntax(syntax.clone());
    //     let normal = optimal::normalize(optimal_term);
    //     let normal_syntax = optimal::to_syntax(normal);
    //     println!("{:?}", normal_syntax);
    //     let normal_value = Some(normal_syntax)
    //         .and_then(|x| try_discard_lets(&x))
    //         .and_then(|x| try_numeral(&x));
    //     assert_eq!(Some(value), normal_value);
    // }
}
