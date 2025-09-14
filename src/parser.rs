use core::panic;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

fn collect_idents(expr: &Expression, acc: &mut HashSet<String>) {
    match expr {
        Expression::Word(w) => {
            acc.insert(w.clone());
        }
        Expression::Apply(exprs) => {
            for e in exprs {
                collect_idents(e, acc);
            }
        }
        _ => {}
    }
}

fn tree_shake(std_defs: Vec<Expression>, used: &HashSet<String>) -> Vec<Expression> {
    let mut index = HashMap::new();
    for expr in &std_defs {
        if let Expression::Apply(list) = expr {
            if let [Expression::Word(kw), Expression::Word(name), _rest @ ..] = &list[..] {
                if kw == "let" {
                    index.insert(name.clone(), expr.clone());
                }
            }
        }
    }

    let mut kept = Vec::new();
    let mut visited = HashSet::new();

    fn visit(
        name: &str,
        index: &HashMap<String, Expression>,
        kept: &mut Vec<Expression>,
        visited: &mut HashSet<String>,
    ) {
        if visited.contains(name) {
            return;
        }
        if let Some(def) = index.get(name) {
            if let Expression::Apply(list) = def {
                if list.len() >= 3 {
                    let mut deps = HashSet::new();
                    collect_idents(&list[2], &mut deps);
                    for dep in deps {
                        visit(&dep, index, kept, visited);
                    }
                }
            }
            kept.push(def.clone());
        }
        visited.insert(name.to_string());
    }

    for name in used {
        visit(name, &index, &mut kept, &mut visited);
    }

    kept
}

fn flush(buf: &mut String, out: &mut Vec<String>) {
    if !buf.is_empty() {
        out.push(std::mem::take(buf));
    }
}

fn tokenize(input: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut buf = String::new();
    let mut chars = input.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            ';' => {
                while let Some(nc) = chars.peek() {
                    if *nc == '\n' {
                        break;
                    }
                    chars.next();
                }
            }
            '(' | ')' => {
                flush(&mut buf, &mut out);
                out.push(ch.to_string());
            }
            c if c.is_whitespace() => {
                flush(&mut buf, &mut out);
            }
            _ => {
                buf.push(ch);
            }
        }
    }
    flush(&mut buf, &mut out);
    out
}

fn parse_expr(tokens: &[String], i: &mut usize) -> Result<Expression, String> {
    if *i >= tokens.len() {
        return Err("Unexpected end of input".into());
    }
    let tok = &tokens[*i];

    if tok == "(" {
        *i += 1;
        let mut exprs = Vec::new();
        while *i < tokens.len() && tokens[*i] != ")" {
            exprs.push(parse_expr(tokens, i)?);
        }
        if *i >= tokens.len() {
            return Err("Unclosed '('".into());
        }
        *i += 1;
        Ok(Expression::Apply(exprs))
    } else if tok == ")" {
        Err("Unexpected ')'".into())
    } else {
        *i += 1;
        if is_number(tok) {
            let n: i32 = tok
                .parse()
                .map_err(|e| format!("Bad number '{}': {}", tok, e))?;
            Ok(Expression::Atom(n))
        } else {
            Ok(Expression::Word(tok.clone()))
        }
    }
}
pub fn parse(src: &str) -> Result<Vec<Expression>, String> {
    let tokens = tokenize(src);
    let mut i = 0;
    let mut exprs = Vec::new();
    while i < tokens.len() {
        exprs.push(parse_expr(&tokens, &mut i)?);
    }
    Ok(exprs)
}
fn preprocess(source: &str) -> String {
    let mut out = String::new();
    let mut chars = source.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            '[' => {
                out.push('(');
                out.push_str("array ");
            }
            ']' => out.push(')'),
            '"' => {
                let mut s = String::new();
                while let Some(&next) = chars.peek() {
                    chars.next();
                    if next == '"' {
                        break;
                    } else {
                        s.push(next);
                    }
                }
                out.push_str("(array ");
                for (i, c) in s.chars().enumerate() {
                    if i > 0 {
                        out.push(' ');
                    }
                    out.push_str(&(c as u32).to_string());
                    out.push_str("");
                }
                out.push(')');
            }
            _ => out.push(ch),
        }
    }

    out
}
fn desugar(expr: Expression) -> Expression {
    match expr {
        Expression::Apply(exprs) if !exprs.is_empty() => {
            let exprs: Vec<Expression> = exprs.into_iter().map(desugar).collect::<Vec<_>>();

            if let Expression::Word(ref name) = exprs[0] {
                match name.as_str() {
                    "|>" => pipe_transform(exprs),
                    "cond" => cond_transform(exprs),
                    "if" => if_transform(exprs),
                    "unless" => unless_transform(exprs),
                    "-" => minus_transform(exprs),
                    "+" => plus_transform(exprs),
                    "*" => mult_transform(exprs),
                    "/" => div_transform(exprs),
                    "and" => and_transform(exprs),
                    "or" => or_transform(exprs),
                    "!=" => not_equal_transform(exprs),
                    "<>" => not_equal_transform(exprs),
                    "." => accessor_transform(exprs),
                    "get" => accessor_transform(exprs),
                    "variable" => variable_transform(exprs),
                    "integer" => integer_transform(exprs),
                    "boolean" => boolean_transform(exprs),
                    "loop" => loop_transform(exprs),
                    _ => Expression::Apply(exprs),
                }
            } else {
                Expression::Apply(exprs)
            }
        }
        other => other,
    }
}
fn loop_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);
    let len = exprs.len();
    if len == 2 {
        return Expression::Apply(vec![
            Expression::Word("loop-finish".to_string()),
            exprs[0].clone(),
            exprs[1].clone(),
        ]);
    } else {
        return Expression::Apply(vec![
            Expression::Word("loop".to_string()),
            exprs[0].clone(),
            exprs[1].clone(),
            exprs[2].clone(),
        ]);
    }
}
fn accessor_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);
    let len = exprs.len();
    let mut iter = exprs.into_iter();
    let first = iter.next().unwrap();

    if len == 1 {
        return Expression::Apply(vec![
            Expression::Word("get".to_string()),
            first,
            Expression::Atom(0),
        ]);
    }

    let mut acc = first;

    for e in iter {
        acc = Expression::Apply(vec![Expression::Word("get".to_string()), acc, e]);
    }

    acc
}
fn variable_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);
    Expression::Apply(vec![
        Expression::Word("let".to_string()),
        exprs[0].clone(),
        Expression::Apply(vec![Expression::Word("box".to_string()), exprs[1].clone()]),
    ])
}
fn integer_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);
    Expression::Apply(vec![
        Expression::Word("let".to_string()),
        exprs[0].clone(),
        Expression::Apply(vec![Expression::Word("int".to_string()), exprs[1].clone()]),
    ])
}
fn boolean_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);
    match &exprs[1] {
        Expression::Word(x) => {
            if x != "true" && x != "false" {
                panic!(
                    "Booleans variables only be assigned to true or false but got: {}",
                    x
                )
            }
        }
        Expression::Apply(x) => match &x[0] {
            Expression::Word(y) => {
                if y != "="
                    && y != ">"
                    && y != "<"
                    && y != "<="
                    && y != ">="
                    && y != "not"
                    && y != "or"
                    && y != "and"
                {
                    panic!(
                        "Booleans variables only be assigned to results of boolean expressions but got: {}",
                        y
                    )
                }
            }
            _ => panic!(
                "Booleans variables only be assigned to true or false but got: {:?}",
                x[0]
            ),
        },
        x => panic!(
            "Booleans variables only be assigned to true or false but got : {:?}",
            x
        ),
    }
    Expression::Apply(vec![
        Expression::Word("let".to_string()),
        exprs[0].clone(),
        Expression::Apply(vec![
            Expression::Word("array".to_string()),
            exprs[1].clone(),
        ]),
    ])
}
fn not_equal_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);
    Expression::Apply(vec![
        Expression::Word("not".to_string()),
        Expression::Apply(vec![
            Expression::Word("=".to_string()),
            exprs[0].clone(),
            exprs[1].clone(),
        ]),
    ])
}
fn minus_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);

    match exprs.len() {
        0 => Expression::Atom(0),
        1 => Expression::Apply(vec![
            Expression::Word("*".to_string()),
            exprs.remove(0),
            Expression::Atom(-1),
        ]),
        _ => {
            let first = exprs.remove(0);
            exprs.into_iter().fold(first, |acc, next| {
                Expression::Apply(vec![Expression::Word("-".to_string()), acc, next])
            })
        }
    }
}
fn plus_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);

    match exprs.len() {
        0 => Expression::Atom(1),
        _ => {
            let first = exprs.remove(0);
            exprs.into_iter().fold(first, |acc, next| {
                Expression::Apply(vec![Expression::Word("+".to_string()), acc, next])
            })
        }
    }
}

fn and_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);

    match exprs.len() {
        0 => Expression::Atom(1),
        _ => {
            let first = exprs.remove(0);
            exprs.into_iter().fold(first, |acc, next| {
                Expression::Apply(vec![Expression::Word("and".to_string()), acc, next])
            })
        }
    }
}

fn or_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);

    match exprs.len() {
        0 => Expression::Atom(1),
        _ => {
            let first = exprs.remove(0);
            exprs.into_iter().fold(first, |acc, next| {
                Expression::Apply(vec![Expression::Word("or".to_string()), acc, next])
            })
        }
    }
}
fn mult_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);

    match exprs.len() {
        0 => Expression::Atom(1),
        _ => {
            let first = exprs.remove(0);
            exprs.into_iter().fold(first, |acc, next| {
                Expression::Apply(vec![Expression::Word("*".to_string()), acc, next])
            })
        }
    }
}
fn div_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);

    match exprs.len() {
        0 => Expression::Atom(1),
        _ => {
            let first = exprs.remove(0);
            exprs.into_iter().fold(first, |acc, next| {
                Expression::Apply(vec![Expression::Word("/".to_string()), acc, next])
            })
        }
    }
}
fn cond_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);

    if exprs.is_empty() {
        return Expression::Atom(0);
    }

    let mut pairs = Vec::new();
    let mut default = Expression::Atom(0);

    if exprs.len() % 2 == 1 {
        default = exprs.pop().unwrap();
    }

    for chunk in exprs.chunks(2) {
        let test = chunk[0].clone();
        let branch = chunk[1].clone();
        pairs.push((test, branch));
    }

    let mut result = default;
    for (test, branch) in pairs.into_iter().rev() {
        result = Expression::Apply(vec![
            Expression::Word("if".to_string()),
            test,
            branch,
            result,
        ]);
    }

    result
}
fn if_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);

    return Expression::Apply(vec![
        Expression::Word("if".to_string()),
        exprs[0].clone(),
        exprs[1].clone(),
        if exprs.len() == 2 {
            Expression::Atom(0)
        } else {
            exprs[2].clone()
        },
    ]);
}
fn unless_transform(mut exprs: Vec<Expression>) -> Expression {
    exprs.remove(0);

    return Expression::Apply(vec![
        Expression::Word("if".to_string()),
        exprs[0].clone(),
        if exprs.len() == 2 {
            Expression::Atom(0)
        } else {
            exprs[2].clone()
        },
        exprs[1].clone(),
    ]);
}
fn pipe_transform(mut exprs: Vec<Expression>) -> Expression {
    let mut inp = exprs.remove(1);

    for stage in exprs.into_iter().skip(1) {
        if let Expression::Apply(mut inner) = stage {
            if inner.is_empty() {
                continue;
            }
            let func = inner.remove(0);
            let mut new_stage = vec![func, inp];
            new_stage.extend(inner);
            inp = Expression::Apply(new_stage);
        } else {
            inp = Expression::Apply(vec![stage, inp]);
        }
    }

    inp
}
fn is_number(s: &str) -> bool {
    if s == "-" || s == "+ " || s == "+" {
        return false;
    }
    if !s.chars().any(|c| c.is_ascii_digit()) {
        return false;
    }
    let trimmed = if let Some(stripped) = s.strip_prefix('-') {
        stripped
    } else {
        s
    };
    for c in trimmed.chars() {
        if !c.is_ascii_digit() {
            return false;
        }
    }
    true
}
macro_rules! s {
    ($s:expr) => {
        $s.to_string()
    };
}
#[derive(Debug, Clone)]
pub enum Expression {
    Atom(i32),
    Word(String),
    Apply(Vec<Expression>),
}

impl Expression {
    pub fn to_rust(&self) -> String {
        match self {
            Expression::Atom(n) => format!("Atom({})", n),
            Expression::Word(w) => format!("Word(s!({:?}))", w),
            Expression::Apply(exprs) => {
                let inner: Vec<String> = exprs.iter().map(|e| e.to_rust()).collect();
                format!("Apply(vec![{}])", inner.join(", "))
            }
        }
    }
}
#[allow(dead_code)]
pub fn with_std(program: &str, std: &str) -> Expression {
    let preprocessed = preprocess(&program);

    let exprs = parse(&preprocessed).unwrap();
    let desugared: Vec<Expression> = exprs
        .into_iter()
        .map(desugar_tail_recursion)
        .map(desugar)
        .collect();
    let preprocessed_std = preprocess(&std);
    let exprs_std = parse(&preprocessed_std).unwrap();
    let desugared_std: Vec<Expression> = exprs_std
        .into_iter()
        // .map(desugar_tail_recursion)
        .map(desugar)
        .collect();
    let mut used = HashSet::new();
    for e in &desugared {
        collect_idents(e, &mut used);
    }

    let shaken_std = tree_shake(desugared_std, &used);

    let wrapped = Expression::Apply(
        std::iter::once(Expression::Word("do".to_string()))
            .chain(shaken_std.into_iter())
            .chain(desugared.into_iter())
            .collect(),
    );
    wrapped
}

// Main entry: recursively transform expressions, but when a let with a lambda bound to a name
// is encountered, run the "lambda-to-loop" transform on it if it's tail-recursive.
fn desugar_tail_recursion(expr: Expression) -> Expression {
    match expr {
        Expression::Apply(mut items) if !items.is_empty() => {
            // desugar children first
            let head = items.remove(0);
            let head = desugar_tail_recursion(head);
            let rest = items
                .into_iter()
                .map(desugar_tail_recursion)
                .collect::<Vec<_>>();

            // Reconstruct and check for (let name (lambda ...))
            if let Expression::Word(ref w) = head {
                if w == "let" && rest.len() == 2 {
                    // rest[0] is var name, rest[1] is value
                    if let Expression::Word(var_name) = &rest[0] {
                        // If value is a lambda, consider converting
                        if let Expression::Apply(lambda_items) = &rest[1] {
                            if let Some(Expression::Word(lambda_kw)) = lambda_items.get(0) {
                                if lambda_kw == "lambda" {
                                    // Build original lambda expression (with already-desugared inner content)
                                    let lambda_expr = Expression::Apply({
                                        let mut v = vec![Expression::Word("lambda".to_string())];
                                        v.extend(
                                            lambda_items[1..]
                                                .iter()
                                                .cloned()
                                                .map(desugar_tail_recursion),
                                        );
                                        v
                                    });
                                    // Try to transform that lambda into loop form:
                                    return Expression::Apply(vec![
                                        Expression::Word("let".to_string()),
                                        Expression::Word(var_name.clone()),
                                        transform_named_lambda_to_loop(
                                            var_name.clone(),
                                            lambda_expr,
                                        ),
                                    ]);
                                }
                            }
                        }
                    }
                }
            }

            // default: just reassemble
            let mut v = vec![head];
            v.extend(rest);
            Expression::Apply(v)
        }

        // recursively descend
        Expression::Word(_) | Expression::Atom(_) => expr,
        other => other,
    }
}

/// Transform a `lambda` expression **bound to name `fn_name`** into a loop-driven lambda
/// if it contains tail-recursive calls. If no tail recursion is found, return the original lambda.
///
/// Input lambda shape: (lambda p1 p2 ... body)
fn transform_named_lambda_to_loop(fn_name: String, lambda_expr: Expression) -> Expression {
    // Extract params and body from lambda_expr
    let items = match lambda_expr {
        Expression::Apply(v) => v,
        _ => return lambda_expr, // not a lambda (shouldn't happen)
    };

    // items[0] == "lambda"
    let params_and_body = &items[1..];
    if params_and_body.is_empty() {
        // zero-arg lambda -> nothing to do
        return Expression::Apply(items);
    }

    // last element is body, the earlier ones are param words (or special parameter forms)
    let param_exprs = params_and_body[..params_and_body.len() - 1].to_vec();
    let body_expr = params_and_body[params_and_body.len() - 1].clone();

    // Collect parameter names
    let mut params = Vec::new();

    for p in &param_exprs {
        if let Expression::Word(s) = p {
            params.push(s.clone());
        } else {
            // if param is not a simple word, bail out (keep original)
            return Expression::Apply(items);
        }
    }

    // STEP 1: detect whether lambda body contains any tail calls to fn_name
    let has_tail = contains_tail_call_to(&body_expr, &fn_name);
    if !has_tail {
        // nothing to do — but we still want inner body desugared (done earlier), so return original
        return Expression::Apply(items);
    }

    // STEP 2: build transformed body
    // We'll create internal names:
    // __rec_cont  -- 1 while should continue, 0 when finished
    // __rec_result -- holds final result
    // We'll also make params "mutable single-element arrays" by wrapping initial param value into [param_val]
    // That means every occurrence of param in body must be replaced by (get param 0)
    //
    // Construct sequence:
    //
    // (lambda ORIGINAL_PARAMS
    //   (do
    //     (let __rec_cont 1)
    //     (let __rec_result 0)
    //     ;; rebind original params to single-element arrays holding their initial values:
    //     (let p1 [p1]) (let p2 [p2]) ...
    //
    //     (loop-finish __rec_cont
    //       (lambda
    //         ;; body where:
    //         ;; - tail call (fn args...) -> (do (set! p1 0 arg1) ... ) ;; then return
    //         ;; - tail-return V -> (do (set! __rec_result 0 V) (set! __rec_cont 0) )
    //         ;; non-tail occurrences of param replaced with (get p 0)
    //       ))
    //
    //     (get __rec_result 0) ;; return final result stored in the result cell
    //   )
    // )

    // Create let-bindings for cont/result
    let cont_name = "__rec_cont".to_string();
    let result_name = "__rec_result".to_string();

    // Replace param occurrences in body by (get param 0) — we will do this during the transform pass,
    // because some occurrences are in contexts (e.g. being assigned) and we only want read accesses replaced.
    //
    // Make the mutable param initializations:
    // for each param `p` we will emit: (let p [ p ])
    let mut param_inits = Vec::new();
    for p in &params {
        let init = Expression::Apply(vec![
            Expression::Word("let".to_string()),
            Expression::Word("_".to_string() + p),
            Expression::Apply(vec![
                Expression::Word("array".to_string()), // array literal constructor
                Expression::Word(p.clone()),
            ]),
        ]);
        let temp = Expression::Apply(vec![
            Expression::Word("let".to_string()),
            Expression::Word("_new_".to_string() + p),
            Expression::Apply(vec![
                Expression::Word("array".to_string()), // array literal constructor
            ]),
        ]);
        param_inits.push(init);
        param_inits.push(temp);
    }

    // Build the loop body by transforming tail positions:
    let loop_function_body =
        transform_tail_positions(&body_expr, &fn_name, &params, &cont_name, &result_name);

    // The function to call by loop-finish must be a 0-arg lambda with the body above:
    let loop_fn_lambda = Expression::Apply(vec![
        Expression::Word("lambda".to_string()),
        // zero params
        loop_function_body,
    ]);

    // Construct the loop-finish call: (loop-finish __rec_cont (lambda ...))
    let loop_finish_call = Expression::Apply(vec![
        Expression::Word("loop-finish".to_string()),
        Expression::Apply(vec![
            Expression::Word("=".to_string()),
            Expression::Apply(vec![
                Expression::Word("get".to_string()),
                Expression::Word(cont_name.clone()),
                Expression::Atom(0),
            ]),
            Expression::Atom(0),
        ]),
        loop_fn_lambda,
    ]);

    // After loop finishes, return the actual stored result: since we put final result into __rec_result,
    // which we created as an int, but to be consistent with our single-element approach we stored values inside arrays
    // earlier — to be safe, we will store result into a single-element array as well and fetch via (get __rec_result 0).
    // For simplicity here: we made __rec_result a boxed single element array initialised to [0] instead of 0.
    //

    let let_cont_arr = Expression::Apply(vec![
        Expression::Word("let".to_string()),
        Expression::Word(cont_name.clone()),
        Expression::Apply(vec![
            Expression::Word("array".to_string()),
            Expression::Atom(0),
        ]),
    ]);
    // NOTE: Earlier we made __rec_result as Atom(0) — but that isn't an array. Let's change: create __rec_result as [0]
    let let_result_arr = Expression::Apply(vec![
        Expression::Word("let".to_string()),
        Expression::Word(result_name.clone()),
        Expression::Apply(vec![Expression::Word("array".to_string())]),
    ]);

    // final return: (get __rec_result 0)
    let final_get_result = Expression::Apply(vec![
        Expression::Word("get".to_string()),
        Expression::Word(result_name.clone()),
        Expression::Atom(0),
    ]);

    // Assemble the whole `do` body in order:
    let mut do_items = vec![];
    do_items.push(let_cont_arr);
    do_items.push(let_result_arr); // replaced earlier simple let with array version
    do_items.extend(param_inits);
    do_items.push(loop_finish_call);
    // do_items.push(final_get_result);

    let wrapped = Expression::Apply({
        let mut r = vec![Expression::Word("do".to_string())];
        r.extend(do_items);
        r.extend(vec![final_get_result]);
        r
    });

    // Build the outer lambda with original param list but wrapped body
    let mut lambda_vec = vec![Expression::Word("lambda".to_string())];
    lambda_vec.extend(param_exprs.clone()); // keep original parameter names as they are
    lambda_vec.push(wrapped);

    Expression::Apply(lambda_vec)
}

/// Detect whether there is a *tail call* to `fn_name` inside `expr` (tail position inside expression).
/// We conservatively check "is there any call to fn_name that appears in tail position" — i.e., the final
/// expression of the body or the final branch of conditionals, etc.
fn contains_tail_call_to(expr: &Expression, fn_name: &str) -> bool {
    match expr {
        Expression::Apply(items) if !items.is_empty() => {
            if let Expression::Word(op) = &items[0] {
                if op == fn_name {
                    // a direct call in this position => tail call (since we're asking about this spot)
                    return true;
                }
                // special control forms where the tail position is specific:
                match op.as_str() {
                    "if" | "unless" => {
                        // (if cond then else) -> check then and else tails
                        if items.len() >= 3 {
                            return contains_tail_call_to(&items[2], fn_name)
                                || (items.len() > 3 && contains_tail_call_to(&items[3], fn_name));
                        }
                        false
                    }
                    "do" => {
                        // tail is the last expr in do
                        if items.len() >= 2 {
                            return contains_tail_call_to(&items[items.len() - 1], fn_name);
                        }
                        false
                    }
                    _ => false,
                }
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Transform expressions recursively, rewriting only *tail-position* subexpressions:
/// - tail call `(fn_name arg1 arg2 ...)` -> `(do (set! p1 0 arg1) ... )`  (then returns, ending lambda iteration)
/// - tail return `V` -> `(do (set! __rec_result 0 V) (set! __rec_cont 0))`
/// - other positions are transformed by replacing param occurrences with `(get param 0)` reads
fn transform_tail_positions(
    expr: &Expression,
    fn_name: &str,
    params: &[String],
    cont_name: &str,
    result_name: &str,
) -> Expression {
    // helper to replace param reads by (get p 0) everywhere except when param appears as the target of `set!`
    fn replace_param_reads(e: &Expression, params: &[String]) -> Expression {
        match e {
            Expression::Word(w) if params.contains(w) => Expression::Apply(vec![
                Expression::Word("get".to_string()),
                Expression::Word("_".to_string() + w),
                Expression::Atom(0),
            ]),
            Expression::Apply(items) => {
                let mapped = items
                    .iter()
                    .map(|it| replace_param_reads(it, params))
                    .collect();
                Expression::Apply(mapped)
            }
            other => other.clone(),
        }
    }

    // For tail position:
    match expr {
        Expression::Apply(items) if !items.is_empty() => {
            if let Expression::Word(op) = &items[0] {
                // If this exact application is a recursive call in tail position:
                if op == fn_name {
                    // generate (do (set! p1 0 arg1) (set! p2 0 arg2) ... )
                    let mut sets = vec![Expression::Word("do".to_string())];
                    for (i, p) in params.iter().enumerate() {
                        let arg = items.get(1 + i).cloned().unwrap_or(Expression::Atom(0));
                        let arg_replaced = replace_param_reads(&arg, params);
                        let set_new_call = Expression::Apply(vec![
                            Expression::Word("set!".to_string()),
                            Expression::Word("_new_".to_string() + p),
                            Expression::Atom(0),
                            arg_replaced,
                        ]);
                        sets.push(set_new_call);
                    }
                    for (i, p) in params.iter().enumerate() {
                        let arg = items.get(1 + i).cloned().unwrap_or(Expression::Atom(0));
                        let arg_replaced = replace_param_reads(&arg, params);
                        let set_call = Expression::Apply(vec![
                            Expression::Word("set!".to_string()),
                            Expression::Word("_".to_string() + p),
                            Expression::Atom(0),
                            Expression::Apply(vec![
                                Expression::Word("get".to_string()),
                                Expression::Word("_new_".to_string() + p),
                                Expression::Atom(0),
                            ]),
                        ]);
                        sets.push(set_call);
                    }
                    // after setting params we simply return (function iteration ends)
                    return Expression::Apply(sets);
                }
                // If it's a control form with branches where tail positions exist, we need special handling:
                match op.as_str() {
                    "if" => {
                        // (if cond then else) -> cond is non-tail; then/else are tail
                        if items.len() < 3 {
                            return replace_param_reads(expr, params);
                        }
                        let cond = replace_param_reads(&items[1], params);
                        let then_branch = transform_tail_positions(
                            &items[2],
                            fn_name,
                            params,
                            cont_name,
                            result_name,
                        );
                        let else_branch = if items.len() > 3 {
                            transform_tail_positions(
                                &items[3],
                                fn_name,
                                params,
                                cont_name,
                                result_name,
                            )
                        } else {
                            // missing else -> keep as is
                            Expression::Atom(0)
                        };
                        return Expression::Apply(vec![
                            Expression::Word("if".to_string()),
                            cond,
                            then_branch,
                            else_branch,
                        ]);
                    }
                    "do" => {
                        // all but last are non-tail; last is tail
                        let mut new_items = vec![Expression::Word("do".to_string())];
                        if items.len() >= 2 {
                            for it in &items[1..items.len() - 1] {
                                new_items.push(replace_param_reads(it, params));
                            }
                            let last = transform_tail_positions(
                                &items[items.len() - 1],
                                fn_name,
                                params,
                                cont_name,
                                result_name,
                            );
                            new_items.push(last);
                        }
                        return Expression::Apply(new_items);
                    }
                    _ => {
                        // Generic non-tail application: treat as expr returning value in tail position:
                        // e.g. last-expr is `(foo a b)` but foo != fn_name -> then presence of this expression in tail position means we must convert it into setting result & cont=0
                        // Build:
                        // (do (let __tmp <expr>) (set! __rec_result 0 __tmp) (set! __rec_cont 0))
                        // but to avoid extra let we can write: (do (set! __rec_result 0 <expr_replaced>) (set! __rec_cont 0))
                        let expr_replaced = replace_param_reads(expr, params);
                        return Expression::Apply(vec![
                            Expression::Word("do".to_string()),
                            Expression::Apply(vec![
                                Expression::Word("set!".to_string()),
                                Expression::Word(result_name.to_string()),
                                Expression::Atom(0),
                                expr_replaced,
                            ]),
                            Expression::Apply(vec![
                                Expression::Word("set!".to_string()),
                                Expression::Word(cont_name.to_string()),
                                Expression::Atom(0),
                                Expression::Atom(1),
                            ]),
                        ]);
                    }
                }
            } else {
                // If head isn't a word, replace param reads inside, then treat as terminal expression
                let expr_replaced = replace_param_reads(expr, params);
                return Expression::Apply(vec![
                    Expression::Word("do".to_string()),
                    Expression::Apply(vec![
                        Expression::Word("set!".to_string()),
                        Expression::Word(result_name.to_string()),
                        Expression::Atom(0),
                        expr_replaced,
                    ]),
                    Expression::Apply(vec![
                        Expression::Word("set!".to_string()),
                        Expression::Word(cont_name.to_string()),
                        Expression::Atom(0),
                        Expression::Atom(1),
                    ]),
                ]);
            }
        }

        // Atom or Word in tail position -> set result and clear cont
        other => {
            let replaced = replace_param_reads(other, params);
            Expression::Apply(vec![
                Expression::Word("do".to_string()),
                Expression::Apply(vec![
                    Expression::Word("set!".to_string()),
                    Expression::Word(result_name.to_string()),
                    Expression::Atom(0),
                    replaced,
                ]),
                Expression::Apply(vec![
                    Expression::Word("set!".to_string()),
                    Expression::Word(cont_name.to_string()),
                    Expression::Atom(0),
                    Expression::Atom(1),
                ]),
            ])
        }
    }
}
