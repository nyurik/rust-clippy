// run-rustfix

#![allow(clippy::eq_op)]
#![allow(clippy::format_in_format_args)]
#![allow(clippy::print_literal)]
#![allow(named_arguments_used_positionally)]
#![allow(unused_variables)]
#![warn(clippy::inline_format_args)]

fn tester(fn_arg: i32) {
    let local_value = 1;
    let local_value2 = 2.0;
    let local_opt: Option<i32> = Some(3);
    let width = 4;
    let prec = 5;
    let val = 6;

    // make sure this file hasn't been corrupted with tabs converted to spaces
    // TODO: ideally this should be a const compiler check, similar to
    // https://docs.rs/static_assertions/1.1.0/static_assertions/macro.const_assert_eq.html
    assert_eq!("	 	", "\t \t");

    println!("val='{}'", local_value);
    println!("val='{   }'", local_value);
    println!("val='{	}'", local_value);
    println!("val='{ 	}'", local_value);
    println!("val='{	 }'", local_value);
    println!(
        "val='{
    }'",
        local_value
    );
    println!("{}", local_value);
    println!("{}", fn_arg);
    println!("{:?}", local_value);
    println!("{:#?}", local_value);
    println!("{:4}", local_value);
    println!("{:04}", local_value);
    println!("{:<3}", local_value);
    println!("{:#010x}", local_value);
    println!("{:.1}", local_value2);
    println!("Hello {} is {:.*}", "x", local_value, local_value2);
    println!("{} {}", local_value, local_value2);
    println!("{}, {}", local_value, local_opt.unwrap());
    println!("{}", val);
    println!("{}", v = val);

    // TODO: these cases should be handled
    println!("val='{\t }'", local_value);
    println!("val='{\n }'", local_value);
    println!("val='{local_value}'", local_value = local_value);
    println!("val='{local_value}'", local_value = fn_arg);
    println!("{0}", local_value);
    println!("{0:?}", local_value);
    println!("{0:#?}", local_value);
    println!("{0:04}", local_value);
    println!("{0:<3}", local_value);
    println!("{0:#010x}", local_value);
    println!("{0:.1}", local_value2);
    println!("{0} {0}", local_value);
    println!("{1} {} {0} {}", local_value, local_value2);
    println!("{0} {1}", local_value, local_value2);
    println!("{1} {0}", local_value, local_value2);
    println!("{1} {0} {1} {0}", local_value, local_value2);
    println!("{1} {0}", "str", local_value);
    println!("{v}", v = local_value);
    println!("{local_value:0$}", width);
    println!("{local_value:w$}", w = width);
    println!("{local_value:.0$}", prec);
    println!("{local_value:.p$}", p = prec);
    println!("{:0$}", width);
    println!("{:0$}", v = val);
    println!("{0:0$}", v = val);
    println!("{:0$.0$}", v = val);
    println!("{0:0$.0$}", v = val);
    println!("{0:0$.v$}", v = val);
    println!("{0:v$.0$}", v = val);
    println!("{v:0$.0$}", v = val);
    println!("{v:v$.0$}", v = val);
    println!("{v:0$.v$}", v = val);
    println!("{v:v$.v$}", v = val);
    println!("{:w$}", w = width);
    println!("{:.0$}", prec);
    println!("{:.p$}", p = prec);
    println!("{:0$.1$}", width, prec);
    println!("{:0$.w$}", width, w = prec);
    println!("{:1$.2$}", local_value2, width, prec);
    println!("Width = {}, value with width = {:0$}", local_value, local_value2);
    println!("{:p$.w$}", local_value, w = width, p = prec);
    println!("{:p$.w$}", p = width, w = prec);
    println!("{}", format!("{}", local_value));

    // wouldn't be modified by the lint
    println!(concat!("nope ", "{}"), local_value);
    println!("val='{local_value}'");
    println!("val='{local_value }'");
    println!("val='{local_value	}'"); // with tab
    println!("val='{local_value\n}'");
    println!("{}", usize::MAX);
    println!("{}", local_opt.unwrap());
    println!(
        "val='{local_value
    }'"
    );
}

fn main() {
    tester(42);
}
