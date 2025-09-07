use crate::Span;

#[test]
fn length_test() {
    // a
    let span = Span::new(0, 1);

    println!("{:?}", span);

    assert_eq!(span.len(), 1);
}