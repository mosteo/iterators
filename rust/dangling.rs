fn dangling() -> impl std::iter::Iterator<Item=&'static i32> {
    let mut v : Vec<i32> = std::vec::Vec::new();
    v.push(1);
    return v.iter();
}

fn main() {
    dangling().for_each(|x| println!("{:?}", x));
}
