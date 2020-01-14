fn main() {
    let mut v = [1, 2, 3, 4];
    
    v.iter_mut()
    .filter(|&&mut x| x < 3)
    .map(|x| x.to_string())
    .for_each(|x| println!("{:?}", x));
    
    v.iter().for_each(|x| println!("{}", x));    
}
