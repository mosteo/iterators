fn main() {
    let mut v = [1, 2, 3, 4];
    
    v.iter_mut().filter(|&&mut x| x < 3).for_each(|x| *x*=2);
    
    v.iter().for_each(|x| println!("{}", x));    
}
