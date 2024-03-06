// This program use Fonctional programming Only

// enum InfiniteTape {
//     let left: []
// }

// fn main() {
//     let x: i32 = 5;
//     let y: i32 = 6;
//     let result: i32 = add(x, y);
//     println!("{} + {} = {}", x, y, result);
//     let array: [i32; 5] = [1, 2, 3, 4, 5];
//     // let tuple = (1,2,3,4,5);
//     print_recursive(&array);

//     let exemple = |x: i32| x * x;
//     println!("Le carré de 5 est {}", exemple(5));

//     let vec = vec![1, 2, 3, 4, 5];
//     let iter = vec.iter();

//     // iter.map(|&x| x * 2).collect();
//     iter.rev().for_each(|&x| println!("{}", x));
//     // iter.for_each(|&x| print!("{}", x)); // Affiche chaque élément.

// }

// fn add(x: i32, y: i32) -> i32 {
//     x + y
// }

// fn print_recursive(array: &[i32]) {
//     if array.len() > 0 {
//         print!("{}", array[0]);
//         print_recursive(&array[1..]);
//     }
//     else {
//         println!();
//     }
// }
// use crate::infinit_tape::colored::Colorize;


pub mod infinit_tape;

pub fn main() {
    infinit_tape::main();
}
