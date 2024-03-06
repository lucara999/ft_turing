/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   infinit_tape.rs                                    :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: laraujo <laraujo@student.42.fr>            +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2024/03/06 13:31:15 by laraujo           #+#    #+#             */
/*   Updated: 2024/03/06 15:13:16 by laraujo          ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

use colored::Colorize;

enum InfiniteTape {
	Left(Vec<char>),
	Cursor(char),
	Right(Vec<char>),
}

pub fn main() {
	// let input = "11+01=";
	let infinit_tape = vec![
		InfiniteTape::Left(vec!['.','.','.']),
		InfiniteTape::Cursor('.'),
		InfiniteTape::Right(vec!['.','.','.','.','.','.']),
	];

	infinit_tape.iter().for_each(|x| match x {
		InfiniteTape::Left(l) => l.iter().rev().for_each(|cell| print!("{}", cell)),
		InfiniteTape::Cursor(c) => print!("{}", c.to_string().red().bold().underline()),
		InfiniteTape::Right(r) => r.iter().for_each(|cell| print!("{}", cell)),
	})
}
