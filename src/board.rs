use std::collections::HashMap;

#[derive(PartialEq,Eq,Copy,Clone)]
enum Player {
    Red,
    Green,
    Blue,
}

#[derive(Copy,Clone,Eq,PartialEq)]
enum Direction {
    NE,
    EE,
    SE,
    SW,
    WW,
    NW,
}

#[derive(Copy,Clone,PartialEq)]
enum Step {
    SL,
    SF,
    SR,
}

#[derive(Copy,Clone,Eq,Hash,PartialEq)]
struct Hex {
    row: i8,
    diag: i8,
}

struct Board {
    pieces: HashMap<Hex,(Player,Direction)>,
    up: Player,
    remainder: u8,
}

impl Board {
    fn players(&self) -> Roster {
        Roster {
            remainder: self.remainder,
            start: self.up,
        }
    }

    fn moves(&self) -> impl Iterator<Item=Option<((Hex,Step),(Hex,Step))>> + '_ {
        use Step::*;
        self.pieces.iter()
            .filter(|(_,(p1,_))| *p1 == self.up)
            .flat_map(move |(h1,(_,d1))| Step::all().filter_map(move |s| match s {
                SF => {
                    let hd = h1.neighbour(*d1);
                    if hd.on_board() && match self.pieces.get(&hd) {
                        Some((_,dd)) => dd.flip() != *d1,
                        None => true,
                    } {
                        Some((h1,s))
                    } else {
                        None
                    }
                }
                _ => Some((h1,s)),
            })).flat_map(move |(h1,s1)| {
                let (hd,d1) = match self.pieces.get(h1) {
                    Some((_,d1)) => (h1.neighbour(*d1),*d1),
                    None => unreachable!(),
                };
                self.pieces.iter()
                    .filter(move |(h2,(p1,_))| *p1 == self.up && (s1 != SF || h1 != *h2))
                    .flat_map(move |(h2,(_,d2))| {
                        let (h2,d2) = if h2 == h1 {
                            (hd,d1)
                        } else {
                            (*h2,*d2)
                        };
                        Step::all().filter_map(move |s2| match s2 {
                            SF => {
                                let hd2 = h2.neighbour(d2);
                                if hd2.on_board() && (if hd2 == hd {
                                    d1.flip() != d2
                                } else {
                                    match self.pieces.get(&h2) {
                                        Some((_,dd)) => dd.flip() != d2,
                                        None => true,
                                    }
                                }) {
                                    Some(Some(((*h1,s1),(h2,s2))))
                                } else {
                                    None
                                }
                            }
                            _ => Some(Some(((*h1,s1),(h2,s2))))
                        })
                    })
            })
    }
}

/* (row, diagonal)
 *   (1,0)  (1,1)
 * (0,-1) (0,0) (0,1)
 *   (-1,-1)  (-1,0)
 */

impl Hex {
    fn neighbour(self, d: Direction) -> Self {
        use Direction::*;
        match d {
            NE => Self { row: self.row + 1, diag: self.diag + 1 },
            EE => Self { row: self.row, diag: self.diag + 1 },
            SE => Self { row: self.row - 1, diag: self.diag },
            SW => Self { row: self.row - 1, diag: self.diag - 1 },
            WW => Self { row: self.row, diag: self.diag - 1 },
            NW => Self { row: self.row + 1, diag: self.diag },
        }
    }

    fn on_board(self) -> bool {
        self.row >= -5 &&
            self.row <= 5 &&
            self.diag >= -5 &&
            self.diag <= 5 &&
            self.diag >= self.row - 5 &&
            self.diag <= self.row + 5
    }
}

impl Player {
    fn bitmask(self) -> u8 {
        match self {
            Self::Red => 1,
            Self::Green => 2,
            Self::Blue => 4,
        }
    }

    fn rotate(self) -> Self {
        match self {
            Self::Red => Self::Green,
            Self::Green => Self::Blue,
            Self::Blue => Self::Red,
        }
    }
}

impl Direction {
    fn turn(self, r: u8) -> Self {
        use Direction::*;
        let d = match self {
            NE => 0,
            EE => 1,
            SE => 2,
            SW => 3,
            WW => 4,
            NW => 5,
        };
        match (d + r) % 6 {
            0 => NE,
            1 => EE,
            2 => SE,
            3 => SW,
            4 => WW,
            5 => NW,
            _ => unreachable!(),
        }
    }

    fn flip(self) -> Self {
        self.turn(3)
    }
}

struct Roster {
    remainder: u8,
    start: Player,
}

impl Iterator for Roster {
    type Item = Player;
    fn next(&mut self) -> Option<Player> {
        if self.remainder == 0 {
            None
        } else {
            loop {
                let who = self.start;
                let mask = who.bitmask();
                self.start = who.rotate();
                if mask & self.remainder != 0 {
                    self.remainder &= !mask;
                    break Some(who);
                }
            }
        }
    }
}

impl Step {
    fn apply(self, p: Hex, d: Direction) -> (Hex, Direction) {
        use Step::*;
        match self {
            SL => (p, d.turn(5)),
            SF => (p.neighbour(d), d),
            SR => (p, d.turn(1)),
        }
    }

    fn all() -> impl Iterator<Item=Step> {
        struct I {
            i: u8,
        }
        impl Iterator for I {
            type Item = Step;
            fn next(&mut self) -> Option<Step> {
                use Step::*;
                match self.i {
                    0 => Some(SL),
                    1 => Some(SF),
                    2 => Some(SR),
                    _ => None,
                }.map(|a| { self.i += 1; a })
            }
        }
        I { i: 0 }
    }
}

